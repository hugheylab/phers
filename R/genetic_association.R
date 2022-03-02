#' Perform genotype association tests
#'
#' Perform association tests between the phenotype risk scores and genotypes
#'
#' @param scores A data.table of phenotype risk scores. Must have columns
#'   `person_id`, `disease_id`, `score`.
#' @param genotypes A data.table of genotypes. Must have columns `person_id`,
#'   and variant IDs to be tested. each variant ID column should contain the
#'   number of copies of the minor allele each person has (additive model).
#' @param demos A data.table of covariates to be used in the association
#'   analysis. Must have column `person_id`.
#' @param diseaseGeneVarMap A data.table that maps diseases to genes and
#'   variants. Must have columns `disease_id`, `gene`, and `vid`.
#'   `disease_id` must include all disease IDs in `scores`.
#' @param formStr A string representing the linear model of covariates to be
#'   used in genetic association tests. All covariates must be existing columns
#'   in `demos`. By default uses a simple linear model of all covariates
#'   in demos.
#' @param coding Coding of the genotypes. Either 'additive' or 'genotypic'.
#' @param doPar Logical indicating whether to run calculations in parallel if
#'   a parallel backend is already set up, e.g., using
#'   [doParallel::registerDoParallel()]. Recommended to minimize runtime.
#'
#' @return A data.table containing to results of the genotype association tests
#'   with the following columns. If the model fails to converge,
#'   NAs will be reported.
#'   \item{\code{disease_id}}{disease identifier}
#'   \item{\code{gene}}{name of the gene}
#'   \item{\code{vid}}{variant identifier}
#'   \item{\code{beta}}{The beta coefficient for the variant}
#'   \item{\code{se}}{The standard error for the beta coefficient}
#'   \item{\code{p}}{The p-value for the variant}
#'   \item{\code{lower}}{The lower bound of the `confint` confidence interval}
#'   \item{\code{upper}}{The upper bound of the `confint` confidence interval}
#'
#' @export
genotypeAssociation = function(
  scores, genotypes, demos, diseaseGeneVarMap, formStr = '~ .',
  coding = 'additive', doPar = FALSE) {
  diseaseID = disease_id = gene = snp = n_het = n_hom = n_total =
    n_wild = variant = count = diseaseGene = NULL

  checkScores(scores)
  checkGenotypes(genotypes)
  checkDemos(demos)
  checkDiseaseGeneVarMap(diseaseGeneVarMap, scores)
  assertFormulaString(formStr, demos)
  assertCoding(coding)
  assertFlag(doPar)

  lmInput = merge(scores, demos, by = 'person_id')

  # remove snps with only one genotype in the population
  genoCount = melt(
    genotypes[, lapply(.SD, uniqueN), .SDcols=colnames(genotypes)],
    id.vars = 'person_id', variable.name = 'snp', value.name = 'count')
  okSnps = genoCount[count != 1]$snp


  reg = foreach::getDoParRegistered()
  doOp = if(doPar && reg) `%dopar%` else `%do%`
  foe = foreach(diseaseID = unique(scores$disease_id), .combine = rbind)

  statsAll = doOp(foe, {
    diseaseGenes = unique(diseaseGeneVarMap[disease_id == diseaseID]$gene)
    lmInputSub = lmInput[disease_id == diseaseID][, !c('disease_id')]

    statsGenes = foreach(
      diseaseGene = diseaseGenes, .combine = rbind) %do% {

        snpSub = unique(diseaseGeneVarMap[gene == diseaseGene]$vid)
        # making sure the snps we're looping through are in genotypes
        # and are not all 0
        snpSub = snpSub[snpSub %in% okSnps]

        statsSnps = foreach(snp=snpSub, .combine = rbind) %do% {

          genoCols = c('person_id', snp)
          genotypesSub = genotypes[, .SD, .SDcols = genoCols]

          lmInputSub1 = merge(lmInputSub, genotypesSub, by = 'person_id')
          lmInputSub1 = lmInputSub1[, !c('person_id')]
          setnames(lmInputSub1, snp, 'variant')

          stat = data.table(
            disease_id = diseaseID, gene = diseaseGene, vid = snp)

          stat[, n_total := nrow(lmInputSub1)]
          stat[, n_wild := nrow(lmInputSub1[variant == 0])]
          stat[, n_het := nrow(lmInputSub1[variant == 1])]
          stat[, n_hom := nrow(lmInputSub1[variant == 2])]

          glmStat = runLinear(lmInputSub1, formStr, coding = coding)
          stat = cbind(stat, glmStat)
        }
      }
  })

return(statsAll)}



#' Perform a single genotype association test
#'
#' Perform an association test between one variant and the phenotype risk scores
#' corresponding to one disease.
#'
#' @param lmInput A data.table of of the input to the genetic association model.
#'   Must have columns `score`, `variant` and any other covariates to be used
#'   in the linear regression model. `variant` should contain the number of
#'   copies of the minor allele each person has (additive model).
#' @param formStr a string representing the linear model of covariates to be
#'   used in the genetic association test. All covariates must be existing
#'   columns in `lmInput`. By default uses a simple linear model of all
#'   columns other than `score` and `variant` in `lmInput`.
#' @param coding coding of the genotypes. Either 'additive' or 'genotypic'
#'
#' @return A data.table containing to results of the genotype association test
#'   with the following columns. If the model fails to converge,
#'   NAs will be reported.
#'   \item{\code{beta}}{The beta coefficient for the variant}
#'   \item{\code{se}}{The standard error for the beta coefficient}
#'   \item{\code{p}}{The p-value for the variant}
#'   \item{\code{lower}}{The lower bound of the `confint` confidence interval}
#'   \item{\code{upper}}{The upper bound of the `confint` confidence interval}
#'
#' @export
runLinear = function(lmInput, formStr = '~ .', coding = 'additive') {
  lower = melt = p = se = upper = variant = NULL

  checkLmInput(lmInput)
  assertFormulaString(formStr, lmInput)
  assertCoding(coding)

  formStr = paste0('score ~ variant', '+', trimws(sub('.*~', '', formStr)))

  if(coding == 'additive'){
    fit = glm(formStr, data = lmInput)
    varNames = c('variant')
  }
  if(coding == 'genotypic'){
    lmInput[, variant := as.factor(variant)]
    fit = glm(formStr, data = lmInput)
    varNames = c('variant1', 'variant2')
  }

  glmStat = data.table()
  for (varName in varNames) {
    glmStat = glmStat[, c('beta', 'se', 'p', 'lower', 'upper') := NA]
    if (!is.na(fit$coefficients[varName])) {
      fitSnpCoefs = summary(fit)$coefficients[varName,]
      glmStat[, beta := fitSnpCoefs['Estimate']]
      glmStat[, se := fitSnpCoefs['Std. Error']]
      glmStat[, p := fitSnpCoefs['Pr(>|t|)']]
      ci =  suppressMessages(confint(fit))
      glmStat[, lower := ci[varName, '2.5 %']]
      glmStat[, upper := ci[varName, '97.5 %']]
    }

    c0 = c('beta', 'se', 'p', 'lower', 'upper')
    if(varName == 'variant1'){
      setnames(glmStat, c0, paste0(c0, '_het'))
    }
    if(varName == 'variant2'){
      setnames(glmStat, c0, paste0(c0, '_hom'))
    }
  }
return(glmStat)}
