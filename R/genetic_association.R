#' Perform genotype association tests
#'
#' Perform association tests between the phenotype risk scores and genotypes
#'
#' @param scores A data.table of phenotype risk scores. The columns are
#'   `person_id`, `disease_id`, `score`.
#' @param genotypes A data.table of genotypes. The columns are `person_id`, and
#'   variant IDs to be tested.
#' @param covariates A data.table of covariates to be used in the association
#'   analysis.
#' @param formStr a formula object representing the linear model for
#'   genetic associations. must have 'score' as the dependent variable
#'   and 'variant' as the independent variable. All other independent variables
#'   must be existing columns in `covariates`.
#' @param coding coding of the genotypes. Either 'additive' or 'genotypic'
#'
#' @return A data.table containing to results of the genotype association tests
#'   with the following columns. If the model fails to converge,
#'   NAs will be reported.
#'   \item{\code{disease_id}}{disease identifier}
#'   \item{\code{vid}}{variant identifier}
#'   \item{\code{beta}}{The beta coefficient for the variant}
#'   \item{\code{se}}{The standard error for the beta coefficient}
#'   \item{\code{p}}{The p-value for the variant}
#'   \item{\code{lower}}{The lower bound of the `confint` confidence interval}
#'   \item{\code{upper}}{The upper bound of the `confint` confidence interval}
#'
#' @export
genotypeAssociation = function(
  scores, genotypes, covariates, formStr = as.formula("score ~ ."),
  coding = 'additive') {
  diseaseID = disease_id = gene = ..genoCols = snp = n_het = n_hom = n_total = n_wild = variant = count = diseaseGene = NULL

  lmInput = merge(scores, covariates, by = 'person_id')

  # remove snps with only one genotype in the population
  genoCount = melt(
    genotypes[, lapply(.SD, uniqueN), .SDcols=colnames(genotypes)],
    id.vars = 'person_id', variable.name = 'snp', value.name = 'count')
  okSnps = genoCount[count != 1]$snp

  statsAll = foreach(
    diseaseID = unique(scores$disease_id), .combine = rbind) %dopar% {

      diseaseGenes = phers::diseaseInfo[disease_id == diseaseID]$gene
      lmInputSub = lmInput[disease_id == diseaseID][, !c('disease_id')]

      statsGenes = foreach(
        diseaseGene = diseaseGenes, .combine = rbind) %dopar% {

          snpSub = unique(phers::geneVarMap[gene == diseaseGene]$vid)
          # making sure the snps we're looping through are in genotypes
          # and are not all 0
          snpSub = snpSub[snpSub %in% okSnps]

          statsSnps = foreach(snp=snpSub, .combine = rbind) %dopar% {

            genoCols = c('person_id', snp)
            genotypesSub = genotypes[, ..genoCols]

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
    }
return(statsAll)}



runLinear = function(lmInput, formStr, coding = 'additive') {
  lower = melt = p = se = upper = variant = NULL

  if(coding == 'additive'){
    fit = glm(formStr, data = lmInput)
    varNames = c('variant')
  }
  if(coding == 'genotypic'){
    lmInput[, variant := as.factor(variant)]
    fit = glm(formStr, data = lmInput)
    varNames = c('variant1', 'variant2')
  }

  # beta = NA, se = NA, p = NA, lower = NA, upper = NA
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
