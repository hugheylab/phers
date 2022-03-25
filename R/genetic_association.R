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
#' @param diseaseVariantMap A data.table that maps diseases to variants.
#'   Must have columns `disease_id` and `vid`.
#'   `disease_id` must include all disease IDs in `scores`.
#' @param glmFormula A formula object representing the linear model of covariates
#'   to be used in the genetic association tests. All covariates must be
#'   existing columns in `demos`.
#' @param modelType Coding of the genotypes. Either 'additive' or 'genotypic'.
#' @param dopar Logical indicating whether to run calculations in parallel if
#'   a parallel backend is already set up, e.g., using
#'   [doParallel::registerDoParallel()]. Recommended to minimize runtime.
#'
#' @return A data.table containing to results of the genotype association tests
#'   with the following columns. If the model fails to converge,
#'   NAs will be reported.
#'   * `disease_id`: disease identifier
#'   * `gene`: name of the gene
#'   * `vid`: variant identifier
#'   * `beta`: The beta coefficient for the variant
#'   * `se`: The standard error for the beta coefficient
#'   * `p`: The p-value for the variant
#'   * `lower`: The lower bound of the `confint` confidence interval
#'   * `upper`: The upper bound of the `confint` confidence interval
#'
#' @eval example3()
#'
#' @export
getGeneticAssociations = function(
  scores, genotypes, demos, diseaseVariantMap, glmFormula,
  modelType = c('additive', 'genotypic'), dopar = FALSE) {
  diseaseId = disease_id = snp = allele_count = count = N = vid = NULL

  checkScores(scores)
  checkGenotypes(genotypes)
  checkDemos(demos)
  checkDiseaseVariantMap(diseaseVariantMap, scores)
  checkGlmFormula(glmFormula, demos)
  modelType = match.arg(modelType)
  assertFlag(dopar)

  lmInput = merge(scores, demos, by = 'person_id')

  # remove snps with only one genotype in the population
  genoCount = melt(
    genotypes[, lapply(.SD, uniqueN), .SDcols = colnames(genotypes)],
    id.vars = 'person_id', variable.name = 'snp', value.name = 'count')
  okSnps = unique(genoCount[count != 1]$snp)


  reg = foreach::getDoParRegistered()
  doOp = if (dopar && reg) `%dopar%` else `%do%`
  foe = foreach(diseaseId = unique(scores$disease_id), .combine = rbind)

  statsAll = doOp(foe, {

    lmInputSub = lmInput[disease_id == diseaseId, !'disease_id']
    # making sure the snps we're looping through are in genotypes
    # and are not all 0
    diseaseVariantMapSub = unique(
      diseaseVariantMap[disease_id == diseaseId & vid %in% okSnps])

    statsSnps = foreach(
      snp = diseaseVariantMapSub$vid, .combine = rbind) %do% {


        genotypesSub = genotypes[, c('person_id', snp), with = FALSE]
        lmInputSub1 = merge(
          lmInputSub, genotypesSub, by = 'person_id')[, !'person_id']
        setnames(lmInputSub1, snp, 'allele_count')
        lmInputSub1 = lmInputSub1[!is.na(allele_count)]

        glmStat = runLinear(
          lmInputSub1, glmFormula, modelType = modelType, diseaseId, snp)}
  })
  return(statsAll[])}



#' Perform a single genotype association test
#'
#' Perform an association test between one variant and the phenotype risk scores
#' corresponding to one disease.
#'
#' @param lmInput A data.table of of the input to the genetic association model.
#'   Must have columns `score`, `allele_count` and any other covariates to be used
#'   in the linear regression model. `allele_count` should contain the number of
#'   copies of the minor allele each person has (additive model).
#' @param glmFormula a formula object representing the linear model of covariates
#'   to be used in the genetic association test. All covariates must be existing
#'   columns in `lmInput`.
#' @param modelType Coding of the genotypes. Either 'additive' or 'genotypic'
#' @param diseaseId Numeric value of the disease ID in the association test.
#' @param snp String value of the variant ID in the association test.
#'
#' @return A data.table containing to results of the genotype association test
#'   with the following columns. If the model fails to converge,
#'   NAs will be reported.
#'
#'   * `disease_id`: disease identifier
#'   * `gene`: name of the gene
#'   * `vid`: variant identifier
#'   * `beta`: The beta coefficient for the variant
#'   * `se`: The standard error for the beta coefficient
#'   * `p`: The p-value for the variant
#'   * `lower`: The lower bound of the `confint` confidence interval
#'   * `upper`: The upper bound of the `confint` confidence interval
#'
runLinear = function(
  lmInput, glmFormula, modelType = c('additive', 'genotypic'), diseaseId, snp) {
  lower = melt = pval = se = upper = allele_count = varName = n_het = n_hom =
    n_total = n_wt = NULL

  checkLmInput(lmInput)
  checkGlmFormula(glmFormula, lmInput)
  modelType = match.arg(modelType)

  glmFormula = update.formula(glmFormula, score ~ allele_count + .)

  if (modelType == 'additive'){
    fit = glm(glmFormula, data = lmInput)
    varNames = 'allele_count'}

  if (modelType == 'genotypic'){
    lmInput1 = copy(lmInput)
    lmInput1[, allele_count := as.factor(allele_count)]
    fit = glm(glmFormula, data = lmInput1)
    varNames = c('allele_count1', 'allele_count2')}

  stat = data.table(disease_id = diseaseId, vid = snp)
  stat = cbind(stat, getAlleleCounts(lmInput))

  glmStats = foreach(varName = varNames, .combine = cbind) %do% {
    glmStat = data.table(beta = NA, se = NA, pval = NA, lower = NA, upper = NA)

    if (!is.na(fit$coefficients[varName])) {
      fitSnpCoefs = summary(fit)$coefficients[varName, ]
      glmStat[, beta := fitSnpCoefs['Estimate']]
      glmStat[, se := fitSnpCoefs['Std. Error']]
      glmStat[, pval := fitSnpCoefs['Pr(>|t|)']]
      ci =  suppressMessages(confint(fit))
      glmStat[, lower := ci[varName, '2.5 %']]
      glmStat[, upper := ci[varName, '97.5 %']]}

    c0 = c('beta', 'se', 'pval', 'lower', 'upper')
    c1 = if (varName == 'allele_count1') '_het' else if (
      varName == 'allele_count2') '_hom' else ''
    setnames(glmStat, c0, paste0(c0, c1))}

  stat = cbind(stat, glmStats)

return(stat[])}
