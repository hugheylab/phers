#' Perform genotype association tests
#'
#' Perform association tests between the phenotype risk scores and genotypes
#'
#' @param scores A data.table of phenotype risk scores. Must have columns
#'   `person_id`, `disease_id`, `score`.
#' @param genotypes  A 'BEDMatrix' object linked to the PLINK bed file containing
#'   genetic data. The row names correspond to the `person_id`'s in
#'   `demos` and `scores` tables. The column names correspond to variant IDs.
#' @param demos A data.table of covariates to be used in the association
#'   analysis. Must have column `person_id`.
#' @param diseaseVariantMap A data.table that maps diseases to variants.
#'   Must have columns `disease_id` and `vid`.
#'   `disease_id` must include all disease IDs in `scores`.
#' @param glmFormula A formula object representing the linear model of covariates
#'   to be used in the genetic association tests. All covariates must be
#'   existing columns in `demos`.
#' @param modelType Coding of the genotypes. Either 'additive' or 'genotypic'.
#' @param level Confidence level used for calculating confidence intervals.
#'   Default is 0.95.
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
#'   * `beta`: The beta coefficient for the association
#'   * `se`: The standard error for the beta coefficient
#'   * `pval`: The p-value for the association
#'   * `ci_lower`: The lower bound of the `confint` confidence interval
#'   * `ci_upper`: The upper bound of the `confint` confidence interval
#'
#' @eval example3()
#'
#' @export
getGeneticAssociations = function(
  scores, genotypes, demos, diseaseVariantMap, glmFormula,
  modelType = c('additive', 'dominant', 'recessive', 'genotypic'),
                level = 0.95, dopar = FALSE) {
  diseaseId = disease_id = snp = allele_count = count = N = vid = NULL

  checkScores(scores)
  checkGenotypes(genotypes)
  checkDemos(demos)
  checkDiseaseVariantMap(diseaseVariantMap, scores)
  checkGlmFormula(glmFormula, demos)
  modelType = match.arg(modelType)
  assertFlag(dopar)

  lmInput = merge(scores, demos, by = 'person_id')


  reg = foreach::getDoParRegistered()
  doOp = if (dopar && reg) `%dopar%` else `%do%`
  foe = foreach(diseaseId = unique(scores$disease_id), .combine = rbind)

  statsAll = doOp(foe, {

    lmInputSub = lmInput[disease_id == diseaseId, !'disease_id']

    # making sure the variants we're looping through are in genotypes
    snpSub = unique(diseaseVariantMap[disease_id == diseaseId]$vid)
    snpSub = intersect(colnames(genotypes),snpSub)

    genotypesSub = data.table(
      'person_id' = as.numeric(rownames(genotypes)),
      genotypes[, snpSub, drop=FALSE])

    # exclude variants with only one genotype in the population
    genoCount = melt(
      genotypesSub[, lapply(.SD, uniqueN), .SDcols = snpSub],
      measure.vars = snpSub , variable.name = 'snp', value.name = 'count',
      variable.factor = FALSE)
    snpSub = unique(genoCount[count != 1]$snp)


    statsSnps = foreach(
      snp = snpSub, .combine = rbind) %do% {


        genotypesSub2 = genotypesSub[, c('person_id', snp), with = FALSE]
        lmInputSub2 = merge(
          lmInputSub, genotypesSub2, by = 'person_id')[, !'person_id']
        setnames(lmInputSub2, snp, 'allele_count')
        lmInputSub2 = lmInputSub2[!is.na(allele_count)]

        glmStat = runLinear(
          lmInputSub2, glmFormula, modelType = modelType, diseaseId, snp, level)}
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
#' @param level Confidence level used for calculating confidence intervals.
#'   Default is 0.95.
#'
#' @return A data.table containing to results of the genotype association test
#'   with the following columns. If the model fails to converge,
#'   NAs will be reported.
#'
#'   * `disease_id`: disease identifier
#'   * `gene`: name of the gene
#'   * `vid`: variant identifier
#'   * `beta`: The beta coefficient for the association
#'   * `se`: The standard error for the beta coefficient
#'   * `pval`: The p-value for the association
#'   * `ci_lower`: The lower bound of the `confint` confidence interval
#'   * `ci_upper`: The upper bound of the `confint` confidence interval
#'
runLinear = function(
  lmInput, glmFormula,
  modelType = c('additive', 'dominant', 'recessive', 'genotypic'),
  diseaseId, snp, level = 0.95) {
  ci_lower = melt = pval = se = ci_upper = allele_count = varName = n_het = n_hom =
    n_total = n_wt = NULL

  checkLmInput(lmInput)
  checkGlmFormula(glmFormula, lmInput)
  modelType = match.arg(modelType)

  lmInput1 = copy(lmInput)
  glmFormula = update.formula(glmFormula, score ~ allele_count + .)
  varNames = 'allele_count'

  if (modelType == 'dominant') {
    lmInput1[allele_count == 2, allele_count := 1]}

  else if (modelType == 'recessive') {
    lmInput1[allele_count == 1, allele_count := 0]
    lmInput1[allele_count == 2, allele_count := 1]}

  else if (modelType == 'genotypic') {
    lmInput1[, allele_count := as.factor(allele_count)]
    varNames = c('allele_count1', 'allele_count2')}

  fit = glm(glmFormula, data = lmInput1)
  stat = data.table(disease_id = diseaseId, vid = snp)
  stat = cbind(stat, getAlleleCounts(lmInput))

  glmStats = foreach(varName = varNames, .combine = cbind) %do% {
    glmStat = data.table(beta = NA, se = NA, pval = NA, ci_lower = NA, ci_upper = NA)

    if (!is.na(fit$coefficients[varName])) {
      fitSnpCoefs = summary(fit)$coefficients[varName, ]
      glmStat[, beta := fitSnpCoefs['Estimate']]
      glmStat[, se := fitSnpCoefs['Std. Error']]
      glmStat[, pval := fitSnpCoefs['Pr(>|t|)']]
      ci =  suppressMessages(confint(fit, parm = varName, level = level))
      glmStat[, ci_lower := ci[1]]
      glmStat[, ci_upper := ci[2]]}

    c0 = c('beta', 'se', 'pval', 'ci_lower', 'ci_upper')
    c1 = if (varName == 'allele_count1') '_het' else if (
      varName == 'allele_count2') '_hom' else ''
    setnames(glmStat, c0, paste0(c0, c1))}

  stat = cbind(stat, glmStats)

return(stat[])}
