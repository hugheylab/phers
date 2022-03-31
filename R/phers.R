#' @import checkmate
#' @import data.table
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom stats confint glm as.formula update.formula rstandard
#' @importFrom iterators iter
NULL


#' Map ICD code occurrences to phecode occurrences
#'
#' This function takes a data table of patient ICD codes and maps them to
#' phecodes.
#'
#' @param icdOccurrences A data.table of occurrences of ICD codes for each
#'   person in the cohort. Must have columns `person_id`, `icd`, and `flag`.
#' @param icdPhecodeMap A data.table containing the mapping between ICD codes
#'   and phecodes. Must have columns `icd`, `phecode`, and `flag`. By default uses
#'   the mapping included in this package.
#' @param dxIcd A data.table of diagnostic ICD codes to remove for every person.
#'   Must have columns `icd` and `flag`. By default uses a table provided in
#'   this package with mapping between diseases and ICD codes used to indicate
#'   their diagnosis. If `NULL` no ICD codes will be removed.
#'
#' @return A data.table of phecode occurrences for each person.
#'
#' @eval example1()
#'
#' @export
getPhecodeOccurrences = function(
  icdOccurrences, icdPhecodeMap = phers::icdPhecodeMap,
  dxIcd = phers::diseaseDxIcdMap) {
  flag = icd = person_id = . = NULL

  checkIcdOccurrences(icdOccurrences)

  assertDataTable(icdPhecodeMap)
  assertNames(colnames(icdPhecodeMap),
              permutation.of = c('phecode', 'icd', 'flag'))
  assertCharacter(icdPhecodeMap$icd)
  assertCharacter(icdPhecodeMap$phecode)
  assert(anyDuplicated(icdPhecodeMap) == 0)


  assert(checkDataTable(dxIcd), checkNull(dxIcd))
  if (!is.null(dxIcd)) {
  assertDataTable(dxIcd)
  assertNames(colnames(dxIcd),
              must.include = c('icd', 'flag'),
              disjunct.from = 'person_id')
  assertCharacter(dxIcd$icd)}

  # remove diagnostic codes
  if (!is.null(dxIcd)) {
    icdOccurrences = icdOccurrences[!dxIcd, on = c('icd', 'flag')]}

  pheOccs = merge(
    icdOccurrences, icdPhecodeMap, by = c('icd', 'flag'), allow.cartesian = TRUE)
  pheOccs = pheOccs[, !c('icd', 'flag')]
  return(pheOccs)}


#' Calculate phecode-specific weights for phenotype risk scores
#'
#' The weights correspond to the -log10 prevalences in the cohort.
#'
#' @param demos A data.table having one row per person in the cohort. Must have
#'   a column `person_id`.
#' @param phecodeOccurrences A data.table of occurrences of phecodes for each
#'   person in the cohort. Must have columns `person_id` and `phecode`.
#'
#' @return A data.table containing the prevalence (`prev`) and weight (`w`) for
#'   each phecode.
#'
#' @eval example1()
#'
#' @export
getWeights = function(demos, phecodeOccurrences) {
  phecode = person_id = . = prev = w = NULL

  checkDemos(demos)
  checkPhecodeOccurrences(phecodeOccurrences, demos)

  weights = phecodeOccurrences[, .(prev = uniqueN(person_id) / nrow(demos)),
                               by = phecode]
  weights[, w := -log10(prev)]
  return(weights[])}


#' Calculate phenotype risk scores
#'
#' A person's phenotype risk score for a particular disease corresponds to the
#' sum of the weights of the disease-relevant phecodes that the person has
#' received.
#'
#' @param demos A data.table having one row per person in the cohort. Must have
#'   a column `person_id`.
#' @param phecodeOccurrences A data.table of occurrences of phecodes for each
#'   person in the cohort. Must have columns `person_id` and `phecode`.
#' @param weights A data.table where each row is a phecode and the weight
#'   corresponding to it. The columns are `phecode` and `w`.
#' @param diseasePhecodeMap A data.table containing the mapping between
#'   diseases and phecodes. Must have columns `disease_id` and `phecode`.
#'
#' @return A data.table containing the phenotype risk score for each person for
#'   each disease.
#'
#' @eval example1()
#'
#' @export
getScores = function(demos, phecodeOccurrences, weights, diseasePhecodeMap) {
  person_id = phecode = disease_id = w = score = . = NULL

  checkDemos(demos)
  checkPhecodeOccurrences(phecodeOccurrences, demos)
  checkWeights(weights)
  checkDiseasePhecodeMap(diseasePhecodeMap)

  pheOccWei = merge(unique(phecodeOccurrences[, .(person_id, phecode)]),
                    weights, by = 'phecode')

  rBig = merge(pheOccWei, diseasePhecodeMap, by = 'phecode',
               allow.cartesian = TRUE)
  rSum = rBig[, .(score = sum(w)), by = .(person_id, disease_id)]
  r = merge(CJ(person_id = demos$person_id,
               disease_id = unique(diseasePhecodeMap$disease_id)),
            rSum, by = c('person_id', 'disease_id'), all.x = TRUE)
  r[is.na(score), score := 0]
  return(r[])}


#' Calculate residual phenotype risk scores
#'
#' The residual phenotype risk score corresponds to how much a person's
#' phenotype risk score deviates from what is expected given
#' their characteristics (e.g. age, sex, and race)
#'
#' @param demos A data.table of covariates to be used in the calculation
#'   of residual phenotype risk score. Must have column `person_id`.
#' @param scores A data.table containing the phenotype risk score for each person for
#'   each disease. Must have columns `person_id`, `disease_id`, and `score`.
#' @param glmFormula A formula object representing the linear model of covariates
#'   to be used in the calculation of residual phenotype risk score.
#'   All covariates must be existing columns in `demos`.
#'
#' @return A data.table containing the phenotype risk score and the residual
#'   phenotype risk score for each person for each disease.
#'
#' @eval example1()
#'
#' @export
getResidualScores = function(demos, scores, glmFormula) {
  disease_id = diseaseId = resid_score = . = person_id = score = NULL

  checkDemos(demos)
  checkScores(scores)
  checkGlmFormula(glmFormula, demos)

  rInput = merge(scores, demos, by = 'person_id')
  glmFormula = update.formula(glmFormula, score ~ .)

  rScores = rInput[
    , .(person_id, score,
        resid_score = rstandard(glm(glmFormula, data = .SD))),
    by = disease_id]
  setcolorder(rScores, c('person_id', 'disease_id', 'score', 'resid_score'))

return(rScores[])}
