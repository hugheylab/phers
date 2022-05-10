#' @import checkmate
#' @import data.table
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom stats lm confint update.formula rstandard
#' @importFrom iterators iter
#' @importFrom BEDMatrix BEDMatrix
# BEDMatrix importFrom only to avoid note on R CMD check
NULL


#' Map ICD code occurrences to phecode occurrences
#'
#' This is typically the first step of an analysis using phenotype risk scores,
#' the next is [getWeights()].
#'
#' @param icdOccurrences A data.table of occurrences of ICD codes for each
#'   person in the cohort. Must have columns `person_id`, `icd`, and `flag`.
#' @param icdPhecodeMap A data.table of the mapping between ICD codes and
#'   phecodes. Must have columns `icd`, `phecode`, and `flag`. Default is the
#'   map included in this package.
#' @param dxIcd A data.table of ICD codes to exclude from mapping to phecodes.
#'   Must have columns `icd` and `flag`. Default is the table of Mendelian
#'   diseases and the corresponding ICD codes that indicate a genetic diagnosis.
#'   If `NULL`, no ICD codes will be excluded.
#'
#' @return A data.table of phecode occurrences for each person.
#'
#' @eval example1()
#'
#' @seealso [getWeights()], [getScores()], [phers()]
#'
#' @export
getPhecodeOccurrences = function(
  icdOccurrences, icdPhecodeMap = phers::icdPhecodeMap,
  dxIcd = phers::diseaseDxIcdMap) {
  flag = icd = person_id = . = NULL

  checkIcdOccurrences(icdOccurrences)
  checkIcdPhecodeMap(icdPhecodeMap)
  checkDxIcd(dxIcd, nullOk = TRUE)

  # remove diagnostic codes
  if (!is.null(dxIcd)) {
    icdOccurrences = icdOccurrences[!dxIcd, on = c('icd', 'flag')]}

  pheOccs = merge(
    icdOccurrences, icdPhecodeMap[, c('icd', 'flag', 'phecode')],
    by = c('icd', 'flag'), allow.cartesian = TRUE)
  pheOccs = unique(pheOccs[, !c('icd', 'flag')])

  setcolorder(pheOccs, c('person_id', 'phecode'))
  setkeyv(pheOccs, c('person_id', 'phecode'))
  return(pheOccs)}


#' Calculate phecode-specific weights for phenotype risk scores
#'
#' This is typically the second step of an analysis using phenotype risk scores,
#' the next is [getScores()].
#'
#' @param demos A data.table having one row per person in the cohort. Must have
#'   a column `person_id`.
#' @param phecodeOccurrences A data.table of phecode occurrences for each person
#'   in the cohort. Must have columns `person_id` and `phecode`.
#'
#' @return A data.table with columns `phecode`, `prev` (prevalence), and `w`
#'   (weight). Prevalence corresponds to fraction of the cohort that has at
#'   least one occurrence of the given phecode. Weight is calculated as `-log10`
#'   prevalence.
#'
#' @eval example1()
#'
#' @seealso [getPhecodeOccurrences()], [getScores()], [phers()]
#'
#' @export
getWeights = function(demos, phecodeOccurrences) {
  phecode = person_id = . = prev = w = NULL

  checkDemos(demos)
  checkPhecodeOccurrences(phecodeOccurrences, demos)

  weights = phecodeOccurrences[
    , .(prev = uniqueN(person_id) / nrow(demos)),
    keyby = phecode]
  weights[, w := -log10(prev)]
  return(weights[])}


#' Calculate phenotype risk scores
#'
#' A person's phenotype risk score for a given disease corresponds to the
#' sum of the weights of the disease-relevant phecodes that the person has
#' received.
#'
#' @param demos A data.table having one row per person in the cohort. Must have
#'   a column `person_id`.
#' @param phecodeOccurrences A data.table of phecode occurrences for each person
#'   in the cohort. Must have columns `person_id` and `phecode`.
#' @param weights A data.table of phecodes and their corresponding weights.
#'   Must have columns `phecode` and `w`.
#' @param diseasePhecodeMap A data.table of the mapping between diseases and
#'   phecodes. Must have columns `disease_id` and `phecode`.
#'
#' @return A data.table containing the phenotype risk score for each person for
#'   each disease.
#'
#' @eval example1()
#'
#' @seealso [mapDiseaseToPhecode()], [getPhecodeOccurrences()], [getWeights()],
#'   [getResidualScores()], [phers()]
#'
#' @export
getScores = function(demos, phecodeOccurrences, weights, diseasePhecodeMap) {
  person_id = phecode = disease_id = w = score = . = NULL

  checkDemos(demos)
  checkPhecodeOccurrences(phecodeOccurrences, demos)
  checkWeights(weights)
  checkDiseasePhecodeMap(diseasePhecodeMap)

  rBig = merge(unique(phecodeOccurrences[, .(person_id, phecode)]),
               diseasePhecodeMap, by = 'phecode', allow.cartesian = TRUE)

  rBig = merge(rBig, weights, by = 'phecode')
  rSum = rBig[, .(score = sum(w)), keyby = .(person_id, disease_id)]
  r = merge(CJ(person_id = demos$person_id,
               disease_id = unique(diseasePhecodeMap$disease_id)),
            rSum, by = c('person_id', 'disease_id'), all.x = TRUE)
  r[is.na(score), score := 0]
  return(r[])}


#' Calculate residual phenotype risk scores
#'
#' The residual score indicates to what extent a person's phenotype risk score
#' for a given disease deviates from the expected score, after adjusting for
#' the person's characteristics in a linear model.
#'
#' @param demos A data.table of characteristics for each person in the cohort.
#'   Must have column `person_id`.
#' @param scores A data.table containing the phenotype risk score for each
#'   person for each disease. Must have columns `person_id`, `disease_id`, and
#'   `score`.
#' @param lmFormula A formula representing the linear model to use for
#'   calculating residual scores. All terms in the formula must correspond to
#'   columns in `demos`.
#'
#' @return A data.table, based on `scores`, with an additional column
#'   `resid_score`. Residual scores for each disease are standardized to have
#'   unit variance.
#'
#' @eval example1()
#'
#' @seealso [stats::rstandard()], [getScores()], [phers()]
#'
#' @export
getResidualScores = function(demos, scores, lmFormula) {
  disease_id = diseaseId = resid_score = . = person_id = score = NULL

  checkDemos(demos)
  checkScores(scores)
  checkLmFormula(lmFormula, demos)

  rInput = merge(scores, demos, by = 'person_id')
  lmFormula = update.formula(lmFormula, score ~ .)

  rScores = rInput[
    , .(person_id, score,
        resid_score = rstandard(lm(lmFormula, data = .SD))),
    keyby = disease_id]
  setkeyv(rScores, c('person_id', 'disease_id'))
  setcolorder(rScores, c('person_id', 'disease_id', 'score', 'resid_score'))

  return(rScores[])}


#' Perform multiple steps of an analysis using phenotype risk scores
#'
#' This function can map ICD occurrences to phecode occurrences, calculate
#' weights for each phecode, and calculate raw and residual phenotype risk
#' scores.
#'
#' @param demos A data.table having one row per person in the cohort. Must have
#'   a column `person_id`.
#' @param icdOccurrences A data.table of occurrences of ICD codes for each
#'   person in the cohort. Must have columns `person_id`, `icd`, and `flag`.
#' @param diseasePhecodeMap A data.table of the mapping between diseases and
#'   phecodes. Must have columns `disease_id` and `phecode`.
#' @param icdPhecodeMap A data.table of the mapping between ICD codes and
#'   phecodes. Must have columns `icd`, `phecode`, and `flag`. Default is the
#'   map included in this package.
#' @param dxIcd A data.table of ICD codes to exclude from mapping to phecodes.
#'   Must have columns `icd` and `flag`. Default is the table of Mendelian
#'   diseases and the corresponding ICD codes that indicate a genetic diagnosis.
#'   If `NULL`, no ICD codes will be excluded.
#' @param weights A data.table of phecodes and their corresponding weights.
#'   Must have columns `phecode` and `w`. If `NULL` (the default), weights will
#'   be calculated based on data for the cohort provided. If the cohort is small
#'   or its phecode prevalences do not reflect those in the population of
#'   interest, it is recommended to use [preCalcWeights].
#' @param residScoreFormula A formula representing the linear model to use for
#'   calculating residual scores. All terms in the formula must correspond to
#'   columns in `demos`. If `NULL`, no residual scores will be calculated.
#'
#' @return A list with elements:
#'
#' * `phecodeOccurences`: A data.table of phecode occurrences for each person
#'   in the cohort.
#' * `weights`: A data.table of phecodes and their corresponding weights.
#' * `scores`: A data.table of raw and possibly residual phenotype risk scores
#'   for each person and each disease.
#'
#' @eval example4()
#'
#' @seealso [getPhecodeOccurrences()], [getWeights()], [getScores()],
#'   [getResidualScores()], [mapDiseaseToPhecode()], [icdPhecodeMap],
#'   [diseaseDxIcdMap], [preCalcWeights], [getDxStatus()]
#'
#' @export
phers = function(
  demos, icdOccurrences, diseasePhecodeMap,
  icdPhecodeMap = phers::icdPhecodeMap, dxIcd = phers::diseaseDxIcdMap,
  weights = NULL, residScoreFormula = NULL) {

  checkDemos(demos)
  checkIcdOccurrences(icdOccurrences)
  checkDiseasePhecodeMap(diseasePhecodeMap)
  checkIcdPhecodeMap(icdPhecodeMap)
  checkDxIcd(dxIcd, nullOk = TRUE)
  if (!is.null(weights)) checkWeights(weights)
  if (!is.null(residScoreFormula)) checkLmFormula(residScoreFormula, demos)

  phecodeOccurrences = getPhecodeOccurrences(
    icdOccurrences, icdPhecodeMap = icdPhecodeMap, dxIcd = dxIcd)

  if (is.null(weights)) weights = getWeights(demos, phecodeOccurrences)

  scores = getScores(demos, phecodeOccurrences, weights, diseasePhecodeMap)

  if (!is.null(residScoreFormula)) {
    scores = getResidualScores(demos, scores, residScoreFormula)}

  output = list(
    phecodeOccurrences = phecodeOccurrences, weights = weights, scores = scores)
  return(output)}
