#' @import checkmate
#' @import data.table
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom stats lm confint update.formula rstandard glm predict
#' @importFrom iterators iter
#' @importFrom BEDMatrix BEDMatrix
#' @importFrom survival coxph
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
#' @eval example2()
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
#'   Must have columns `phecode` and `w` when weights are unique to the
#'   population (e.g. calculated using the `prevalence` method)
#'   and `person_id`, `phecode`, and `w` when weights are unique to each
#'   person (e.g. calculated using `logistic`, `cox`, or `loglinear` methods).
#' @param diseasePhecodeMap A data.table of the mapping between diseases and
#'   phecodes. Must have columns `disease_id` and `phecode`.
#'
#' @return A data.table containing the phenotype risk score for each person for
#'   each disease.
#'
#' @eval example2()
#'
#' @seealso [mapDiseaseToPhecode()], [getPhecodeOccurrences()], [getWeights()],
#'   [getResidualScores()], [phers()]
#'
#' @export
getScores = function(
    demos, phecodeOccurrences, weights, diseasePhecodeMap) {
  person_id = phecode = disease_id = w = score = . = NULL

  checkDemos(demos)
  checkPhecodeOccurrences(phecodeOccurrences, demos)
  checkDiseasePhecodeMap(diseasePhecodeMap)

  rBig = merge(unique(phecodeOccurrences[, .(person_id, phecode)]),
               diseasePhecodeMap, by = 'phecode', allow.cartesian = TRUE)

  if ('person_id' %in% colnames(weights)) {
    checkWeights(weights, type = 'personalized')
    rBig = merge(rBig, weights, by = c('person_id', 'phecode'))}
  else {
    checkWeights(weights, type = 'population')
    rBig = merge(rBig, weights, by = 'phecode')}

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
#' @eval example2()
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
#'   a column `person_id`. The `cox` method also requires `first_age` and
#'   `last_age` columns corresponding to the first and last age of visit
#'   (in years).
#' @param icdOccurrences A data.table of occurrences of ICD codes for each
#'   person in the cohort. Must have columns `person_id`, `icd`, and `flag`.
#'   The `cox` method requires an additional `occurrence_age` column
#'   corresponding to the age (in years) a person acquired an ICD code.
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
#' @param method A string indicating the statistical model for calculating weights.
#' @param methodFormula A formula representing the right hand side of the model
#'  corresponding to `method`. All terms in the formula must correspond to
#'  columns in `demos`. A method formula is not required for the `prevalence`
#'  method.
#' @param dopar Logical indicating whether to run the calculation of weights
#'   in parallel if a parallel backend is already set up, e.g., using
#'   [doParallel::registerDoParallel()]. Recommended to minimize runtime.
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
#' @eval example5()
#'
#' @seealso [getPhecodeOccurrences()], [getWeights()], [getScores()],
#'   [getResidualScores()], [mapDiseaseToPhecode()], [icdPhecodeMap],
#'   [diseaseDxIcdMap], [preCalcWeights], [getDxStatus()]
#'
#' @export
phers = function(
  demos, icdOccurrences, diseasePhecodeMap,
  icdPhecodeMap = phers::icdPhecodeMap, dxIcd = phers::diseaseDxIcdMap,
  weights = NULL, method = c('prevalence', 'logistic', 'cox', 'loglinear'),
  methodFormula = NULL, dopar = FALSE, residScoreFormula = NULL) {

  occurrence_age = person_id = phecode = . = NULL

  method = match.arg(method)
  checkDemos(demos, method = method)

  if(method == 'cox') {
    checkIcdOccurrences(
      icdOccurrences, cols = c('person_id', 'icd', 'flag', 'occurrence_age'))}
  else {
    checkIcdOccurrences(icdOccurrences)}

  checkDiseasePhecodeMap(diseasePhecodeMap)
  checkIcdPhecodeMap(icdPhecodeMap)
  checkDxIcd(dxIcd, nullOk = TRUE)
  if (!is.null(weights)) checkWeights(weights, type = 'population')

  if (method != 'prevalence') {
    checkMethodFormula(methodFormula, demos)
    assertFlag(dopar)}
  if (!is.null(residScoreFormula)) checkLmFormula(residScoreFormula, demos)

  phecodeOccurrences = getPhecodeOccurrences(
    icdOccurrences, icdPhecodeMap = icdPhecodeMap, dxIcd = dxIcd)

  if (method == 'cox') {
    phecodeOccurrences = phecodeOccurrences[, .(
      occurrence_age = min(occurrence_age)) , by = .(person_id, phecode)]}
  else if (method == 'loglinear') {
    phecodeOccurrences = phecodeOccurrences[, .(
      num_occurrences = uniqueN(occurrence_age)), by = .(person_id, phecode)]}

  if (is.null(weights)) weights = getWeights(
    demos, phecodeOccurrences, method, methodFormula, dopar)

  scores = getScores(
    demos, phecodeOccurrences, weights, diseasePhecodeMap)

  if (!is.null(residScoreFormula)) {
    scores = getResidualScores(demos, scores, residScoreFormula)}

  output = list(
    phecodeOccurrences = phecodeOccurrences, weights = weights, scores = scores)
  return(output)}
