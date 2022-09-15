getWeightsPrevalence = function(demos, phecodeOccurrences) {
  phecode = person_id = . = prev = w = NULL
  weights = phecodeOccurrences[, .(
    prev = uniqueN(person_id) / nrow(demos)),
    keyby = phecode]
  weights[, w := -log10(prev)]
  return(weights)}


getWeightsLogistic = function(
    demos, phecodeOccurrences, methodFormula, foreachCall, doOp) {
  phecode = person_id = . = w = phe = dx_status = prob = NULL

  weights = doOp(foreachCall, {
    pheSub = unique(phecodeOccurrences[phecode == phe, .(person_id, phecode)])
    glmInput = merge(demos, pheSub, by = 'person_id', all.x = TRUE)
    glmInput[!is.na(phecode), dx_status := 1]
    glmInput[is.na(phecode), dx_status := 0]
    glmInput[, phecode := phe]

    methodFormula = update.formula(methodFormula, dx_status ~ .)
    fit = glm(methodFormula, data = glmInput, family = 'binomial')
    glmInput[, prob := predict(
      fit, newdata = .SD , type = 'response', se.fit = FALSE)]
    glmInput = glmInput[, .(person_id, phecode, prob, dx_status)]})

  weights[, w := -log10(prob) * dx_status]
  weights[, dx_status := NULL]
  return(weights)}


getWeightsLoglinear = function(
    demos, phecodeOccurrences, methodFormula, foreachCall, doOp) {
  phecode = person_id = . = w = phe = pred = num_occurrences = NULL

  weights = doOp(foreachCall, {
    pheSub = unique(
      phecodeOccurrences[phecode == phe, .(person_id, phecode, num_occurrences)])
    lmInput = merge(demos, pheSub, by = 'person_id', all.x = TRUE)

    lmInput[is.na(phecode), num_occurrences := 0]
    lmInput[, phecode := phe]

    methodFormula = update.formula(methodFormula, log2(num_occurrences + 1) ~ .)
    fit = lm(methodFormula, data = lmInput)
    lmInput[, pred := predict(fit, newdata = .SD)]
    lmInput = lmInput[, .(person_id, phecode, num_occurrences, pred)]})

  weights[, w := log2(num_occurrences + 1) - pred]
  weights[num_occurrences == 0, w := 0]
  weights[, `:=`(num_occurrences = NULL, pred = NULL)]
  return(weights)}


getWeightsCox = function(
    demos, phecodeOccurrences, methodFormula, foreachCall, doOp) {
  phecode = person_id = . = w = phe = dx_status = prob = occurrence_age =
    first_age = last_age = age2 = NULL

  weights = doOp(foreachCall, {
    pheSub = unique(
      phecodeOccurrences[phecode == phe, .(person_id, phecode, occurrence_age)])
    coxInput = merge(demos, pheSub, by = 'person_id', all.x = TRUE)

    coxInput[!is.na(phecode), `:=`(dx_status = 1, age2 = occurrence_age)]
    coxInput[is.na(phecode), `:=`(dx_status = 0, age2 = last_age)]
    coxInput[, phecode := phe]
    coxInput[, `:=`(last_age = NULL, occurrence_age = NULL)]

    coxInput[age2 == first_age, age2 := age2 + (1 / 365.25)]
    coxInput = coxInput[age2 > first_age]

    methodFormula = update.formula(
      methodFormula, Surv(first_age, age2, dx_status) ~ .)
    fit = coxph(methodFormula, data = coxInput, model = TRUE)
    coxInput[, prob := 1 - exp(
      -predict(fit, newdata = .SD, type = 'expected', se.fit = FALSE))]
    coxInput = coxInput[, .(person_id, phecode, prob, dx_status)]})

  weights[, w := -log10(prob) * dx_status]
  weights[prob == 0 & dx_status == 0, w := 0]
  weights[, dx_status := NULL]
  return(weights)}


#' Calculate phecode-specific weights for phenotype risk scores
#'
#' This is typically the second step of an analysis using phenotype risk scores,
#' the next is [getScores()].
#'
#' @param demos A data.table having one row per person in the cohort. Must have
#'   a column `person_id`. When the `cox` method is used, `demos`
#'   must have columns `first_age` and `last_age` corresponding to first and
#'   last age of visit (in years).
#' @param phecodeOccurrences A data.table of phecode occurrences for each person
#'   in the cohort. Must have columns `person_id` and `phecode` under the
#'   "prevalence" or "logistic" methods, columns `person_id`, `phecode`, and
#'   `num_occurrences` under the "loglinear" method, and columns `person_id`,
#'   `phecode`, and `occurrence_age` under the "cox" method. `num_occurrences`
#'   refers to the number of unique dates a phecode was recorded for a person.
#'   `occurrence_age` refers to the first age (in years) a person acquired a
#'   phecode.
#' @param method A string indicating the statistical model for calculating
#'   weights.
#' @param methodFormula A formula representing the right-hand side of the model
#'   corresponding to `method`. All terms in the formula must correspond to
#'   columns in `demos`. Do not use age-related covariates with the "cox"
#'   method.
#' @param dopar Logical indicating whether to run calculations in parallel if
#'   a parallel backend is already set up, e.g., using
#'   [doParallel::registerDoParallel()]. Recommended to minimize runtime.
#'
#' @return A data.table with various columns. If `method` is "prevalence":
#'   `phecode`, `prev` (prevalence), and `w` (weight). If `method` is
#'   "logistic" or "cox": `person_id`, `phecode`, `prob` (probability), and `w`.
#'   If `method` is "loglinear": `person_id`, `phecode`, and `w`. Prevalence
#'   corresponds to fraction of the cohort that has at least one occurrence of
#'   the given phecode. Probability corresponds to predicted probability of
#'   given individual having a given phecode based on `method` and
#'   `methodFormula`. For the "prevalence" `method`, weight is calculated as
#'   `-log10(prev)`, for "logistic" and "cox" as `-log10(prob)`, and for
#'   "loglinear" as the difference between observed and predicted
#'   `log2(num_occurrences + 1)`.
#'
#' @eval example1()
#'
#' @seealso [getPhecodeOccurrences()], [getScores()], [phers()]
#'
#' @export
getWeights = function(
    demos, phecodeOccurrences,
    method = c('prevalence', 'logistic', 'cox', 'loglinear'),
    methodFormula = NULL, dopar = FALSE) {

  method = match.arg(method)
  checkDemos(demos, method)
  checkPhecodeOccurrences(phecodeOccurrences, demos, method)

  if (method == 'prevalence') {
    weights = getWeightsPrevalence(demos, phecodeOccurrences)
    return(weights[])}

  checkMethodFormula(methodFormula, demos)
  assertFlag(dopar)

  reg = foreach::getDoParRegistered()
  doOp = if (dopar && reg) `%dopar%` else `%do%`
  foe = foreach(phe = unique(phecodeOccurrences$phecode), .combine = rbind)

  getWeightsFunc = switch(
    method, logistic = getWeightsLogistic,
    loglinear = getWeightsLoglinear, cox = getWeightsCox)

  weights = getWeightsFunc(demos, phecodeOccurrences, methodFormula, foe, doOp)
  return(weights[])}
