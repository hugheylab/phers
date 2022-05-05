#' Identify cases and controls for Mendelian diseases
#'
#' This function is useful for verifying that raw or residual phenotype risk
#' scores of diagnosed individuals (cases) tend to be higher than scores of
#' undiagnosed individuals (controls).
#'
#' @param demos A data.table having one row per person in the cohort. Must have
#'   a column `person_id`.
#' @param icdOccurrences A data.table of occurrences of ICD codes for each
#'   person in the cohort. Must have columns `person_id`, `icd`, `flag`, and
#'   `entry_date`.
#' @param minUniqueDates Integer indicating the minimum number of unique
#'   ICD code entry dates required to classify a person as a case. Persons with
#'   at least one, but fewer than `minUniqueDates` entry dates, are assigned as
#'   neither cases nor controls.
#' @param diseaseDxIcdMap A data.table of the mapping between diseases and
#'   the corresponding ICD codes that indicate a diagnosis. Must have columns
#'   `disease_id`, `icd`, and `flag`. Default is [diseaseDxIcdMap].
#'
#' @return A data.table with columns `person_id`, `disease_id`, and `dx_status`
#'   (1 indicates a case, 0 indicates a control, -1 indicates neither).
#'
#' @eval example2()
#'
#' @seealso [phers()]
#'
#' @export
getDxStatus = function(
  demos, icdOccurrences, minUniqueDates = 2L,
  diseaseDxIcdMap = phers::diseaseDxIcdMap) {
  dx_status = entry_date = uniq_dates = . = NULL

  checkDemos(demos)
  checkIcdOccurrences(
    icdOccurrences, cols = c('person_id', 'icd', 'flag', 'entry_date'))
  checkDate(icdOccurrences$entry_date)
  assertCount(minUniqueDates, positive = TRUE)

  checkDxIcd(diseaseDxIcdMap, nullOk = FALSE)
  byCols = c('person_id', 'disease_id')

  dxIcd = merge(
    icdOccurrences, diseaseDxIcdMap[, c('disease_id', 'icd', 'flag')],
    by = c('icd', 'flag'))
  dxIcd = dxIcd[, .(uniq_dates = uniqueN(entry_date)), keyby = byCols]
  dxIcd[, dx_status := -1L]
  dxIcd[uniq_dates >= minUniqueDates, dx_status := 1L]

  dxStatus = merge(
    CJ(person_id = demos$person_id,
       disease_id = unique(diseaseDxIcdMap$disease_id)),
    dxIcd[, c(byCols, 'dx_status'), with = FALSE], by = byCols, all.x = TRUE)
  dxStatus[is.na(dx_status), dx_status := 0L]
  return(dxStatus[])}
