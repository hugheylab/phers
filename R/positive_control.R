#' Identify Mendelian disease cases and controls
#'
#' This function Identifies cases and controls of Mendelian diseases.
#'
#' @param demos A data.table having one row per person in the cohort. Must have
#'   a column `person_id`.
#' @param icdOccurrences A data.table of occurrences of ICD codes for each
#'   person in the cohort.
#'   Must have columns `person_id`, `icd`, `flag`, and `entry_date`.
#' @param minUniqDates Positive integer indicating the minimum number of unique
#'   ICD code entry dates required to classify a person as a case.
#' @param diseaseDxIcdMap A data.table containing mapping between diseases and
#'   the icd codes that represent being diagnosed with them. Must have columns
#'   `disease_id`, `icd` and `flag`.
#'
#' @return A data.table with cases and controls of each disease in
#'   `diseaseDxIcdMap`. The columns are `person_id`, `disease_id`,
#'   and `dx_status`.
#'
#' @eval example2()
#'
#' @export
getDxStatus = function(
  demos, icdOccurrences, minUniqDates = 2,
  diseaseDxIcdMap = phers::diseaseDxIcdMap) {
  dx_status = entry_date = uniq_dates = . = NULL

  checkDemos(demos)
  checkIcdOccurrences(
    icdOccurrences, cols = c('person_id', 'icd', 'flag', 'entry_date'))
  checkDate(icdOccurrences$entry_date)
  assertCount(minUniqDates, positive = TRUE)

  assertDataTable(diseaseDxIcdMap)
  assertNames(colnames(diseaseDxIcdMap),
              must.include = c('disease_id', 'icd', 'flag'),
              disjunct.from = 'person_id')
  assertCharacter(diseaseDxIcdMap$icd)

  dxIcd = merge(
    icdOccurrences,
    diseaseDxIcdMap[, c('disease_id', 'icd', 'flag')],
    by = c('icd', 'flag'))
  dxIcd = dxIcd[, .(uniq_dates = uniqueN(entry_date)), by = c('person_id', 'disease_id')]
  cases = dxIcd[uniq_dates >= minUniqDates, !'uniq_dates']
  cases[, dx_status := 1]

  exclude = dxIcd[uniq_dates < minUniqDates & uniq_dates > 0, !'uniq_dates']
  cohort = fsetdiff(
    CJ(person_id = demos$person_id,
       disease_id = unique(diseaseDxIcdMap$disease_id)),
    exclude)

  dxStatus = merge(cohort, cases, by = c('person_id', 'disease_id'), all.x = TRUE)
  dxStatus[is.na(dx_status), dx_status := 0]
  return(dxStatus[])}
