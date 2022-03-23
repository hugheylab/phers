#' Identify Mendelian disease cases and controls
#'
#' This function Identifies cases and controls of Mendelian diseases.
#'
#' @param demos A data.table having one row per person in the cohort. Must have
#'   a column `person_id`.
#' @param icdOccurrences A data.table of occurrences of ICD codes for each
#'   person in the cohort. Must have columns `person_id`, `icd`, and `flag`.
#' @param diseaseDxIcdMap A data.table containing mapping between diseases and
#'   the icd codes that represent being diagnosed with them. Must have columns
#'   `disease_id`, `icd` and `flag`.
#'
#' @return A data.table with cases and controls of each disease in
#'   `diseaseDxIcdMap`. The columns are `person_id`, `disease_id`,
#'   and `dx_status`.
#'
#' @export
getDxStatus = function(
  demos, icdOccurrences, diseaseDxIcdMap = phers::diseaseDxIcdMap) {
  dx_status = NULL

  checkDemos(demos)
  checkIcdOccurrences(icdOccurrences)

  assertDataTable(diseaseDxIcdMap)
  assertNames(colnames(diseaseDxIcdMap),
              must.include = c('disease_id', 'icd', 'flag'),
              disjunct.from = 'person_id')
  assertCharacter(diseaseDxIcdMap$icd)

  cases = merge(icdOccurrences, diseaseDxIcdMap, by = c('icd', 'flag'))
  cases = unique(cases[, c('person_id', 'disease_id')])
  cases[, dx_status := 1]

  dxStatus = merge(CJ(person_id = demos$person_id,
           disease_id = unique(diseaseDxIcdMap$disease_id)),
        cases, by = c('person_id', 'disease_id'), all.x = TRUE)
  dxStatus[is.na(dx_status), dx_status := 0]
  return(dxStatus[])}
