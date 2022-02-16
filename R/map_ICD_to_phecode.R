#' @import checkmate
#' @importFrom data.table data.table := set uniqueN .N
NULL

#' Map ICD codes to phecodes
#'
#' This function takes a data table of patient ICD codes and maps them to
#' phecodes.
#'
#' @param ICDs A data.table containing ICD codes recorded for each person.
#'   The columns are `person_id`, `icd`, and `flag`.
#' @param ICDPhecodeMap A data.table containing the mapping between ICD codes
#'   and phecodes. The columns are `icd`, `phecode`, `flag`. By default uses
#'   the mapping included in this package.
#'
#' @return A data.table of phecodes corresponding to each person. The columns
#'   are `person_id` and `phecode`.
#'
#' @export
mapICDToPhecode = function(ICDs, ICDPhecodeMap = phers::ICDPhecodeMap) {
  person_id = phecode = `.` = NULL

  assertDataTable(ICDs)
  assertNames(colnames(ICDs), must.include = c('person_id', 'icd', 'flag'))
  assertCharacter(ICDs$icd)

  phecodes = merge(ICDs, ICDPhecodeMap,
                   by=c('icd', 'flag'))[,.(person_id, phecode)]

return(phecodes)}

