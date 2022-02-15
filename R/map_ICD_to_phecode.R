#' @import checkmate
#' @importFrom data.table data.table := set uniqueN .N
NULL

#' Map ICD codes to phecodes
#'
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

