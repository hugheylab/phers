#' @import checkmate
#' @importFrom data.table data.table := set uniqueN .N
NULL

#' Map ICD codes to phecodes
#'
#'
#' @export
mapICDToPhecode = function(ICDs, ICDPhecodeMap = phers::ICDPhecodeMap) {
  person_ID = phecode = `.` = NULL

  assertDataTable(ICDs)
  assertNames(colnames(ICDs), must.include = c('person_ID', 'ICD', 'flag'))
  assertCharacter(ICDs$ICD)

  phecodes = merge(ICDs, ICDPhecodeMap,
                   by=c('ICD', 'flag'))[,.(person_ID, phecode)]

return(phecodes)}

