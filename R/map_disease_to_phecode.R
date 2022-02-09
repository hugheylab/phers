#' @import checkmate
#' @importFrom data.table data.table := set uniqueN .N
NULL


#' Map diseases IDs to phecodes using HPO terms
#'
#'
#' @export
mapDiseaseToPhecode = function(diseaseIDs, dbName = 'OMIM',
                               diseaseHPOMap = phers::diseaseHPOMap,
                               HPOPhecodeMap = phers::HPOPhecodeMap) {
  db_name = phecode = disease_ID = NULL

  diseaseHPOMapSub = diseaseHPOMap[db_name == dbName][disease_ID %in% diseaseIDs]
  diseasePhecodeMap =merge(diseaseHPOMapSub,
                           HPOPhecodeMap[,!c('HPO_term_name')], by = 'term_ID')
  diseasePhecodeMap = unique(diseasePhecodeMap[phecode!=''][, c('disease_ID',
                                                               'phecode')])
return(diseasePhecodeMap)}


'diseaseHPOMap'
'HPOPhecodeMap'
