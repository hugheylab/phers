#' @import checkmate
#' @importFrom data.table data.table := set uniqueN .N
NULL


#' Map diseases IDs to phecodes using HPO terms
#'
#'
#' @return A data.table containing the mapping between diseases and
#'   their phenotypes. The columns are `disease_ID`, `phecode`.
#'
#' @export
mapDiseaseToPhecode = function(diseaseIDs, dbName = 'OMIM',
                               diseaseHPOMap = phers::diseaseHPOMap,
                               HPOPhecodeMap = phers::HPOPhecodeMap) {

  db_name = phecode = disease_id = NULL

  assertString(dbName)
  assertNames(dbName, subset.of = c('DECIPHER', 'OMIM', 'ORPHA'))
  # change error message
  assertChoice(as.numeric(diseaseIDs), unique(diseaseHPOMap$disease_id))

  diseaseHPOMapSub = diseaseHPOMap[db_name == dbName][disease_id %in% diseaseIDs]
  diseasePhecodeMap = merge(diseaseHPOMapSub,
                           HPOPhecodeMap[,!c('hpo_term_name')], by = 'term_id')
  diseasePhecodeMap = unique(diseasePhecodeMap[phecode!=''][, c('disease_id',
                                                               'phecode')])
return(diseasePhecodeMap)}



