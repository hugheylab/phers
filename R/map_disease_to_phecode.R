#' Map diseases entities to phecodes using HPO terms
#'
#' This function takes a list of disease identifiers and returns the
#' clinical features mapped to them as phecodes.
#'
#' @param diseaseIDs A numeric vector of disease identifiers to be mapped
#' @param dbName A character string of the name of database used for mapping.
#'   One of either 'OMIM', 'ORPHA', and 'DECIPHER'.
#' @param diseaseHPOMap A data.table containing the mapping between disease
#'   entities in `diseaseIDs` and HPO terms. The columns are `db_name`,
#'   `disease_id`, `term_id`. By default uses the map included in this package.
#' @param HPOPhecodeMap A data.table containing the mapping between HPO terms
#'   and phecodes. The columns are `term_id` and `phecode`. By default uses the
#'   map included in this package.
#'
#' @return A data.table containing the mapping between diseases and phecodes.
#'   The columns are `disease_id` and `phecode`.
#'
#' @export
mapDiseaseToPhecode = function(
  diseaseIDs = unique(phers::diseaseHPOMap[db_name == 'OMIM']$disease_id),
  dbName = 'OMIM', diseaseHPOMap = phers::diseaseHPOMap,
  HPOPhecodeMap = phers::HPOPhecodeMap) {

  db_name = phecode = disease_id = NULL

  assertString(dbName)
  assertNames(dbName, subset.of = unique(diseaseHPOMap$db_name))
  # change error message
  assertChoice(
    as.numeric(diseaseIDs), unique(diseaseHPOMap[db_name == dbName]$disease_id))

  assertDataTable(diseaseHPOMap)
  assertNames(
    colnames(diseaseHPOMap), must.include = c('db_name', 'disease_id', 'term_id'))
  assertNumeric(diseaseHPOMap$term_id)

  assertDataTable(HPOPhecodeMap)
  assertNames(
    colnames(HPOPhecodeMap), must.include = c('term_id', 'phecode'))
  assertNumeric(HPOPhecodeMap$term_id)
  assertCharacter(HPOPhecodeMap$phecode)


  diseaseHPOMapSub = diseaseHPOMap[db_name == dbName & disease_id %in% diseaseIDs]
  diseasePhecodeMap = merge(diseaseHPOMapSub, HPOPhecodeMap, by = 'term_id')
  diseasePhecodeMap = unique(
    diseasePhecodeMap[phecode != ''][, c('disease_id', 'phecode')])

return(diseasePhecodeMap)}



