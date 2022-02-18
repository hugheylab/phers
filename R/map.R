#' Mapping of disease entities and their clinical features
#'
#' This table provides mapping between disease entities and the clinical
#' features they are linked to using annotations from multiple
#' databases (OMIM, Orphanet, and Decipher).
#'
#' @format A data table with 166,143 rows and the 6 following columns:
#'
#' \describe{
#'   \item{\code{db_name}}{Character vector indicating the reference database
#'    used for each annotation ('OMIM', 'DECIPHER', 'ORPHA')}
#'   \item{\code{disease_id}}{Numeric vector of the database identifier
#'   corresponding to the disease being annotated}
#'   \item{\code{disease_name}}{Character vector of disease name}
#'   \item{\code{hpo_id}}{Character vector of the HPO identifier for the
#'    phenotype linked to `disease_id`}
#'   \item{\code{term_id}}{Numeric vector containing the numeric portion of
#'    `hpo_id`}
#'   \item{\code{hpo_term_name}}{Character vector containing the description
#'   corresponding to `hpo_id`}
#'}
#'
#' @source <https://hpo.jax.org/app/download/annotation>
#'
#' @seealso [mapDiseaseToPhecode()]
'diseaseHpoMap'


#' Mapping of HPO terms and phecodes
#'
#' This table provides mapping between Human Phenotype Ontology (HPO)
#' annotations and phecodes (groupings of ICD-9-CM and ICD-10-CM)
#'
#' @format A data table with 14,160 rows and the 4 following columns:
#'
#' \describe{
#'   \item{\code{term_id}}{Numeric vector representing the HPO term ID to be
#'   mapped}
#'   \item{\code{hpo_term_name}}{Character vector containing the description
#'   corresponding to each HPO term ID}
#'   \item{\code{phecode}}{Character vector representing the phecode mapping}
#'   \item{\code{phecode_string}}{Character vector containing the description
#'    corresponding to each phecode}
#'}
#'
#' @source <https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6857501/#sup1>
#'
#' @seealso [mapDiseaseToPhecode()]
'hpoPhecodeMap'


#' Mapping of ICD and phecodes
#'
#' This table provides mapping between International Classification of
#' Diseases 9th and 10th revisions (ICD-9-CM and ICD-10-CM) and phecodes
#' (groupings of ICD codes).
#'
#' @format A data table with 156,641 rows and the 3 following columns:
#'
#' \describe{
#'   \item{\code{icd}}{Character vector representing the ICD code to be mapped}
#'   \item{\code{phecode}}{Character vector representing the phecode mapping}
#'   \item{\code{flag}}{Integer vector representing the vocabulary of the
#'   ICD code (**9**: ICD-9-CM,  **10**: ICD-10-CM)}
#'}
#'
#' @source <https://phewascatalog.org/phecodes_v1_1>
#'
#' @seealso [getPhecodeOccurrences()]
'icdPhecodeMap'


#' Map diseases entities to phecodes using HPO terms
#'
#' This function takes a list of disease identifiers and returns the
#' clinical features mapped to them as phecodes.
#'
#' @param diseaseIDs A numeric vector of disease identifiers to be mapped
#' @param dbName A character string of the name of database used for mapping.
#'   One of either 'OMIM', 'ORPHA', and 'DECIPHER'.
#' @param diseaseHpoMap A data.table containing the mapping between disease
#'   entities in `diseaseIDs` and HPO terms. The columns are `db_name`,
#'   `disease_id`, `term_id`. By default uses the map included in this package.
#' @param hpoPhecodeMap A data.table containing the mapping between HPO terms
#'   and phecodes. The columns are `term_id` and `phecode`. By default uses the
#'   map included in this package.
#'
#' @return A data.table containing the mapping between diseases and phecodes.
#'   The columns are `disease_id` and `phecode`.
#'
#' @export
mapDiseaseToPhecode = function(
  diseaseIDs = unique(phers::diseaseHpoMap[db_name == 'OMIM']$disease_id),
  dbName = 'OMIM', diseaseHpoMap = phers::diseaseHpoMap,
  hpoPhecodeMap = phers::hpoPhecodeMap) {

  db_name = phecode = disease_id = NULL

  assertString(dbName)
  assertNames(dbName, subset.of = unique(diseaseHpoMap$db_name))
  # change error message
  assertChoice(
    as.numeric(diseaseIDs), unique(diseaseHpoMap[db_name == dbName]$disease_id))

  assertDataTable(diseaseHpoMap)
  assertNames(
    colnames(diseaseHpoMap), must.include = c('db_name', 'disease_id', 'term_id'))
  assertNumeric(diseaseHpoMap$term_id)

  assertDataTable(hpoPhecodeMap)
  assertNames(
    colnames(hpoPhecodeMap), must.include = c('term_id', 'phecode'))
  assertNumeric(hpoPhecodeMap$term_id)
  assertCharacter(hpoPhecodeMap$phecode)

  diseaseHpoMapSub = diseaseHpoMap[db_name == dbName & disease_id %in% diseaseIDs]
  diseasePhecodeMap = merge(diseaseHpoMapSub, hpoPhecodeMap, by = 'term_id')
  diseasePhecodeMap = unique(
    diseasePhecodeMap[phecode != ''][, c('disease_id', 'phecode')])

  return(diseasePhecodeMap)}
