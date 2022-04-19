#' Sample diagnostic code dataset
#'
#' This table contains a sample dataset of patient diagnostic codes
#' (ICD-9-CM and ICD-10-CM).
#'
#' @format A data table with 141 rows and the 4 following columns:
#'
#' * `person_id`: Character vector of the identifier for each
#'    person
#' * `icd`: Character vector containing the ICD codes recorded
#'    for each person
#' * `flag`: Numeric vector representing the vocabulary of the
#'    ICD code (**9**: ICD-9-CM,  **10**: ICD-10-CM)
#' * `entry_date`: vector of type `Date` indicating the date ICD code was
#'                 recorded.
#'
#' @seealso [getPhecodeOccurrences()], [getWeights()], [getScores()]
'icdSample'


#' Sample demographics dataset
#'
#' This table contains a sample dataset of patient demographics information.
#'
#' @format A data table with 50 rows and the 2 following columns:
#'
#' * `person_id`: Character vector of the identifier for each
#'      person in the cohort
#' * `sex`: Character vector indicating biological sex
#'
#' @seealso [getWeights()], [getScores()]
'demoSample'


#' pre-calculated weights
#'
#' This table provides weights for 1865 phecodes calculated using data from
#' Vanderbilt University Medical Center Synthetic Derivative (SD) and ICD-phecode
#' map Version 1.2
#'
#' @format A data table with 1865 rows and the 3 following columns:
#'
#' * `phecode`: Character vector of phecodes
#' * `prev`: Prevelance of each phecode within SD
#' * `w`: Weight calculated for each phecode using the PheRS model
#'
#'
#' @seealso [getWeights()]
'preCalcWeights'


#' Mapping of diseases and diagnostic ICD codes
#'
#' This table provides mapping between 27 Mendelian diseases and ICD9 and ICD10
#' codes that indicate diagnosis with each disease.
#'
#' @format A data table with 81 rows and the 3 following columns:
#'
#' * `disease_id`: Numeric vector of OMIM disease identifiers
#' * `disease_name`: Character vector of disease names
#' * `icd`: Character vector representing the diagnostic ICD code
#' * `flag`: Integer vector representing the vocabulary of the ICD code
#'    (**9**: ICD-9-CM,  **10**: ICD-10-CM)
#' * `icd_name`: Character vector containing the description of each ICD code
#'
#'
#' @seealso [getPhecodeOccurrences(), getDxStatus()]
'diseaseDxIcdMap'


#' Mapping of Mendelian diseases and their clinical features
#'
#' This table provides mapping between Mendelian diseases and the clinical
#' features they are linked to using annotations from
#' Online Mendelian Inheritance in Man (OMIM).
#'
#' @format A data table with 75,084 rows and the 6 following columns:
#'
#' * `disease_id`: Numeric vector of OMIM disease identifiers corresponding to
#'    the disease being annotated
#' * `disease_name`: Character vector of disease name
#' * `hpo_term_id`: Character vector of the HPO identifier corresponding to the
#'    phenotype mapped to `disease_id`
#' * `hpo_term_name`: Character vector containing the description of `hpo_term_id`
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
#' @format A data table with 4189 rows and the 4 following columns:
#'
#' * `hpo_term_id`: Character vector representing the HPO term ID to be mapped
#' * `hpo_term_name`: Character vector containing the description of `hpo_term_id`
#' * `phecode`: Character vector representing the phecode mapping
#' * `phecode_name`: Character vector containing the description of each phecode
#'
#' @seealso [mapDiseaseToPhecode()]
'hpoPhecodeMap'


#' Mapping of ICD codes and phecodes
#'
#' This table provides mapping between International Classification of
#' Diseases 9th and 10th revisions (ICD-9-CM and ICD-10-CM) and phecodes
#' (Version 1.2).
#'
#' @format A data table with 156,641 rows and the 3 following columns:
#'
#' * `icd`: Character vector of the ICD code to be mapped
#' * `flag`: Integer vector representing the vocabulary of the ICD code
#'   (**9**: ICD-9-CM,  **10**: ICD-10-CM)
#' * `icd_name`: Character vector containing the description of each ICD code
#' * `phecode`: Character vector of the phecode mapping
#' * `phecode_name`: Character vector containing the description of each phecode
#'
#' @source <https://phewascatalog.org/phecodes>
#'
#' @seealso [getPhecodeOccurrences()]
'icdPhecodeMap'


#' Map diseases entities to phecodes using HPO terms
#'
#' This function takes a list of disease identifiers and returns the
#' clinical features mapped to them as phecodes.
#'
#' @param diseaseHpoMap A data.table containing the mapping between disease
#'   entities in `diseaseIds` and HPO terms. Must have columns `disease_id` and
#'   `term_id`. By default uses the map included in this package.
#' @param hpoPhecodeMap A data.table containing the mapping between HPO terms
#'   and phecodes. Must have columns `term_id` and `phecode`. By default uses the
#'   map included in this package.
#'
#' @return A data.table containing the mapping between diseases and phecodes.
#'   The columns are `disease_id` and `phecode`.
#'
#' @eval example1()
#'
#' @export
mapDiseaseToPhecode = function(
  diseaseHpoMap = phers::diseaseHpoMap,
  hpoPhecodeMap = phers::hpoPhecodeMap) {

  assertDataTable(diseaseHpoMap)
  assertNames(
    colnames(diseaseHpoMap), must.include = c('disease_id', 'hpo_term_id'))
  assertCharacter(diseaseHpoMap$hpo_term_id)

  assertDataTable(hpoPhecodeMap)
  assertNames(
    colnames(hpoPhecodeMap), must.include = c('hpo_term_id', 'phecode'))
  assertCharacter(hpoPhecodeMap$hpo_term_id)
  assertCharacter(hpoPhecodeMap$phecode, any.missing = FALSE, min.chars = 1)

  diseasePhecodeMap = merge(diseaseHpoMap, hpoPhecodeMap, by = 'hpo_term_id')
  diseasePhecodeMap = unique(diseasePhecodeMap[, c('disease_id', 'phecode')])

  return(diseasePhecodeMap)}
