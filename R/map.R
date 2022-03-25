#' Sample diagnostic code dataset
#'
#' This table contains a sample dataset of patient diagnostic codes
#' (ICD-9-CM and ICD-10-CM).
#'
#' @format A data table with 9 rows and the 3 following columns:
#'
#' * `person_id`: Character vector of the identifier for each
#'    person
#' * `icd`: Character vector containing the ICD codes recorded
#'    for each person
#' * `flag`: Numeric vector representing the vocabulary of the
#'    ICD code (**9**: ICD-9-CM,  **10**: ICD-10-CM)
#'
#' @seealso [getPhecodeOccurrences()], [getWeights()], [getScores()]
'icdSample'


#' Sample demographics dataset
#'
#' This table contains a sample dataset of patient demographics information.
#'
#' @format A data table with 5 rows and the 2 following columns:
#'
#' * `person_id`: Character vector of the identifier for each
#'      person in the cohort
#' * `sex`: Character vector indicating biological sex
#'
#' @seealso [getWeights()], [getScores()]
'demoSample'


#' Sample genotype dataset
#'
#' This table contains a sample dataset of genotypes.
#'
#' @format A data table with 5 rows and the 21 columns. The first column is
#'   `person_id` and the rest are variant IDs.
#'
'genoSample'


#' pre-calculated weights
#'
#' This table provides weights for 1868 phecodes calculated using data from
#' Vanderbilt University Medical Center Synthetic Derivative (SD)
#'
#' @format A data table with 1868 rows and the 3 following columns:
#'
#' * `phecode`: Character vector of phecodes
#' * `prev`: Prevelance of each phecode within SD
#' * `w`: Weight calculated for each phecode using the PheRS model
#'
#' @source <>
#'
#' @seealso [getWeights()]
'preCalcWeights'


#' Mapping of diseases and diagnostic ICD codes
#'
#' This table provides mapping between diseases and ICD9 and ICD10 codes that
#' represent being diagnosed with them.
#'
#' @format A data table with 63 rows and the 3 following columns:
#'
#' * `disease_id`: Numeric vector of OMIM disease identifiers
#' * `disease_name`: Character vector of disease names
#' * `icd`: Character vector representing the diagnostic ICD code
#' * `icd_string`: Character vector containing the description of each icd code
#' * `flag`: Integer vector representing the vocabulary of the ICD code
#'    (**9**: ICD-9-CM,  **10**: ICD-10-CM)
#'
#' @source <>
#'
#' @seealso []
'diseaseDxIcdMap'


#' Mapping of disease entities and their clinical features
#'
#' This table provides mapping between disease entities and the clinical
#' features they are linked to using annotations from the OMIM database.
#'
#' @format A data table with 166,143 rows and the 6 following columns:
#'
#' * `disease_id`: Numeric vector of OMIM disease identifiers corresponding to
#'    the disease being annotated
#' * `disease_name`: Character vector of disease name
#' * `hpo_id`: Character vector of the HPO identifier for the
#'    phenotype linked to `disease_id`
#' * `term_id`: Numeric vector containing the numeric portion of `hpo_id`
#' * `hpo_term_name`: Character vector containing the description
#'    corresponding to `hpo_id`
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
#' * `term_id`: Numeric vector representing the HPO term ID to be mapped
#' * `hpo_term_name`: Character vector containing the description of each
#'     HPO term ID
#' * `phecode`: Character vector representing the phecode mapping
#' * `phecode_string`: Character vector containing the description of each
#'     phecode
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
#' * `icd`: Character vector representing the ICD code to be mapped
#' * `phecode`: Character vector representing the phecode mapping
#' * `flag`: Integer vector representing the vocabulary of the ICD code
#'   (**9**: ICD-9-CM,  **10**: ICD-10-CM)
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
    colnames(diseaseHpoMap), must.include = c('disease_id', 'term_id'))
  assertNumeric(diseaseHpoMap$term_id)

  assertDataTable(hpoPhecodeMap)
  assertNames(
    colnames(hpoPhecodeMap), must.include = c('term_id', 'phecode'))
  assertNumeric(hpoPhecodeMap$term_id)
  assertCharacter(hpoPhecodeMap$phecode, any.missing = FALSE, min.chars = 1)

  diseasePhecodeMap = merge(diseaseHpoMap, hpoPhecodeMap, by = 'term_id')
  diseasePhecodeMap = unique(diseasePhecodeMap[, c('disease_id', 'phecode')])

  return(diseasePhecodeMap)}
