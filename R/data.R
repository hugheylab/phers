#' Sample diagnostic code dataset
#'
#' This table contains a sample dataset of patient diagnostic codes
#' (ICD-9-CM and ICD-10-CM).
#'
#' @format A data table with 9 rows and the 3 following columns:
#'
#' \describe{
#'   \item{\code{person_id}}{Character vector of the identifier for each
#'   person}
#'   \item{\code{icd}}{Character vector containing the ICD codes recorded
#'   for each person}
#'   \item{\code{flag}}{Numeric vector representing the vocabulary of the
#'   ICD code (**9**: ICD-9-CM,  **10**: ICD-10-CM)}
#'}
#'
#' @seealso [getWeights()], [getPheRS()]
'icdSample'



#' Sample demographics dataset
#'
#' This table contains a sample dataset of patient demographics information.
#'
#' @format A data table with 5 rows and the 5 following columns:
#'
#' \describe{
#'   \item{\code{person_id}}{Character vector of the identifier for each
#'   person in the cohort}
#'   \item{\code{sex}}{Character vector indicating biological sex
#'   ('M': male, 'F': female)}
#'   \item{\code{uniq_age}}{Numeric vector containing the number of unique
#'    years with ICD codes}
#'   \item{\code{first_age}}{Numeric vector containing the age (in days) when
#'    the person first received an ICD code}
#'   \item{\code{last_age}}{Numeric vector containing the age (in days) when
#'    the person last received an ICD code}
#'}
#'
#' @seealso [getWeights()], [getPheRS()]
'demoSample'




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
#'   \item{\code{disease_name}}{Character vector containing the name of the
#'   disease associated with `disease_id`}
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
'diseaseHPOMap'



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
'HPOPhecodeMap'



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
#' @seealso [mapICDToPhecode()]
'ICDPhecodeMap'

