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



#' Sample genotype dataset
#'
#' This table contains a sample dataset of genotypes.
#'
#' @format A data table with 5 rows and the 805 columns. The first column is
#'   `person_id` and the rest are variant IDs.
#'
'genoSample'



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



#' Mapping of genes and variants
#'
#' This table provides mapping between genes and genetic variants using
#' ANNOVAR annotations
#'
#' @format A data table with 65,808 rows and the 3 following columns:
#'
#' \describe{
#'   \item{\code{gene}}{Character vector of gene names}
#'   \item{\code{vid}}{Character vector of variant IDs based on position on
#'   the chromosome}
#'   \item{\code{rsid}}{Character vector of dbSNP Reference SNP numbers (rs)}
#'}
#'
#' @source <https://annovar.openbioinformatics.org/en/latest/>
#'
#' @seealso [genotypeAssociation()]
'geneVarMap'



#' Mendelian disease annotation
#'
#' This table provides annotations for Mendelian diseases
#'
#' @format A data table with 63 rows and the 3 following columns:
#'
#' \describe{
#'   \item{\code{db_name}}{Character vector indicating the reference database
#'   associated with `disease_id`}
#'   \item{\code{disease_id}}{Numeric vector of disease identifiers}
#'   \item{\code{disease_name}}{Character vector of disease names}
#'   \item{\code{gene}}{Character vector containing the gene known to cause
#'   the disease}
#'}
#'
#' @source <>
#'
#' @seealso [genotypeAssociation()]
'diseaseInfo'



#' Diagnostic phecodes
#'
#' This table provides phecodes that represent diagnosis with a Mendelian disease.
#'
#' @format A data table with 1 row and the 2 following columns:
#'
#' \describe{
#'   \item{\code{disease_id}}{Numeric vector of disease identifiers}
#'   \item{\code{dx_phecode}}{Character vector of the phecode that represents
#'   diagnosis with `disease_id`}
#'}
#'
#' @source <>
#'
#' @seealso [runPositiveControl()]
'diseaseDxPhecode'
