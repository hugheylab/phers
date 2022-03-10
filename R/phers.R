#' @import checkmate
#' @import data.table
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom stats confint glm as.formula update.formula
#' @importFrom iterators iter
NULL


#' Map ICD code occurrences to phecode occurrences
#'
#' This function takes a data table of patient ICD codes and maps them to
#' phecodes.
#'
#' @param icdOccurrences A data.table of occurrences of ICD codes for each
#'   person in the cohort. Must have columns `person_id`, `icd`, and `flag`.
#' @param icdPhecodeMap A data.table containing the mapping between ICD codes
#'   and phecodes. Must have columns `icd`, `phecode`, and `flag`. By default uses
#'   the mapping included in this package.
#' @param DxIcd character vector of diagnostic ICD codes to remove
#'
#' @return A data.table of phecode occurrences for each person.
#'
#' @export
getPhecodeOccurrences = function(
  icdOccurrences, icdPhecodeMap = phers::icdPhecodeMap, DxIcd = NULL) {

  assertDataTable(icdOccurrences)
  assertNames(colnames(icdOccurrences),
              must.include = c('person_id', 'icd', 'flag'),
              disjunct.from = 'phecode')
  assertCharacter(icdOccurrences$icd)

  assertDataTable(icdPhecodeMap)
  assertNames(colnames(icdPhecodeMap),
              permutation.of = c('phecode', 'icd', 'flag'))
  assertCharacter(icdPhecodeMap$icd)
  assertCharacter(icdPhecodeMap$phecode)
  assert(anyDuplicated(icdPhecodeMap) == 0)

  pheOccs = merge(icdOccurrences, icdPhecodeMap, by = c('icd', 'flag'))
  pheOccs = pheOccs[, !c('icd', 'flag')]
  return(pheOccs)}


#' Calculate phecode-specific weights for phenotype risk scores
#'
#' The weights correspond to the -log10 prevalences in the cohort.
#'
#' @param demos A data.table having one row per person in the cohort. Must have
#'   a column `person_id`.
#' @param phecodeOccurrences A data.table of occurrences of phecodes for each
#'   person in the cohort. Must have columns `person_id` and `phecode`.
#'
#' @return A data.table containing the prevalence (`prev`) and weight (`w`) for
#'   each phecode.
#'
#' @export
getWeights = function(demos, phecodeOccurrences) {
  phecode = person_id = . = prev = w = NULL

  checkDemos(demos)
  checkPhecodeOccurrences(phecodeOccurrences, demos)

  weights = phecodeOccurrences[, .(prev = uniqueN(person_id) / nrow(demos)),
                               by = phecode]
  weights[, w := -log10(prev)]
  return(weights[])}


#' Calculate phenotype risk scores
#'
#' A person's phenotype risk score for a particular disease corresponds to the
#' sum of the weights of the disease-relevant phecodes that the person has
#' received.
#'
#' @param demos A data.table having one row per person in the cohort. Must have
#'   a column `person_id`.
#' @param phecodeOccurrences A data.table of occurrences of phecodes for each
#'   person in the cohort. Must have columns `person_id` and `phecode`.
#' @param weights A data.table where each row is a phecode and the weight
#'   corresponding to it. The columns are `phecode` and `w`.
#' @param diseasePhecodeMap A data.table containing the mapping between
#'   diseases and phecodes. Must have columns `disease_id` and `phecode`.
#'
#' @return A data.table containing the phenotype risk score for each person for
#'   each disease.
#'
#' @export
getScores = function(demos, phecodeOccurrences, weights, diseasePhecodeMap) {
  person_id = phecode = disease_id = w = score = . = NULL

  checkDemos(demos)
  checkPhecodeOccurrences(phecodeOccurrences, demos)
  checkWeights(weights)
  checkDiseasePhecodeMap(diseasePhecodeMap)

  pheOccWei = merge(unique(phecodeOccurrences[, .(person_id, phecode)]),
                    weights, by = 'phecode')

  rBig = merge(pheOccWei, diseasePhecodeMap, by = 'phecode',
               allow.cartesian = TRUE)
  rSum = rBig[, .(score = sum(w)), by = .(person_id, disease_id)]
  r = merge(CJ(person_id = demos$person_id,
               disease_id = unique(diseasePhecodeMap$disease_id)),
            rSum, by = c('person_id', 'disease_id'), all.x = TRUE)
  r[is.na(score), score := 0]
  return(r[])}
