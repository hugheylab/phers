#' @import checkmate
#' @importFrom data.table data.table := set uniqueN
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom stats lm rstudent
NULL

#' Calculate phenotype weights
#'
#' Calculate the population prevalence and weight for each phenotype.
#' These weights will be used in the calculation of the phenotype risk score.
#'
#' @param demos A data.table containing demographic information for each person
#'   in the population. The columns are `person_id`.
#' @param phecodes A data.table containing phenotypes stored as phecodes
#'   for each person. The columns are `person_id` and `phecode`.
#'
#' @return A data.table where each row is a phecode and the population
#'   prevalence and weight corresponding to it.
#'   The columns are `phecode`, `prev`, `w`.
#'
#' @export
getWeights = function(demos, phecodes) {
  phecode = person_id = `.` = NULL

  assertDataTable(demos)
  assertNames(colnames(demos), must.include = c('person_id'))

  assertDataTable(phecodes)
  assertNames(colnames(phecodes), must.include = c('person_id', 'phecode'))
  assertCharacter(phecodes$phecode)

  # better error message?
  assertNumber(uniqueN(phecodes$person_id),
               upper = uniqueN(demos$person_id))


  npop = uniqueN(demos$person_id)
  weights = phecodes[, .(prev = uniqueN(person_id) / npop,
                         w = -log(uniqueN(person_id) / npop)), by = phecode]

return(weights)}


#' Calculate the Phenotype Risk Score
#'
#' Calculate the phenotype risk score (PheRS) for a group of individuals and
#' diseases
#'
#' @param demos A data.table containing demographic information for each person
#'   in the population. The columns are `person_id`, `sex`, `uniq_age`,
#'   `first_age`, and `last_age`. The age columns specify the number of
#'   unique years with ICD codes, the age of the individual at first ICD code,
#'   and the age of the individual at last ICD code.
#' @param phecodes A data.table containing phenotypes stored as phecodes
#'   for each person. The columns are `person_id` and `phecode`.
#' @param weights A data.table where each row is a phecode and the weight
#'   corresponding to it. The columns are `phecode` and `w`.
#' @param diseasePhecodeMap A data.table containing the mapping between
#'   diseases and phecodes. The columns are `disease_id` and `phecode`.
#'
#' @return A data.table of raw and residualized phenotype risk scores
#'   with one row per person per disease. The columns are `person_id`,
#'   `disease_id`, `phers`, `rphers`.
#'
#' @export
getPheRS = function(demos, phecodes, weights, diseasePhecodeMap){
  person_id = disease_id = ID = w = rphers = `.` = NULL

  assertDataTable(demos)
  assertNames(colnames(demos), must.include = c('person_id'))

  assertDataTable(phecodes)
  assertNames(colnames(phecodes), must.include = c('person_id', 'phecode'))
  assertCharacter(phecodes$phecode)

  assertDataTable(weights)
  assertNames(colnames(weights), must.include = c('phecode', 'w'))
  assertCharacter(weights$phecode)
  assertNumeric(weights$w)

  assertDataTable(diseasePhecodeMap)
  assertNames(colnames(diseasePhecodeMap),
              must.include = c('disease_id', 'phecode'))
  assertCharacter(diseasePhecodeMap$phecode)

  demos[, person_id := as.character(person_id)]
  phecodes[, person_id := as.character(person_id)]

  phecodesW = merge(phecodes, weights, by = 'phecode')

  phersAll = foreach (ID = unique(diseasePhecodeMap$disease_id),
                      .combine = rbind) %dopar% {
    phecodesWSub = merge(
      phecodesW, diseasePhecodeMap[disease_id == ID],
      by = 'phecode', allow.cartesian = TRUE)
    phers = phecodesWSub[, .(phers = sum(w)), by = 'person_id']

    phers = merge(demos, phers, by = 'person_id', all.x = TRUE)
    phers[is.na(phers), phers := 0]
    phers[, disease_id := ID]

    # rphersFit = lm(phers ~ sex + uniq_age + first_age + last_age,
    #               data = phers)
    # phers[, rphers := rstudent(rphersFit)]
    # phers[, .(person_id, disease_id, phers, rphers)]

    phers[, .(person_id, disease_id, phers)]}

return(phersAll)}





