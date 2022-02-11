#' @import checkmate
#' @importFrom data.table data.table := set uniqueN .N
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom stats lm rstudent
NULL

#' Calculate phenotype weights
#'
#' Calculate the population prevalence and weight for each phenotype.
#' These weights will be used in the calculation of the phenotype risk score.
#'
#' @param demos A data.table containing demographic information for each person
#'   in the population. The columns are `person_ID`.
#' @param phecodes A data.table containing phenotypes stored as phecodes
#'   for each person. The columns are `person_ID` and `phecode`.
#'
#' @return A data.table where each row is a phecode and the population
#'   prevalence and weight corresponding to it.
#'   The columns are `phecode`, `prev`, `w`.
#'
#' @export
calcWeights = function(demos, phecodes) {
  phecode = person_ID = `.` = NULL

  assertDataTable(demos)
  assertNames(colnames(demos), must.include = c('person_ID'))

  assertDataTable(phecodes)
  assertNames(colnames(phecodes), must.include = c('person_ID', 'phecode'))
  assertCharacter(phecodes$phecode)

  # better error message?
  assertNumber(uniqueN(phecodes$person_ID),
               upper = uniqueN(demos$person_ID))


  npop = uniqueN(demos$person_ID)
  weights = phecodes[, .(prev = uniqueN(person_ID) / npop,
                         w = -log(uniqueN(person_ID) / npop)), by = phecode]

return(weights)}


#' Calculate the Phenotype Risk Score
#'
#' Calculate the phenotype risk score (PheRS)
#' for a group of individuals and diseases
#'
#'
#' @param demos A data.table containing demographic information for each person
#'   in the population. The columns are `person_ID`, `sex`, `uniq_age`,
#'   `first_age`, and `last_age`. The age columns specify the number of
#'   unique years with ICD codes, the age of the individual at first ICD code,
#'   and the age of the individual at last ICD code.
#' @param phecodes A data.table containing phenotypes stored as phecodes
#'   for each person. The columns are `person_ID` and `phecode`.
#' @param weights A data.table where each row is a phecode and the weight
#'   corresponding to it. The columns are `phecode` and `w`.
#' @param diseaseIDs vector of disease IDs to calculate PheRS for.
#'
#' @return A data.table of raw and residualized phenotype risk scores
#'   with one row per person per disease. The columns are `person_ID`,
#'   `disease_ID`, `phers`, `rphers`.
#'
#' @export
calcPheRS = function(demos, phecodes, weights, diseaseIDs, dbName = 'OMIM'){
  person_ID = disease_ID = ID = w = rphers = `.` = NULL

  assertDataTable(demos)
  assertNames(colnames(demos), must.include = c('person_ID'))

  assertDataTable(phecodes)
  assertNames(colnames(phecodes), must.include = c('person_ID', 'phecode'))
  assertCharacter(phecodes$phecode)

  assertDataTable(weights)
  assertNames(colnames(weights), must.include = c('phecode', 'w'))
  assertCharacter(weights$phecode)
  assertNumeric(weights$w)

  demos[,person_ID:=as.character(person_ID)]
  phecodes[,person_ID:=as.character(person_ID)]

  diseasePhecodeMap = mapDiseaseToPhecode(diseaseIDs, dbName)
  phecodesW = merge(phecodes, weights, by = 'phecode')

  phersAll = foreach (ID = unique(diseaseIDs), .combine = rbind) %dopar% {

              phecodesWSub = merge(phecodesW,
                                   diseasePhecodeMap[disease_ID == ID],
                                by = 'phecode', allow.cartesian = TRUE)
              phers = phecodesWSub[, .(phers = sum(w)), by = 'person_ID']

              phers = merge(demos, phers, by = 'person_ID', all.x = TRUE)
              phers[is.na(phers), phers := 0]
              phers[, disease_ID := ID]


              # rphersFit = lm(phers ~ sex + uniq_age + first_age + last_age,
              #               data = phers)
              # phers[, rphers := rstudent(rphersFit)]
              # phers[,.(person_ID, disease_ID, phers, rphers)]


              phers[,.(person_ID, disease_ID, phers)]}

return(phersAll)}





