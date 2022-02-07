#' @import checkmate
#' @importFrom data.table data.table := set
NULL


#'
#' @param GRIDs vector of person IDs for everyone in the population
#' @param phecodes [GRID, phecode]
calcWeights = function(GRIDs, phecodes) {

  npop = uniqueN(GRIDs)
  weights = phecodes[, .(prev = .N / npop, w = -log(.N / npop)), by = phecode]

return(weights)}

#'
#' @param demos vector of person IDs for everyone in the population
#' @param phecodes [GRID, phecode]
#' @param weights  [phecode, w]
#' @param diseasePhecodeMap [dID, phecode]
calcPheRS = function(demos, phecodes, weights, diseasePhecodeMap, diseaseIDs){

  phecodesW = merge(phecodes, weights, by='phecode')

  phersAll = foreach (ID = unique(diseaseIDs),
                        .combine = rbind) %dopar% {


              phecodesWSub = merge(phecodesW, diseasePhecodeMap[diseaseID==ID],
                                by = 'phecode', allow.cartesian = TRUE)
              phers = phecodesWSub[, .(phers = sum(w)), by = .(GRID, dID)]

              phers = merge(demo, phers, by='GRID', all.x = TRUE)
              phers[is.na(phers),phers:=0]
              phers[, diseaseID:=ID]}

return(phersAll)}
