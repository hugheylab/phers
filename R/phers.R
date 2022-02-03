#' @import checkmate
#' @importFrom data.table data.table := set
NULL


#'
#' @param GRIDs vector of person IDs for everyone in the population
#' @param phecodes [GRID, phecode]
calcWeights = function(GRIDs, phecodes) {

  npop = uniqueN(GRIDs)
  weights = phecodes[,.(prev=.N/npop, w=-log(.N/npop)),by=phecode]

return(weights)}

#'
#' @param GRIDs vector of person IDs for everyone in the population
#' @param phecodes [GRID, phecode]
#' @param weights  [phecode, w]
#' @param diseasePhecodeMap [dID, phecode]
calcPheRS = function(GRIDs, phecodes, weights, diseasePhecodeMap){

  GRIDs = data.table('GRID'=GRIDs)
  weightsAll = foreach (subpheno = unique(phecodes$phecode), .combine = rbind,
                         .packages = c('data.table')) %dopar% {
                           weightsSub = merge(GRIDs,
                                              phecodes[phecode==subpheno,],
                                              by = "GRID", all.x = TRUE)

                           weightsSub[, dx_status := ifelse(is.na(phecode),
                                                            0, 1)]
                           weightsSub[,phecode:=subpheno]
                           weightsSub[,w:=dx_status*weights[phecode==subpheno]$w]

                           weightsSub}

  weightsAll = merge(weightsAll, diseasePhecodeMap, by='phecode',
                     allow.cartesian=T)
  phers = weightsAll[, .(phers = sum(w)), by = .(GRID, dID)]

return(phers)}
