#' @import checkmate
#' @importFrom data.table data.table := set
NULL


calcWeights = function(demo, phecodes) {

  npop = uniqueN(demo$GRID)
  prev_weights = phecodes[,.(prev=.N/npop, prev_w=-log(.N/npop)),by=phecode]
  phers_w = phecodes[,.(GRID, prob=.N/npop),by=phecode]

  phers_w_all = foreach (subpheno = unique(phers_w$phecode), .combine = rbind,
                         .packages = c('data.table')) %dopar% {
                           phers_w_demo = merge(demo, phers_w[phecode==subpheno,], by = "GRID", all.x = TRUE)

                           phers_w_demo[, dx_status := ifelse(is.na(phecode), 0, 1)]
                           phers_w_demo[,phecode:=subpheno]

                           uprob = prev_weights[phecode==subpheno]$prev
                           phers_w_demo[,prob:=uprob]

                           phers_w_demo}

  saveRDS(prev_weights, file.path(resultDir, paste0("prev_weights_", dataName, ".rds")))
  calc_phers(phers_w_all, paste0("prev_phers_", dataName, ".rds"), dID_phecodes_long)
}


calc_phers = function(weights, dID_phecodes_long){

  weights[,w:=(1 - 2 * dx_status) * log(dx_status * prob + (1 - dx_status) * (1 - prob))]
  weights[,w0:=dx_status*w]

  dID_weights = merge(weights, dID_phecodes_long, by='phecode', allow.cartesian=T)
  phers = dID_weights[, .(phers = sum(w), phers0=sum(w0)), by = .(GRID, dID)]

}
