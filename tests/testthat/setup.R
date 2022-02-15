library('data.table')
library('qs')
library('foreach')
library('doParallel')
registerDoParallel(cores = 2)


snapshot = function(xObs, path) {
  if (file.exists(path)) {
    xExp = qread(path)
  } else {
    qsave(xObs, path)
    xExp = xObs}
  return(xExp)}




dataDir = 'data'

phecodesTestL = qread(file.path(dataDir, 'phecodes_test.qs'))
demosTestL = qread(file.path(dataDir, 'demos_test.qs'))
weightsTestL = qread(file.path(dataDir, 'weights.qs'))

diseaseIDs = c(154700)
dbName = 'OMIM'

icdTest = data.table('person_id' = c(1, 2, 3),
                     'icd' = c('366', '366', '365'),
                     'flag' = c(9, 9, 9))
demosTest = data.table('person_id' = c(1, 2, 3, 4, 5))
phecodesTest = data.table('person_id' = c(1, 2, 3),
                          'phecode' = c('366', '366', '365'))
weightsTest = data.table('phecode' = c('366', '365'), 'prev' = c(0.4, 0.2),
                         'w' = c(-log(0.4), -log(0.2)))
