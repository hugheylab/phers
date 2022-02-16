library('data.table')
library('qs')
library('foreach')
library('doParallel')
registerDoSEQ()


snapshot = function(xObs, path) {
  if (file.exists(path)) {
    xExp = qread(path)
  } else {
    qsave(xObs, path)
    xExp = xObs}
  return(xExp)}




# data for unit test
icdTest = data.table('person_id' = c(1, 2),
                     'icd' = c('001', '002'),
                     'flag' = c(9, 9))
demosTest = data.table('person_id' = c(1, 2))
phecodesTest = data.table('person_id' = c(1, 2),
                          'phecode' = c('001', '002'))
weightsTest = data.table('phecode' = c('001', '002'), 'prev' = c(0.5, 0.5),
                         'w' = c(-log(0.5), -log(0.5)))
dbNameTest = 'OMIM'
diseaseIDsTest = c(1)
diseaseHPOMapTest = data.table(
  'db_name' = c('OMIM'), 'disease_id' = c(1), 'term_id' = c(1))
HPOPhecodeMapTest = data.table('term_id' = c(1), 'phecode' = c('001'))
diseasePhecodeMapTest = data.table('disease_id' = c(1), 'phecode' = c('001'))
ICDPhecodeMapTest = data.table(
  'icd' = c('001', '002'), 'phecode' = c('001', '002'), 'flag' = c(9, 9))


# # data for snapshot tests
# dataDir = 'data'
# phecodesTestL = qread(file.path(dataDir, 'phecodes_test.qs'))
# demosTestL = qread(file.path(dataDir, 'demos_test.qs'))
# weightsTestL = qread(file.path(dataDir, 'weights.qs'))
# diseasePhecodeMapTestL = qread(file.path(dataDir, 'disease_to_phecode_map.qs'))
