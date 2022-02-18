library('data.table')
foreach::registerDoSEQ()


snapshot = function(xObs, path) {
  if (file.exists(path)) {
    xExp = qs::qread(path)
  } else {
    qs::qsave(xObs, path)
    xExp = xObs}
  return(xExp)}


# data for unit test
icdTest = data.table(person_id = c(1, 2), icd = c('001', '002'), flag = 9)
demosTest = data.table(person_id = c(1, 2))
phecodeOccurrencesTest = data.table(person_id = c(1, 2), phecode = c('001', '002'))
weightsTest = data.table(phecode = c('001', '002'), prev = 0.5, w = -log10(0.5))

dbNameTest = 'OMIM'
diseaseIdsTest = 1
diseaseHpoMapTest = data.table(db_name = 'OMIM', disease_id = 1, term_id = 1)
hpoPhecodeMapTest = data.table(term_id = c(1), phecode = '001')
diseasePhecodeMapTest = data.table(disease_id = 1, phecode = '001')
icdPhecodeMapTest = data.table(
  icd = c('001', '002'), phecode = c('001', '002'), flag = 9)


# # data for snapshot tests
# dataDir = 'data'
# phecodesTestL = qread(file.path(dataDir, 'phecodes_test.qs'))
# demosTestL = qread(file.path(dataDir, 'demos_test.qs'))
# weightsTestL = qread(file.path(dataDir, 'weights.qs'))
# diseasePhecodeMapTestL = qread(file.path(dataDir, 'disease_to_phecode_map.qs'))
