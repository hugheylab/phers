library('data.table')
library('foreach')
library('qs')
registerDoSEQ()


snapshot = function(xObs, path) {
  if (file.exists(path)) {
    xExp = qs::qread(path)
  } else {
    qs::qsave(xObs, path)
    xExp = xObs}
  return(xExp)}
dataDir = 'data'


# data for unit test
icdTest = data.table(
  person_id = c(1, rep(2L, 2), 3, rep(4L, 2)),
  icd = c('001', '002', '003', '002', '004', '005'), flag = 9)
demosTest = data.table(person_id = 1:4)
phecodeOccurrencesTest = data.table(
  person_id = c(1, rep(2L, 2), 3, 4),
  phecode = c('001', '002', '003', '002', '004'))
setkey(phecodeOccurrencesTest)
# weightsTest could probably be placed at the top of test_phers.R
# since it is only used in that script, unless it might be used in other test scripts.
weightsTest = data.table(
  phecode = c('001', '002', '003', '004'),
  prev = c(1 / 4, 2 / 4, 1 / 4, 1 / 4), w = -log10(c(1 / 4, 2 / 4, 1 / 4, 1 / 4)))
dxIcdTest = data.table(disease_id = 1, icd = '005', flag = 9)
diseasePhecodeMapTest = data.table(
  disease_id = 1, phecode = c('001', '002', '003'))
setkey(diseasePhecodeMapTest)
# icdPhecodeMapTest could probably be placed at the top of test_phers.R
# since it is only used in that script, unless it might be used in other test scripts.
icdPhecodeMapTest = data.table(
  icd = c('001', '002', '003', '004', '005'),
  phecode = c('001', '002', '003', '004', '005'), flag = 9)
# lmInputTest could probably be placed at the top of test_genetic_association.R
# since it is only used in that script, unless it might be used in other test scripts.
lmInputTest = data.table(score = c(5, 3, 4, 1, 0.5, 0),
                         allele_count = c(2, 1, 1, 0, 0, 0),
                         sex = c('F', 'M', 'F', 'M', 'F', 'M'))
scoresTest = data.table(
  person_id = rep(1:6, 2), disease_id = c(rep(1, 6), rep(2, 6)),
  score = c(5, 3, 4, 1, 0.5, 0, 2, 1, 0, 1, 2, 0.5))
demosTest2 = data.table(person_id = 1:6, sex = c('F', 'M', 'F', 'M', 'F', 'M'))
formTest = as.formula(~ sex)
