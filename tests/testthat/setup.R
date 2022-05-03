library('data.table')
library('foreach')
library('qs')
library('BEDMatrix')
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
  person_id = c(1, rep(2L, 3), 3, rep(4L, 3)),
  icd = c('001', '002', '003', '005', '002', '004', '005', '006'),
  flag = 9,
  entry_date = c(
    rep(as.Date('2000/01/01'), 6),
    c(as.Date('2000/01/01'), as.Date('2000/01/02'))))

demosTest = data.table(
  person_id = 1:4, sex = c('female', 'male', 'female', 'male'))

phecodeOccurrencesTest = data.table(
  person_id = c(1, rep(2L, 2), 3, 4),
  phecode = c('001', '002', '003', '002', '004'),
  entry_date = rep(as.Date('2000/01/01'), 5))
setkey(phecodeOccurrencesTest)

dxIcdTest = data.table(disease_id = 1, icd = c('005', '006'), flag = 9)

diseasePhecodeMapTest = data.table(
  disease_id = 1, phecode = c('001', '002', '003'))
setkey(diseasePhecodeMapTest)

scoresTest = data.table(
  person_id = rep(1:6, 2), disease_id = c(rep(1, 6), rep(2, 6)),
  score = c(5, 3, 4, 1, 0.5, 0, 2, 1, 0, 1, 2, 0.5))

demosTest2 = data.table(
  person_id = 1:6,
  sex = c('female', 'male', 'female', 'male', 'female', 'male'))
formTest = ~ sex
