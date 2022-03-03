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

# getWeights() and getScores() test data
icdTest = data.table(
  person_id = c(1, rep(2L, 2), 3, 4),
  icd = c('001', '002', '003', '002', '004'), flag = 9)
demosTest = data.table(person_id = 1:4)
phecodeOccurrencesTest = data.table(
  person_id = c(1, rep(2L, 2), 3, 4),
  phecode = c('001', '002', '003', '002', '004'))
setkey(phecodeOccurrencesTest)
weightsTest = data.table(
  phecode = c('001', '002', '003', '004'),
  prev = c(1/4, 2/4, 1/4, 1/4), w = -log10(c(1/4, 2/4, 1/4, 1/4)))

diseaseIdsTest = 1
diseaseHpoMapTest = data.table(disease_id = 1, term_id = c(1, 2, 3))
hpoPhecodeMapTest = data.table(
  term_id = c(1, 2, 3), phecode = c('001', '002', '003'))
diseasePhecodeMapTest = data.table(
  disease_id = 1, phecode = c('001', '002', '003'))
setkey(diseasePhecodeMapTest)
icdPhecodeMapTest = data.table(
  icd = c('001', '002', '003', '004'),
  phecode = c('001', '002', '003', '004'), flag = 9)


# runLinear() test data
lmInputTest = data.table(score = c(5, 3, 4, 1, 0.5, 0),
                         allele_count = c(2, 1, 1, 0, 0, 0),
                         sex = c('F', 'M', 'F', 'M', 'F', 'M'))
formStrTest = as.formula(~ sex)


# genotypeAssociation() test data
scoresTest = data.table(
  person_id = rep(1:6, 2), disease_id = c(rep(1, 6), rep(2, 6)),
  score = c(5, 3, 4, 1, 0.5, 0, 2, 1, 0, 1, 2, 0.5))
genotypesTest = data.table(
  person_id = 1:6, snp1 = c(2, 1, 1, 0, 0, 0),
  snp2 = c(0, 1, 0, 0, 0, 1))
demosTest2 = data.table(person_id = 1:6, sex = c('F', 'M', 'F', 'M', 'F', 'M'))
diseaseGeneVarMapTest = data.table(
  disease_id = c(1, 2), gene = c('a', 'b'), vid = c('snp1', 'snp2'))
formStrTest = as.formula(~ sex)

# # data for snapshot tests
# dataDir = 'data'
# phecodesTestL = qread(file.path(dataDir, 'phecodes_test.qs'))
# demosTestL = qread(file.path(dataDir, 'demos_test.qs'))
# weightsTestL = qread(file.path(dataDir, 'weights.qs'))
# diseasePhecodeMapTestL = qread(file.path(dataDir, 'disease_to_phecode_map.qs'))
