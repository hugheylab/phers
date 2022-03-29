test_that('mapDiseaseToPhecode output', {

  diseaseHpoMapTest = data.table(
    disease_id = 1, term_id = c(1, 2, 3))
  hpoPhecodeMapTest = data.table(
    term_id = c(1, 2, 3), phecode = c('001', '002', '003'))
  mapObs = mapDiseaseToPhecode(
    diseaseHpoMap = diseaseHpoMapTest, hpoPhecodeMap = hpoPhecodeMapTest)
  setkey(mapObs)
  mapExp = diseasePhecodeMapTest

  expect_equal(mapObs, mapExp)
})
