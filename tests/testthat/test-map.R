test_that('check mapICDToPhecode output', {

  icdToPhecodeOut = getPhecodeOccurrences(
    icdTest, icdPhecodeMap = icdPhecodeMapTest)
  setkey(icdToPhecodeOut)
  expect_equal(icdToPhecodeOut, phecodeOccurrencesTest)
  expect_s3_class(icdToPhecodeOut, 'data.table')
})


test_that('check mapICDToPhecode Args', {

  # ICD is numeric
  icdTestErr = copy(icdTest)
  icdTestErr[, icd := as.numeric(icd)]
  expect_error(getPhecodeOccurrences(icdTestErr))

  # no ICD flags
  icdTestErr = icdTest[, .(person_id, icd)]
  expect_error(getPhecodeOccurrences(icdTestErr))

})


test_that('check mapDiseaseToPhecode output', {

  diseaseToPhecodeOut = mapDiseaseToPhecode(
    diseaseHpoMap = diseaseHpoMapTest, hpoPhecodeMap = hpoPhecodeMapTest)
  setkey(diseaseToPhecodeOut)
  expect_equal(diseaseToPhecodeOut, diseasePhecodeMapTest)

  expect_s3_class(diseaseToPhecodeOut, 'data.table')
})
