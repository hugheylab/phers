test_that('check mapICDToPhecode output', {

  icdToPhecodeOut = getPhecodeOccurrences(icdTest, icdPhecodeMap = icdPhecodeMapTest)
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
    c(1), 'OMIM', diseaseHpoMap = diseaseHpoMapTest,
    hpoPhecodeMap = hpoPhecodeMapTest)

  expect_equal(diseaseToPhecodeOut, diseasePhecodeMapTest)

  expect_s3_class(diseaseToPhecodeOut, 'data.table')
})


test_that('check mapDiseaseToPhecode Args', {

  # unknown database name
  dbNameTestErr = 'OMIMErr'
  expect_error(mapDiseaseToPhecode(diseaseIDsTest, dbNameTestErr,
                                   diseaseHPOMapTest, HPOPhecodeMapTest))

  # unknown disease ID
  diseaseIDsTestErr = c(0)
  expect_error(mapDiseaseToPhecode(diseaseIDsTestErr, dbNameTest,
                                   diseaseHPOMapTest, HPOPhecodeMapTest))

})
