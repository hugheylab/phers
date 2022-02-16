
test_that('check mapDiseaseToPhecode output', {

  diseaseToPhecodeOut = mapDiseaseToPhecode(
    c(1), 'OMIM', diseaseHPOMap = diseaseHPOMapTest,
    HPOPhecodeMap = HPOPhecodeMapTest)

  expect_equal(diseaseToPhecodeOut, diseasePhecodeMapTest)

  expect_s3_class(diseaseToPhecodeOut, "data.table")
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
