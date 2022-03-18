test_that('check mapDiseaseToPhecode output', {

  diseaseToPhecodeOut = mapDiseaseToPhecode(
    diseaseHpoMap = diseaseHpoMapTest, hpoPhecodeMap = hpoPhecodeMapTest)
  setkey(diseaseToPhecodeOut)
  expect_equal(diseaseToPhecodeOut, diseasePhecodeMapTest)

  expect_s3_class(diseaseToPhecodeOut, 'data.table')
})
