test_that('check mapDiseaseToPhecode Args', {

  # unknown database name
  dbNameErr = 'OMIMErr'
  expect_error(mapDiseaseToPhecode(diseaseIDs, dbNameErr))

  # unknown disease ID
  diseaseIDsErr = c(11111)
  expect_error(mapDiseaseToPhecode(diseaseIDsErr, dbName))

})
