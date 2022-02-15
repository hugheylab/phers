test_that('check mapICDToPhecode Args', {

  # ICD is numeric
  icdTestErr = copy(icdTest)
  icdTestErr[,icd:=as.numeric(icd)]
  expect_error(mapICDToPhecode(icdTestErr))

  # no ICD flags
  icdTestErr = icdTest[,.(person_id, icd)]
  expect_error(mapICDToPhecode(icdTestErr))

})
