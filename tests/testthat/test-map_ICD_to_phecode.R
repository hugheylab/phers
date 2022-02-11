test_that('check mapICDToPhecode Args', {

  # ICD is numeric
  icdTestErr = copy(icdTest)
  icdTestErr[,ICD:=as.numeric(ICD)]
  expect_error(mapICDToPhecode(icdTestErr))

  # no ICD flags
  icdTestErr = icdTest[,.(person_ID, ICD)]
  expect_error(mapICDToPhecode(icdTestErr))

})
