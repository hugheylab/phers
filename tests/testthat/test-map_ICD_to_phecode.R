test_that('check mapICDToPhecode output', {

  icdToPhecodeOut = mapICDToPhecode(icdTest, ICDPhecodeMap = ICDPhecodeMapTest)
  expect_equal(icdToPhecodeOut, phecodesTest)
  expect_s3_class(icdToPhecodeOut, "data.table")
})


test_that('check mapICDToPhecode Args', {

  # ICD is numeric
  icdTestErr = copy(icdTest)
  icdTestErr[, icd := as.numeric(icd)]
  expect_error(mapICDToPhecode(icdTestErr))

  # no ICD flags
  icdTestErr = icdTest[, .(person_id, icd)]
  expect_error(mapICDToPhecode(icdTestErr))

})
