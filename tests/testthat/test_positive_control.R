test_that('getDxStatus output', {
  resObs = getDxStatus(demosTest, icdTest, diseaseDxIcdMap = dxIcdTest)
  resExp = data.table(
    person_id = seq_len(4),
    disease_id = rep(1, 4),
    dx_status = c('control', 'control', 'control', 'case'))

  expect_equal(resObs, resExp, ignore_attr = TRUE)
})
