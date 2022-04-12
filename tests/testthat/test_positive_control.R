test_that('getDxStatus output', {
  resObs = getDxStatus(demosTest, icdTest, diseaseDxIcdMap = dxIcdTest)
  resExp = data.table(
    person_id = c(1, 3, 4),
    disease_id = 1,
    dx_status = c(0, 0, 1))

  expect_equal(resObs, resExp, ignore_attr = TRUE)
})
