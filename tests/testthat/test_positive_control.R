test_that('check getDxStatus output', {
  dxStatusOut = getDxStatus(demosTest, icdTest, diseaseDxIcdMap = dxIcdTest)
  expect_s3_class(dxStatusOut, 'data.table')

  expect_named(
    dxStatusOut, c('disease_id', 'person_id', 'dx_status'), ignore.order = TRUE)
  setkey(dxStatusOut)
  expect_equal(dxStatusOut, dxStatusTest)
})
