test_that('runLinear output', {

  linearOut = runLinear(lmInputTest, formStrTest, coding = 'additive')

  expect_s3_class(linearOut, 'data.table')
  expect_equal(nrow(linearOut), 1)
  expect_named(linearOut, c('beta', 'se', 'p', 'lower', 'upper'),
               ignore.order = TRUE)

  lmTest = glm(score~variant+sex, data = lmInputTest)

  expect_equal(linearOut$beta, lmTest$coef[['variant']])
  expect_equal(linearOut$se,
               summary(lmTest)$coef['variant', 'Std. Error'])
  expect_equal(linearOut$p,
               summary(lmTest)$coef['variant', 'Pr(>|t|)'])
  expect_equal(
    linearOut$lower, suppressMessages(confint(lmTest)['variant', '2.5 %']))
  expect_equal(
    linearOut$upper, suppressMessages(confint(lmTest)['variant', '97.5 %']))
})



