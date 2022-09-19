test_that('getWeights output (prevalence model)', {

  resObs = getWeights(demosTest, phecodeOccurrencesTest)
  resExp = weightsExp
  expect_equal(resObs, resExp)
})


test_that('getWeights output (logistic model)', {
  resObs = getWeights(
    demosTest, phecodeOccurrencesTest, method = 'logistic',
    methodFormula = ~ sex)
  resExp = snapshot(resObs, file.path(dataDir, 'get_weights_logistic_output.qs'))
  expect_equal(resObs, resExp)
})


test_that('getWeights output (loglinear model)', {
  resObs = getWeights(
    demosTest, phecodeOccurrencesLLTest, method = 'loglinear',
    methodFormula = ~ sex)
  resExp = snapshot(resObs, file.path(dataDir, 'get_weights_loglinear_output.qs'))
  expect_equal(resObs, resExp)
})


test_that('getWeights output (cox model)', {
  resObs = suppressWarnings(getWeights(
    demosTest, phecodeOccurrencesCoxTest, method = 'cox',
    methodFormula = ~ sex))
  resExp = snapshot(resObs, file.path(dataDir, 'get_weights_cox_output.qs'))
  expect_equal(resObs, resExp)
})


test_that('getWeights args error', {

  # no column named person_id
  demosTestErr = copy(demosTest)
  setnames(demosTestErr, 'person_id', 'person_id_Err')
  expect_error(getWeights(demosTestErr, phecodeOccurrencesTest))

  # no column named person_id in phecodes
  phecodeOccurrencesTestErr = copy(phecodeOccurrencesTest)
  setnames(phecodeOccurrencesTestErr, 'person_id', 'person_id_Err')
  expect_error(getWeights(demosTest, phecodeOccurrencesTestErr))

  # demo has less person_ids than phecodes
  demosTestErr = demosTest[1]
  expect_error(getWeights(demosTestErr, phecodeOccurrencesTest))
})
