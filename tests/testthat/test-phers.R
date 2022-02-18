test_that('getWeights output 1', {

  weightsOut = getWeights(demosTest, phecodeOccurrencesTest)

  expect_equal(weightsOut[phecode == '001']$prev, 0.5)
  expect_equal(weightsOut[phecode == '002']$prev, 0.5)
  expect_equal(weightsOut$w, -log10(weightsOut$prev))
  expect_s3_class(weightsOut, 'data.table')
})


test_that('check getWeights Args', {

  # no column named person_id
  demosTestErr = copy(demosTest)
  setnames(demosTestErr, 'person_id', 'person_id_Err')
  expect_error(getWeights(demosTestErr, phecodeOccurrencesTest))

  # no column named person_id in phecodes
  phecodeOccurrencesTestErr = copy(phecodeOccurrencesTest)
  setnames(phecodeOccurrencesTestErr, 'person_id', 'person_id_Err')
  expect_error(getWeights(demosTest, phecodeOccurrencesTestErr))

  # # phecodes is a data.frame
  # phecodeOccurrencesTestErr = copy(phecodeOccurrencesTest)
  # phecodeOccurrencesTestErr = data.frame(phecodeOccurrencesTestErr)
  # expect_error(getWeights(demosTest, phecodeOccurrencesTestErr))

  # # phecodes are numeric
  # phecodeOccurrencesTestErr = copy(phecodeOccurrencesTest)
  # phecodeOccurrencesTestErr[,phecode := as.numeric(phecode)]
  # expect_error(getWeights(demosTest, phecodeOccurrencesTestErr))

  # demo has less person_ids than phecodes
  demosTestErr = demosTest[1]
  expect_error(getWeights(demosTestErr, phecodeOccurrencesTest))
})


# # getScores snapshot test
# test_that('check getScores output', {
#
#   phersObs = getScores(demosTestL, phecodeOccurrencesTestL, weightsTestL,
#                        diseasePhecodeMapTest)
#   phersExp = snapshot(phersObs, file.path(dataDir, 'phers.qs'))
#   expect_equal(phersObs, phersExp)
# })

test_that('getScores output 1', {

  phersOut = getScores(demosTest, phecodeOccurrencesTest, weightsTest,
                      diseasePhecodeMapTest)

  expect_equal(phersOut[person_id == 1 & disease_id == 1]$score, -log10(0.5))
  expect_equal(phersOut[person_id == 2 & disease_id == 1]$score, 0)
  expect_s3_class(phersOut, 'data.table')
})


test_that('check getScores Args', {

  # no column named person_id demos
  demosTestErr = copy(demosTest)
  setnames(demosTestErr, 'person_id', 'person_id_Err')
  expect_error(getScores(demosTestErr, phecodeOccurrencesTest, weightsTest,
                         diseasePhecodeMapTest))

  # no column named person_id in phecodes
  phecodeOccurrencesTestErr = copy(phecodeOccurrencesTest)
  setnames(phecodeOccurrencesTestErr, 'person_id', 'person_id_Err')
  expect_error(getScores(demosTestErr, phecodeOccurrencesTestErr, weightsTest,
                         diseasePhecodeMapTest))

  # phecodes is a data.frame
  # phecodeOccurrencesTestErr = copy(phecodeOccurrencesTest)
  # phecodeOccurrencesTestErr = data.frame(phecodeOccurrencesTestErr)
  # expect_error(getScores(demosTestErr, phecodeOccurrencesTestErr, weightsTest,
  #                        diseasePhecodeMapTest))

  # phecodes are numeric
  phecodeOccurrencesTestErr = copy(phecodeOccurrencesTest)
  phecodeOccurrencesTestErr[, phecode := as.numeric(phecode)]
  expect_error(getScores(demosTestErr, phecodeOccurrencesTestErr, weightsTest,
                         diseasePhecodeMapTest))

  # phecodes are numeric in weights
  weightsTestErr = copy(weightsTest)
  weightsTestErr[, phecode := as.numeric(phecode)]
  expect_error(getScores(demosTestErr, phecodeOccurrencesTest, weightsTestErr,
                         diseasePhecodeMapTest))
})
