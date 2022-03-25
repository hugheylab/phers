test_that('check getPhecodeOccurrences output', {

  phecodeOccurrencesOut = getPhecodeOccurrences(
    icdTest, icdPhecodeMap = icdPhecodeMapTest, dxIcd = dxIcdTest)
  expect_s3_class(phecodeOccurrencesOut, 'data.table')
  expect_named(
    phecodeOccurrencesOut, c('person_id', 'phecode'), ignore.order = TRUE)
  setkey(phecodeOccurrencesOut)
  expect_equal(phecodeOccurrencesOut, phecodeOccurrencesTest)

  phecodeOccurrencesOut2 = getPhecodeOccurrences(
    icdTest, icdPhecodeMap = icdPhecodeMapTest, dxIcd = NULL)
  expect_s3_class(phecodeOccurrencesOut2, 'data.table')
  expect_named(
    phecodeOccurrencesOut2, c('person_id', 'phecode'), ignore.order = TRUE)
  setkey(phecodeOccurrencesOut2)
  expect_equal(phecodeOccurrencesOut2, phecodeOccurrencesTest2)
})


test_that('check getPhecodeOccurrences Args', {

  # ICD is numeric
  icdTestErr = copy(icdTest)
  icdTestErr[, icd := as.numeric(icd)]
  expect_error(getPhecodeOccurrences(icdTestErr))

  # no ICD flags
  icdTestErr = icdTest[, .(person_id, icd)]
  expect_error(getPhecodeOccurrences(icdTestErr))

})


test_that('getWeights output 1', {

  weightsOut = getWeights(demosTest, phecodeOccurrencesTest)

  expect_s3_class(weightsOut, 'data.table')
  expect_equal(nrow(weightsOut), uniqueN(phecodeOccurrencesTest$phecode))
  expect_named(weightsOut, c('phecode', 'prev', 'w'), ignore.order = TRUE)

  expect_equal(weightsOut[phecode == '001']$prev, 1 / 4)
  expect_equal(weightsOut[phecode == '002']$prev, 2 / 4)
  expect_equal(weightsOut$w, -log10(weightsOut$prev))
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

  expect_s3_class(phersOut, 'data.table')
  expect_equal(
    nrow(phersOut), nrow(demosTest) * uniqueN(diseasePhecodeMapTest$disease_id))
  expect_named(phersOut, c('person_id', 'disease_id', 'score'),
               ignore.order = TRUE)

  expect_equal(phersOut[person_id == 1 & disease_id == 1]$score, -log10(1 / 4))
  expect_equal(
    phersOut[person_id == 2 & disease_id == 1]$score, -log10(1 / 4) + -log10(1 / 2))
  expect_equal(phersOut[person_id == 3 & disease_id == 1]$score, -log10(1 / 2))
  expect_equal(phersOut[person_id == 4 & disease_id == 1]$score, 0)

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


test_that('getResidualScores output', {
  rphersOut = getResidualScores(demosTest2, scoresTest, formTest)

  expect_s3_class(rphersOut, 'data.table')
  expect_equal(nrow(rphersOut), nrow(scoresTest))
  expect_named(
    rphersOut, c('person_id', 'disease_id', 'score', 'r_score'),
    ignore.order = TRUE)

  rInputTest = merge(scoresTest, demosTest2, by = 'person_id')
  rFitTest1 = glm(score ~ sex, data = rInputTest[disease_id == 1])
  rFitTest2 = glm(score ~ sex, data = rInputTest[disease_id == 2])

  expect_equal(rphersOut[disease_id == 1]$r_score, unname(rstandard(rFitTest1)))
  expect_equal(rphersOut[disease_id == 2]$r_score, unname(rstandard(rFitTest2)))

})
