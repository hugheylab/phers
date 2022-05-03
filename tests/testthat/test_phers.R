weightsTest = data.table(
  phecode = c('001', '002', '003', '004'),
  prev = c(1 / 4, 2 / 4, 1 / 4, 1 / 4), w = -log10(c(1 / 4, 2 / 4, 1 / 4, 1 / 4)))

icdPhecodeMapTest = data.table(
  icd = c('001', '002', '003', '004', '005', '006'),
  phecode = c('001', '002', '003', '004', '005', '006'), flag = 9)

weightsExp = data.table(
  phecode = c('001', '002', '003', '004'),
  prev = c(0.25, 0.50, 0.25, 0.25),
  w = c(0.60206, 0.30103, 0.60206, 0.60206))
setkeyv(weightsExp, 'phecode')

scoresExp = data.table(
  person_id = seq_len(4),
  disease_id = rep(1, 4),
  score = c(0.60206, 0.90309, 0.30103, 0.00000))
setkeyv(scoresExp, c('person_id', 'disease_id'))


test_that('getPhecodeOccurrences output', {

  resObs = getPhecodeOccurrences(
    icdTest, icdPhecodeMap = icdPhecodeMapTest, dxIcd = dxIcdTest)
  setkey(resObs)
  resExp = phecodeOccurrencesTest

  expect_equal(resObs, resExp, ignore_attr = TRUE)
})


test_that('getPhecodeOccurrences output (dxIcd = NULL)', {

  resObs = getPhecodeOccurrences(
    icdTest, icdPhecodeMap = icdPhecodeMapTest, dxIcd = NULL)
  setkey(resObs)
  resExp = data.table(
    person_id = c(1, rep(2L, 3), 3, rep(4L, 3)),
    phecode = c('001', '002', '003', '005', '002', '004', '005', '006'),
    entry_date = c(rep(as.Date('2000/01/01'), 6),
                   c(as.Date('2000/01/01'), as.Date('2000/01/02'))))
  setkey(resExp)

  expect_equal(resObs, resExp)
})


test_that('getPhecodeOccurrences args error', {

  # ICD is numeric
  icdTestErr = copy(icdTest)
  icdTestErr[, icd := as.numeric(icd)]
  expect_error(getPhecodeOccurrences(icdTestErr))

  # no ICD flags
  icdTestErr = icdTest[, .(person_id, icd)]
  expect_error(getPhecodeOccurrences(icdTestErr))

})


test_that('getWeights output', {

  resObs = getWeights(demosTest, phecodeOccurrencesTest)
  resExp = weightsExp
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


test_that('getScores output', {

  resObs = getScores(demosTest, phecodeOccurrencesTest, weightsTest,
                     diseasePhecodeMapTest)
  resExp = scoresExp
  expect_equal(resObs, resExp, ignore_attr = TRUE)
})


test_that('getScores args error', {

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
  resObs = getResidualScores(demosTest2, scoresTest, formTest)
  resExp = snapshot(resObs, file.path(dataDir, 'get_residual_scores_output.qs'))

  expect_equal(resObs, resExp)
})


test_that('phers output', {

  resObs = phers(
    demosTest,  icdTest, diseasePhecodeMapTest,
    icdPhecodeMap = icdPhecodeMapTest, dxIcd = dxIcdTest)
  resObs = lapply(resObs, setkey)

  resExp = list(
    phecodeOccurrences = phecodeOccurrencesTest,
    weights = weightsExp,
    scores = scoresExp)

  expect_equal(resObs, resExp, ignore_attr = TRUE)
})


test_that('phers output (calculate residual scores)', {

  resObs = phers(
    demosTest, icdTest, diseasePhecodeMapTest,
    icdPhecodeMap = icdPhecodeMapTest, dxIcd = dxIcdTest,
    residScoreFormula = formTest)
  resObs = lapply(resObs, setkey)

  rScoresExp = data.table(
    person_id = seq_len(4),
    disease_id = rep(1, 4),
    score = c(0.60206, 0.90309, 0.30103, 0.00000),
    resid_score = c(0.4472136, 1.3416408, -0.4472136, -1.3416408))

  resExp = list(
    phecodeOccurrences = phecodeOccurrencesTest,
    weights = weightsExp,
    scores = rScoresExp)

  expect_equal(resObs, resExp, ignore_attr = TRUE)
})
