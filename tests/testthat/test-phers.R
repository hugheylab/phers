test_that("calcWeights output 1", {

  weightsOut = calcWeights(demosTest, phecodesTest)

  expect_equal(weightsOut[phecode=='366']$prev, 0.4)
  expect_equal(weightsOut[phecode=='365']$prev, 0.2)
  expect_equal(weightsOut$w, -log(weightsOut$prev))
})

test_that("calcWeights output 2", {

  weightsObs = calcWeights(demosTestL, phecodesTestL)
  weightsExp = snapshot(weightsObs, file.path(dataDir, 'weights.qs'))
  expect_equal(weightsObs, weightsExp)
})

test_that("check CalcWeights Args", {

  # no column named person_id
  demosTestErr = copy(demosTest)
  setnames(demosTestErr, 'person_id', 'person_id_Err')
  expect_error(calcWeights(demosTestErr, phecodesTest))

  # no column named person_id in phecodes
  phecodesTestErr = copy(phecodesTest)
  setnames(phecodesTestErr, 'person_id', 'person_id_Err')
  expect_error(calcWeights(demosTest, phecodesTestErr))

  # phecodes is a data.frame
  phecodesTestErr = copy(phecodesTest)
  phecodesTestErr = data.frame(phecodesTestErr)
  expect_error(calcWeights(demosTest, phecodesTestErr))

  # phecodes are numeric
  phecodesTestErr = copy(phecodesTest)
  phecodesTestErr[,phecode:=as.numeric(phecode)]
  expect_error(calcWeights(demosTest, phecodesTestErr))

  # demo has less person_ids than phecodes
  demosTestErr = demosTest[1:2]
  expect_error(calcWeights(demosTestErr, phecodesTest))

})


test_that('check calcPheRS output', {

  phersObs = calcPheRS(demosTestL, phecodesTestL, weightsTestL,
                         diseaseIDs, dbName = dbName)
  phersExp = snapshot(phersObs, file.path(dataDir, 'phers.qs'))
  expect_equal(phersObs, phersExp)
})


test_that("check CalcPheRS Args",{

  # no column named person_id demos
  demosTestErr = copy(demosTest)
  setnames(demosTestErr, 'person_id', 'person_id_Err')
  expect_error(calcPheRS(demosTestErr, phecodesTest, weightsTest,
                         diseaseIDs, dbName = 'OMIM'))

  # no column named person_id in phecodes
  phecodesTestErr = copy(phecodesTest)
  setnames(phecodesTestErr, 'person_id', 'person_id_Err')
  expect_error(calcPheRS(demosTestErr, phecodesTestErr, weightsTest,
                         diseaseIDs, dbName = 'OMIM'))

  # phecodes is a data.frame
  phecodesTestErr = copy(phecodesTest)
  phecodesTestErr = data.frame(phecodesTestErr)
  expect_error(calcPheRS(demosTestErr, phecodesTestErr, weightsTest,
                         diseaseIDs, dbName = 'OMIM'))

  # phecodes are numeric
  phecodesTestErr = copy(phecodesTest)
  phecodesTestErr[,phecode:=as.numeric(phecode)]
  expect_error(calcPheRS(demosTestErr, phecodesTestErr, weightsTest,
                         diseaseIDs, dbName = 'OMIM'))

  # phecodes are numeric in weights
  weightsTestErr = copy(weightsTest)
  weightsTestErr[,phecode:=as.numeric(phecode)]
  expect_error(calcPheRS(demosTestErr, phecodesTest, weightsTestErr,
                         diseaseIDs, dbName = 'OMIM'))

})

