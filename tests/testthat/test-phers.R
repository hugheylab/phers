test_that("getWeights output 1", {

  weightsOut = getWeights(demosTest, phecodesTest)

  expect_equal(weightsOut[phecode == '001']$prev, 0.5)
  expect_equal(weightsOut[phecode == '002']$prev, 0.5)
  expect_equal(weightsOut$w, -log(weightsOut$prev))
  expect_s3_class(weightsOut, "data.table")
})

# # getWeights snapshot test
# test_that("getWeights output 2", {
#
#   weightsObs = getWeights(demosTestL, phecodesTestL)
#   weightsExp = snapshot(weightsObs, file.path(dataDir, 'weights.qs'))
#   expect_equal(weightsObs, weightsExp)
# })

test_that("check getWeights Args", {

  # no column named person_id
  demosTestErr = copy(demosTest)
  setnames(demosTestErr, 'person_id', 'person_id_Err')
  expect_error(getWeights(demosTestErr, phecodesTest))

  # no column named person_id in phecodes
  phecodesTestErr = copy(phecodesTest)
  setnames(phecodesTestErr, 'person_id', 'person_id_Err')
  expect_error(getWeights(demosTest, phecodesTestErr))

  # phecodes is a data.frame
  phecodesTestErr = copy(phecodesTest)
  phecodesTestErr = data.frame(phecodesTestErr)
  expect_error(getWeights(demosTest, phecodesTestErr))

  # phecodes are numeric
  phecodesTestErr = copy(phecodesTest)
  phecodesTestErr[,phecode := as.numeric(phecode)]
  expect_error(getWeights(demosTest, phecodesTestErr))

  # demo has less person_ids than phecodes
  demosTestErr = demosTest[1]
  expect_error(getWeights(demosTestErr, phecodesTest))

})


# # getPheRS snapshot test
# test_that('check getPheRS output', {
#
#   phersObs = getPheRS(demosTestL, phecodesTestL, weightsTestL,
#                        diseasePhecodeMapTest)
#   phersExp = snapshot(phersObs, file.path(dataDir, 'phers.qs'))
#   expect_equal(phersObs, phersExp)
# })

test_that("getPheRS output 1", {

  phersOut = getPheRS(demosTest, phecodesTest, weightsTest,
                      diseasePhecodeMapTest)

  expect_equal(phersOut[person_id == 1 & disease_id == 1]$phers, -log(0.5))
  expect_equal(phersOut[person_id == 2 & disease_id == 1]$phers, 0)
  expect_s3_class(phersOut, "data.table")
})


test_that("check getPheRS Args",{

  # no column named person_id demos
  demosTestErr = copy(demosTest)
  setnames(demosTestErr, 'person_id', 'person_id_Err')
  expect_error(getPheRS(demosTestErr, phecodesTest, weightsTest,
                         diseasePhecodeMapTest))

  # no column named person_id in phecodes
  phecodesTestErr = copy(phecodesTest)
  setnames(phecodesTestErr, 'person_id', 'person_id_Err')
  expect_error(getPheRS(demosTestErr, phecodesTestErr, weightsTest,
                         diseasePhecodeMapTest))

  # phecodes is a data.frame
  phecodesTestErr = copy(phecodesTest)
  phecodesTestErr = data.frame(phecodesTestErr)
  expect_error(getPheRS(demosTestErr, phecodesTestErr, weightsTest,
                         diseasePhecodeMapTest))

  # phecodes are numeric
  phecodesTestErr = copy(phecodesTest)
  phecodesTestErr[, phecode := as.numeric(phecode)]
  expect_error(getPheRS(demosTestErr, phecodesTestErr, weightsTest,
                         diseasePhecodeMapTest))

  # phecodes are numeric in weights
  weightsTestErr = copy(weightsTest)
  weightsTestErr[, phecode := as.numeric(phecode)]
  expect_error(getPheRS(demosTestErr, phecodesTest, weightsTestErr,
                         diseasePhecodeMapTest))

})

