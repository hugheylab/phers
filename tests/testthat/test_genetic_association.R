test_that('runLinear output (additive)', {

  resObs = runLinear(lmInputTest, formTest, modelType = 'additive', 1, 'snp1')
  resExp = data.table(
    disease_id = 1,
    vid = 'snp1',
    n_total = 6,
    n_wt = 3,
    n_het = 2,
    n_hom = 1,
    beta = 2.3125,
    se = 0.44341243,
    pval = 0.0137074787,
    lower = 1.4434276,
    upper = 3.1815724)

  expect_equal(resObs, resExp)
})


test_that('runLinear output (genotypic)', {

  resObs = runLinear(lmInputTest, formTest, modelType = 'genotypic', 1, 'snp1')
  resExp = data.table(
    disease_id = 1,
    vid = 'snp1',
    n_total = 6,
    n_wt = 3,
    n_het = 2,
    n_hom = 1,
    beta_het = 2.92857142857143,
    se_het = 0.580288457473997,
    pval_het = 0.0370916457829073,
    lower_het = 1.79122695127809,
    upper_het = 4.06591590586477,
    beta_hom = 4.21428571428571,
    se_hom = 0.820651806648289,
    pval_hom = 0.0358910808364643,
    lower_hom = 2.60583772940734,
    upper_hom = 5.82273369916409)

  expect_equal(resObs, resExp)
})


test_that('getGeneticAssociations output (additive)', {

  genotypesTest = data.table(
    person_id = 1:6, snp1 = c(2, 1, 1, 0, 0, 0),
    snp2 = c(0, 1, 0, 0, 0, 1))
  diseaseGeneVarMapTest = data.table(
    disease_id = c(1, 2), gene = c('a', 'b'), vid = c('snp1', 'snp2'))
  resObs = getGeneticAssociations(scoresTest, genotypesTest, demosTest2,
                                diseaseGeneVarMapTest, formTest,
                                modelType = 'additive')
  resExp = data.table(
    disease_id = c(1, 2),
    vid = c('snp1', 'snp2'),
    n_total = c(6, 6),
    n_wt = c(3, 4),
    n_het = c(2, 2),
    n_hom = c(1, 0),
    beta = c(2.3125, -0.25),
    se = c(0.443412430287349, 1.18145390656315),
    pval = c(0.0137074787112402, 0.845976303468274),
    lower = c(1.44342760633942, -2.56560710625793),
    upper = c(3.18157239366058, 2.06560710625793))

  expect_equal(resObs, resExp)
})
