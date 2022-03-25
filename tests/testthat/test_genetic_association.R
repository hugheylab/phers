test_that('runLinear output (additive)', {

  dObs = phers:::runLinear(lmInputTest, formTest, modelType = 'additive', 1, 'snp1')
  dExp = data.table(
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

  expect_equal(dObs, dExp)
})


test_that('runLinear output (genotypic)', {

  linearOut = runLinear(lmInputTest, formTest, modelType = 'genotypic', 1, 'snp1')

  expect_s3_class(linearOut, 'data.table')
  expect_equal(nrow(linearOut), 1)
  expect_named(
    linearOut, c('disease_id', 'vid', 'n_total', 'n_wt', 'n_het', 'n_hom',
                 'beta_het', 'se_het', 'pval_het', 'lower_het', 'upper_het',
                 'beta_hom', 'se_hom', 'pval_hom', 'lower_hom', 'upper_hom'),
               ignore.order = TRUE)

  lmInputTestG = copy(lmInputTest)
  lmInputTestG[, allele_count := factor(allele_count)]
  lmTest = glm(score ~ allele_count + sex, data = lmInputTestG)

  expect_equal(linearOut$disease_id, 1)
  expect_equal(linearOut$vid, 'snp1')
  expect_equal(linearOut$n_total, 6)
  expect_equal(linearOut$n_wt, 3)
  expect_equal(linearOut$n_het, 2)
  expect_equal(linearOut$n_hom, 1)

  expect_equal(linearOut$beta_het, lmTest$coef[['allele_count1']])
  expect_equal(linearOut$se_het,
               summary(lmTest)$coef['allele_count1', 'Std. Error'])
  expect_equal(linearOut$pval_het,
               summary(lmTest)$coef['allele_count1', 'Pr(>|t|)'])
  expect_equal(
    linearOut$lower_het, suppressMessages(confint(lmTest)['allele_count1', '2.5 %']))
  expect_equal(
    linearOut$upper_het, suppressMessages(confint(lmTest)['allele_count1', '97.5 %']))
})


test_that('getGeneticAssociations output (additive)', {

  genoOut = getGeneticAssociations(scoresTest, genotypesTest, demosTest2,
                                diseaseGeneVarMapTest, formTest,
                                modelType = 'additive')

  expect_s3_class(genoOut, 'data.table')
  expect_equal(nrow(genoOut), 2)
  expect_named(
    genoOut,
    c('disease_id', 'vid', 'n_total', 'n_wt', 'n_het', 'n_hom',
      'beta', 'se', 'pval', 'lower', 'upper'),
    ignore.order = TRUE)

  lmInputTest2 = merge(
    merge(scoresTest, demosTest2, by = 'person_id'),
    genotypesTest, by = 'person_id')
  lmTest2 = glm(score ~ snp2 + sex, data = lmInputTest2[disease_id == 2])

  expect_equal(genoOut$disease_id, c(1, 2))
  expect_equal(genoOut$vid, c('snp1', 'snp2'))
  expect_equal(genoOut$n_total, c(6, 6))
  expect_equal(genoOut$n_wt, c(3, 4))
  expect_equal(genoOut$n_het, c(2, 2))
  expect_equal(genoOut$n_hom, c(1, 0))

  expect_equal(genoOut$beta[2], lmTest2$coef[['snp2']])
  expect_equal(genoOut$se[2],
               summary(lmTest2)$coef['snp2', 'Std. Error'])
  expect_equal(genoOut$pval[2],
               summary(lmTest2)$coef['snp2', 'Pr(>|t|)'])
  expect_equal(
    genoOut$lower[2], suppressMessages(confint(lmTest2)['snp2', '2.5 %']))
  expect_equal(
    genoOut$upper[2], suppressMessages(confint(lmTest2)['snp2', '97.5 %']))


})
