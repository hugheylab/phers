test_that('runLinear output (additive)', {

  linearOut = runLinear(lmInputTest, formStrTest, modelType = 'additive')

  expect_s3_class(linearOut, 'data.table')
  expect_equal(nrow(linearOut), 1)
  expect_named(linearOut, c('beta', 'se', 'p', 'lower', 'upper'),
               ignore.order = TRUE)

  lmTest = glm(score~allele_count+sex, data = lmInputTest)

  expect_equal(linearOut$beta, lmTest$coef[['allele_count']])
  expect_equal(linearOut$se,
               summary(lmTest)$coef['allele_count', 'Std. Error'])
  expect_equal(linearOut$p,
               summary(lmTest)$coef['allele_count', 'Pr(>|t|)'])
  expect_equal(
    linearOut$lower, suppressMessages(confint(lmTest)['allele_count', '2.5 %']))
  expect_equal(
    linearOut$upper, suppressMessages(confint(lmTest)['allele_count', '97.5 %']))
})


test_that('runLinear output (genotypic)', {

  linearOut = runLinear(lmInputTest, formStrTest, modelType = 'genotypic')

  expect_s3_class(linearOut, 'data.table')
  expect_equal(nrow(linearOut), 1)
  expect_named(
    linearOut, c('beta_het', 'se_het', 'p_het', 'lower_het', 'upper_het',
                 'beta_hom', 'se_hom', 'p_hom', 'lower_hom', 'upper_hom'),
               ignore.order = TRUE)

  lmInputTestG = copy(lmInputTest)
  lmInputTestG[, allele_count := factor(allele_count)]
  lmTest = glm(score~allele_count+sex, data = lmInputTestG)

  expect_equal(linearOut$beta_het, lmTest$coef[['allele_count1']])
  expect_equal(linearOut$se_het,
               summary(lmTest)$coef['allele_count1', 'Std. Error'])
  expect_equal(linearOut$p_het,
               summary(lmTest)$coef['allele_count1', 'Pr(>|t|)'])
  expect_equal(
    linearOut$lower_het, suppressMessages(confint(lmTest)['allele_count1', '2.5 %']))
  expect_equal(
    linearOut$upper_het, suppressMessages(confint(lmTest)['allele_count1', '97.5 %']))
})


test_that('genotypeAssociation output (additive)', {

  genoOut = genotypeAssociation(scoresTest, genotypesTest, demosTest2,
                                diseaseGeneVarMapTest, formStrTest,
                                modelType = 'additive')

  expect_s3_class(genoOut, 'data.table')
  expect_equal(nrow(genoOut), 2)
  expect_named(
    genoOut,
    c('disease_id', 'gene', 'vid', 'n_total', 'n_wild', 'n_het', 'n_hom',
      'beta', 'se', 'p', 'lower', 'upper'),
    ignore.order = TRUE)

  lmInputTest2 = merge(
    merge(scoresTest, demosTest2, by = 'person_id') ,
    genotypesTest, by = 'person_id')
  lmTest2 = glm(score~snp2+sex, data = lmInputTest2[disease_id == 2])

  expect_equal(genoOut$disease_id, c(1, 2))
  expect_equal(genoOut$gene, c('a', 'b'))
  expect_equal(genoOut$vid, c('snp1', 'snp2'))
  expect_equal(genoOut$n_total, c(6, 6))
  expect_equal(genoOut$n_wild, c(3, 4))
  expect_equal(genoOut$n_het, c(2, 2))
  expect_equal(genoOut$n_hom, c(1, 0))

  expect_equal(genoOut$beta[2], lmTest2$coef[['snp2']])
  expect_equal(genoOut$se[2],
               summary(lmTest2)$coef['snp2', 'Std. Error'])
  expect_equal(genoOut$p[2],
               summary(lmTest2)$coef['snp2', 'Pr(>|t|)'])
  expect_equal(
    genoOut$lower[2], suppressMessages(confint(lmTest2)['snp2', '2.5 %']))
  expect_equal(
    genoOut$upper[2], suppressMessages(confint(lmTest2)['snp2', '97.5 %']))


})


