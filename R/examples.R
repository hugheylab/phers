example1 = function() {
  ex = "

@examples
library('data.table')

# map ICD codes to phecodes
phecodeOccurrences = getPhecodeOccurrences(icdSample)

# calculate weights
weights = getWeights(demoSample, phecodeOccurrences)

# vector of OMIM disease IDs to calculate PheRS for
diseaseId = 154700

# mape diseases to phecodes
diseasePhecodeMap = mapDiseaseToPhecode()

# calculate PheRS
scores = getScores(
  demoSample, phecodeOccurrences, weights, diseasePhecodeMap[disease_id == diseaseId])

# calculate residual PheRS
rscores = getResidualScores(demoSample, scores, glmFormula = ~ sex)

  "
  return(strsplit(ex, split = '\n')[[1L]])}


example2 = function() {
  ex = "

@examples
library('data.table')

dxStatus = getDxStatus(demoSample, icdSample)

  "
  return(strsplit(ex, split = '\n')[[1L]])}


example3 = function() {
  ex = "

@examples
library('data.table')

# map ICD codes to phecodes
phecodeOccurrences = getPhecodeOccurrences(icdSample)

# calculate weights
weights = getWeights(demoSample, phecodeOccurrences)

# vector of OMIM disease IDs to calculate PheRS for
diseaseId = 154700

# mape diseases to phecodes
diseasePhecodeMap = mapDiseaseToPhecode()

# calculate PheRS
scores = getScores(
  demoSample, phecodeOccurrences, weights, diseasePhecodeMap[disease_id == diseaseId])

# create a map of diseases and variants
diseaseVariantMap = data.table(disease_id = diseaseId, vid = paste0('snp', 1:20))

# run genetic association tests
genoStats = getGeneticAssociations(
  scores, genoSample, demoSample, diseaseVariantMap, glmFormula = ~ sex,
  modelType = 'additive')

  "
  return(strsplit(ex, split = '\n')[[1L]])}
