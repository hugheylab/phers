example1 = function() {
  ex = "

@examples
library('data.table')

# map ICD codes to phecodes
phecodeOccurrences = getPhecodeOccurrences(icdSample)

# calculate weights
weights = getWeights(demoSample, phecodeOccurrences)

# vector of OMIM disease IDs to calculate PheRS for
diseaseIds = c(154700)

# mape diseases to phecodes
diseasePhecodeMap = mapDiseaseToPhecode()

# calculate PheRS
scores = getScores(
  demoSample, phecodeOccurrences, weights, diseasePhecodeMap[disease_id %in% diseaseIds])

# calculate residual PheRS
rscores = getResidualScores(demoSample, scores, glmFormula = as.formula(~ sex))

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
diseaseIds = c(154700)

# mape diseases to phecodes
diseasePhecodeMap = mapDiseaseToPhecode()

# calculate PheRS
scores = getScores(
  demoSample, phecodeOccurrences, weights, diseasePhecodeMap[disease_id %in% diseaseIds])

# create a map of diseases and variants
diseaseGeneMap = data.table(disease_id = 154700, gene = 'FBN1')
geneVarMap = data.table(gene = 'FBN1', vid = paste0('snp', 1:20))
diseaseVariantMap = merge(diseaseGeneMap, geneVarMap, by = 'gene')

# run genetic association tests
genoStats = getGeneticAssociations(
  scores, genoSample, demoSample, diseaseVariantMap, glmFormula = as.formula(~ sex),
  modelType = 'additive')

  "
  return(strsplit(ex, split = '\n')[[1L]])}
