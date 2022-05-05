example1 = function() {
  ex = "
@examples
library('data.table')

# map ICD codes to phecodes
phecodeOccurrences = getPhecodeOccurrences(icdSample)

# calculate weights
weights = getWeights(demoSample, phecodeOccurrences)

# OMIM disease IDs for which to calculate phenotype risk scores
diseaseId = 154700

# map diseases to phecodes
diseasePhecodeMap = mapDiseaseToPhecode()

# calculate scores
scores = getScores(
  demoSample, phecodeOccurrences, weights, diseasePhecodeMap[disease_id == diseaseId])

# calculate residual scores
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
library('BEDMatrix')

# map ICD codes to phecodes
phecodeOccurrences = getPhecodeOccurrences(icdSample)

# calculate weights
weights = getWeights(demoSample, phecodeOccurrences)

# OMIM disease IDs for which to calculate phenotype risk scores
diseaseId = 154700

# map diseases to phecodes
diseasePhecodeMap = mapDiseaseToPhecode()

# calculate scores
scores = getScores(
  demoSample, phecodeOccurrences, weights, diseasePhecodeMap[disease_id == diseaseId])

# map diseases to genetic variants
nvar = 10
diseaseVariantMap = data.table(disease_id = diseaseId, vid = paste0('snp', 1:nvar))

# load sample genetic data
npop = 50
genoSample = BEDMatrix(system.file('extdata', 'geno_sample.bed', package = 'phers'))
colnames(genoSample) = paste0('snp', 1:nvar)
rownames(genoSample) = 1:npop

# run genetic association tests
genoStats = getGeneticAssociations(
  scores, genoSample, demoSample, diseaseVariantMap, glmFormula = ~ sex,
  modelType = 'additive')
"
  return(strsplit(ex, split = '\n')[[1L]])}


example4 = function() {
  ex = "
@examples
library('data.table')

# OMIM disease IDs for which to calculate phenotype risk scores
diseaseId = 154700

# map diseases to phecodes
diseasePhecodeMap = mapDiseaseToPhecode()
diseasePhecodeMap = diseasePhecodeMap[disease_id == diseaseId]

# calculate raw and residal scores using weights based on the sample cohort
scores = phers(
  demoSample, icdSample, diseasePhecodeMap, residScoreFormula = ~ sex)

# calculate scores using pre-calculated weights
scores = phers(
  demoSample, icdSample, diseasePhecodeMap,
  weights = phers::preCalcWeights, residScoreFormula = ~ sex)
"
  return(strsplit(ex, split = '\n')[[1L]])}
