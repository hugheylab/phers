checkDemos = function(demos) {
  assertDataTable(demos)
  assertNames(colnames(demos), must.include = 'person_id',
              disjunct.from = c('phecode', 'w', 'disease_id', 'score'))
  assert(anyDuplicated(demos$person_id) == 0)
  invisible()}


checkPhecodeOccurrences = function(phecodeOccurrences, demos) {
  assertDataTable(phecodeOccurrences)
  assertNames(colnames(phecodeOccurrences),
              must.include = c('person_id', 'phecode'),
              disjunct.from = c('w', 'disease_id'))
  assertSubset(phecodeOccurrences$person_id,
               demos$person_id, empty.ok = FALSE)
  assertCharacter(phecodeOccurrences$phecode)
  invisible()}


checkWeights = function(weights) {
  assertDataTable(weights)
  assertNames(colnames(weights), must.include = c('phecode', 'w'),
              disjunct.from = c('person_id', 'disease_id'))
  assertCharacter(weights$phecode)
  assertNumeric(weights$w, finite = TRUE)
  assert(anyDuplicated(weights$phecode) == 0)
  invisible()}


checkDiseasePhecodeMap = function(diseasePhecodeMap) {
  assertDataTable(diseasePhecodeMap)
  assertNames(colnames(diseasePhecodeMap),
              must.include = c('disease_id', 'phecode'),
              disjunct.from = c('id', 'person_id', 'w'))
  assertCharacter(diseasePhecodeMap$phecode)
  assert(anyDuplicated(diseasePhecodeMap[, c('disease_id', 'phecode')]) == 0)
  invisible()}


checkScores = function(scores) {
  assertDataTable(scores)
  assertNames(
    colnames(scores), must.include = c('person_id', 'disease_id', 'score'))
  assertNumeric(scores$score, finite = TRUE)
  assert(anyDuplicated(scores[, c('person_id', 'disease_id')]) == 0)
  invisible()}


checkGenotypes = function(genotypes) {
  assertDataTable(genotypes)
  assertNames(colnames(genotypes), must.include = 'person_id',
              disjunct.from = c('score'))
  assert(anyDuplicated(genotypes$person_id) == 0)
  invisible()}


checkDiseaseGeneVarMap = function(diseaseGeneVarMap, scores) {
  assertDataTable(diseaseGeneVarMap)
  assertNames(
    colnames(diseaseGeneVarMap), permutation.of = c('disease_id', 'gene', 'vid'))
  assert(anyDuplicated(diseaseGeneVarMap[, c('disease_id', 'vid')]) == 0)
  assertSubset(scores$disease_id,
               diseaseGeneVarMap$disease_id, empty.ok = FALSE)
  invisible()}


checkGlmFormula = function(glmFormula, demos) {
  assertFormula(glmFormula)
  assertSubset(all.vars(glmFormula), colnames(demos))
  formVars = all.vars(glmFormula)
  invisible()}


checkModelType = function(modelType) {
  assertCharacter(modelType)
  assert(modelType == 'genotypic' | modelType == 'additive')
  invisible()}


checkLmInput = function(lmInput) {

  assertDataTable(lmInput)
  assertNames(colnames(lmInput), must.include = c('score', 'allele_count'))
  assertNumeric(lmInput$allele_count, finite = TRUE)
  assertSubset(unique(lmInput$allele_count), c(0,1,2))

  invisible()}
