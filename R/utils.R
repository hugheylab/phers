checkDemos = function(demos) {
  assertDataTable(demos)
  assertNames(colnames(demos), must.include = 'person_id',
              disjunct.from = c('phecode', 'w', 'disease_id', 'score'))
  assert(anyDuplicated(demos$person_id) == 0)
  invisible()}


checkIcdOccurrences = function(
  icdOccurrences, cols = c('person_id', 'icd', 'flag')) {

  assertDataTable(icdOccurrences)
  assertNames(colnames(icdOccurrences),
              must.include = cols,
              disjunct.from = c('phecode', 'disease_id'))
  assertCharacter(icdOccurrences$icd)
  invisible()}


checkPhecodeOccurrences = function(phecodeOccurrences, demos) {
  assertDataTable(phecodeOccurrences)
  assertNames(colnames(phecodeOccurrences),
              must.include = c('person_id', 'phecode'),
              disjunct.from = c('w', 'disease_id'))

  coll = makeAssertCollection()
  assertSubset(phecodeOccurrences$person_id,
               demos$person_id, empty.ok = FALSE, add = coll)
  reportSubsetAssertions(phecodeOccurrences$person_id, demos$person_id, coll)

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

  if (!inherits(genotypes, 'BEDMatrix')) {
    stop("genotype must be of type 'BEDMatrix'.")}

  assertNames(colnames(genotypes), disjunct.from = c('score'))
  assertCharacter(rownames(genotypes))
  assert(anyDuplicated(rownames(genotypes)) == 0)
  invisible()}


checkDiseaseVariantMap = function(diseaseVariantMap, scores, genotypes) {
  assertDataTable(diseaseVariantMap)
  assertNames(
    colnames(diseaseVariantMap), must.include = c('disease_id', 'vid'))
  assert(anyDuplicated(diseaseVariantMap[, c('disease_id', 'vid')]) == 0)

  coll = makeAssertCollection()
  assertSubset(diseaseVariantMap$disease_id, scores$disease_id,
               empty.ok = FALSE, add = coll)
  reportSubsetAssertions(diseaseVariantMap$disease_id, scores$disease_id, coll)

  coll = makeAssertCollection()
  assertSubset(diseaseVariantMap$vid, colnames(genotypes),
               empty.ok = FALSE, add = coll)
  reportSubsetAssertions(diseaseVariantMap$vid, colnames(genotypes), coll)

  invisible()}


checkGlmFormula = function(glmFormula, demos) {
  assertFormula(glmFormula)
  assertNames(
    all.vars(glmFormula), subset.of = colnames(demos),
    disjunct.from = c('score', 'allele_count', 'person_id', 'disease_id'))

  if (all.vars(update.formula(glmFormula, . ~ 1)) != '.') {
    stop('provide a formula without the dependent variable')}

  invisible()}


checkLmInput = function(lmInput) {

  assertDataTable(lmInput)
  assertNames(colnames(lmInput), must.include = c('score', 'allele_count'))
  assertNumeric(lmInput$allele_count, finite = TRUE)
  assertSubset(unique(lmInput$allele_count), c(0, 1, 2))
  invisible()}


getAlleleCounts = function(lmInput) {
  n_het = n_hom = n_wt = NULL

  dCounts = data.table(n_total = nrow(lmInput))
  dCounts[, n_wt := sum(lmInput$allele_count == 0)]
  dCounts[, n_het := sum(lmInput$allele_count == 1)]
  dCounts[, n_hom := sum(lmInput$allele_count == 2)]
return(dCounts)}


reportSubsetAssertions = function(x, choices, coll) {
  assertClass(coll, 'AssertCollection')
  if (!coll$isEmpty()) {
    msg1 = paste0(vname(x), ' must be a subset of ', vname(choices))
    stop(msg1, call. = FALSE)}
  invisible(TRUE)}
