checkDemos = function(demos) {
  assertDataTable(demos)
  assertNames(
    colnames(demos), type = 'unique', must.include = 'person_id',
    disjunct.from = c('phecode', 'w', 'disease_id', 'score'))
  assert(anyDuplicated(demos$person_id) == 0)
  invisible()}


checkDxIcd = function(dxIcd, nullOk) {
  assertDataTable(dxIcd, null.ok = nullOk)
  cols = if (nullOk) c('icd', 'flag') else c('disease_id', 'icd', 'flag')
  if (!is.null(dxIcd)) {
    assertNames(
      colnames(dxIcd), type = 'unique', must.include = cols,
      disjunct.from = 'person_id')
    assertCharacter(dxIcd$icd)}
  invisible()}


checkIcdPhecodeMap = function(icdPhecodeMap) {
  assertDataTable(icdPhecodeMap)
  assertNames(
    colnames(icdPhecodeMap), type = 'unique',
    must.include = c('phecode', 'icd', 'flag'))
  assertCharacter(icdPhecodeMap$icd)
  assertCharacter(icdPhecodeMap$phecode)
  assert(anyDuplicated(icdPhecodeMap) == 0)
  invisible()}


checkIcdOccurrences = function(
  icdOccurrences, cols = c('person_id', 'icd', 'flag')) {
  assertDataTable(icdOccurrences)
  assertNames(
    colnames(icdOccurrences), type = 'unique', must.include = cols,
    disjunct.from = c('phecode', 'disease_id'))
  assertCharacter(icdOccurrences$icd)
  invisible()}


checkPhecodeOccurrences = function(phecodeOccurrences, demos) {
  assertDataTable(phecodeOccurrences)
  assertNames(
    colnames(phecodeOccurrences), type = 'unique',
    must.include = c('person_id', 'phecode'),
    disjunct.from = c('w', 'disease_id'))

  coll = makeAssertCollection()
  assertSubset(
    phecodeOccurrences$person_id, demos$person_id, empty.ok = FALSE, add = coll)
  reportSubsetAssertions(phecodeOccurrences$person_id, demos$person_id, coll)

  assertCharacter(phecodeOccurrences$phecode)
  invisible()}


checkWeights = function(weights) {
  assertDataTable(weights)
  assertNames(
    colnames(weights), type = 'unique', must.include = c('phecode', 'w'),
    disjunct.from = c('person_id', 'disease_id'))
  assertCharacter(weights$phecode)
  assertNumeric(weights$w, finite = TRUE)
  assert(anyDuplicated(weights$phecode) == 0)
  invisible()}


checkDiseasePhecodeMap = function(diseasePhecodeMap) {
  assertDataTable(diseasePhecodeMap)
  assertNames(
    colnames(diseasePhecodeMap), type = 'unique',
    must.include = c('disease_id', 'phecode'),
    disjunct.from = c('id', 'person_id', 'w'))
  assertCharacter(diseasePhecodeMap$phecode)
  assert(anyDuplicated(diseasePhecodeMap[, c('disease_id', 'phecode')]) == 0)
  invisible()}


checkScores = function(scores) {
  assertDataTable(scores)
  assertNames(
    colnames(scores), type = 'unique',
    must.include = c('person_id', 'disease_id', 'score'))
  assertNumeric(scores$score, finite = TRUE)
  assert(anyDuplicated(scores[, c('person_id', 'disease_id')]) == 0)
  invisible()}


checkGenotypes = function(genotypes) {
  assertMultiClass(genotypes, c('BEDMatrix', 'matrix'))
  assertNames(rownames(genotypes), type = 'unique')
  assertNames(colnames(genotypes), type = 'unique', disjunct.from = 'score')
  invisible()}


checkDiseaseVariantMap = function(diseaseVariantMap, scores, genotypes) {
  assertDataTable(diseaseVariantMap)
  assertNames(
    colnames(diseaseVariantMap), type = 'unique',
    must.include = c('disease_id', 'variant_id'))
  assert(anyDuplicated(diseaseVariantMap[, c('disease_id', 'variant_id')]) == 0)

  coll = makeAssertCollection()
  assertSubset(
    diseaseVariantMap$disease_id, scores$disease_id, empty.ok = FALSE, add = coll)
  reportSubsetAssertions(diseaseVariantMap$disease_id, scores$disease_id, coll)

  coll = makeAssertCollection()
  assertSubset(
    diseaseVariantMap$variant_id, colnames(genotypes), empty.ok = FALSE, add = coll)
  reportSubsetAssertions(diseaseVariantMap$variant_id, colnames(genotypes), coll)

  invisible()}


checkLmFormula = function(lmFormula, demos) {
  assertFormula(lmFormula)
  assertNames(
    all.vars(lmFormula), subset.of = colnames(demos),
    disjunct.from = c('score', 'allele_count', 'person_id', 'disease_id'))

  if (all.vars(update.formula(lmFormula, . ~ 1)) != '.') {
    stop('The formula contains a dependent variable, which is not allowed.')}

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
