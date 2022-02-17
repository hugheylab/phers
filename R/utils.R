checkDemos = function(demos) {
  assertDataTable(demos)
  assertNames(colnames(demos), must.include = 'person_id',
              disjunct.from = c('phecode', 'w', 'disease_id'))
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


checkDiseasePhecodeMap = function(diseasePhecodeMap, weights) {
  assertDataTable(diseasePhecodeMap)
  assertNames(colnames(diseasePhecodeMap),
              must.include = c('disease_id', 'phecode'),
              disjunct.from = c('id', 'person_id', 'w'))
  assertCharacter(diseasePhecodeMap$phecode)
  assert(anyDuplicated(diseasePhecodeMap[, c('disease_id', 'phecode')]) == 0)
  invisible()}
