library('data.table')

rawDir = 'data-raw'

########################
# map of icd and phecode

icdPhecodeMap = fread(file.path(rawDir, 'icd_phecode_map.csv.gz'),
                      colClasses = list(character = c('icd', 'phecode')))
usethis::use_data(icdPhecodeMap, overwrite = TRUE)

#######################
# map of disease ID and HPO terms

diseaseHpoMap = fread(file.path(rawDir, 'disease_hpo_map_omim.csv.gz'))
usethis::use_data(diseaseHpoMap, overwrite = TRUE)

#######################
# map of HPO terms and phecodes

hpoPhecodeMap = fread(file.path(rawDir, 'hpo_phecode_map.csv.gz'),
                      colClasses = list(character = 'phecode'))
hpoPhecodeMap = hpoPhecodeMap[phecode != '' & !is.na(phecode)]
usethis::use_data(hpoPhecodeMap, overwrite = TRUE)

#######################
# map of disease ID and diagnosis ICD codes

diseaseDxIcdMap = fread(
  file.path(rawDir, 'disease_dx_icd_map_omim.csv.gz'),
  select = c('dID', 'disease', 'ICD', 'ICD_string', 'flag'),
  colClasses = list(character = 'ICD'))

diseaseDxIcdMap = diseaseDxIcdMap[dID != '-']
diseaseDxIcdMap[, dID := as.numeric(dID)]

setnames(
  diseaseDxIcdMap, c('dID', 'disease', 'ICD', 'ICD_string'),
  c('disease_id', 'disease_name', 'icd', 'icd_string'))

usethis::use_data(diseaseDxIcdMap, overwrite = TRUE)


#######################
# pre-calculated weights

# add Netezza query to get demo and ICD data from SD.

# phecodeSD = getPhecodeOccurrences(icdSD)

demoSD = readRDS('~/Dropbox (VUMC)/TimeAwarePheRS/data/demo.rds')
setnames(demoSD, 'GRID', 'person_id')
phecodeSD = readRDS('~/Dropbox (VUMC)/TimeAwarePheRS/data/phecodes.rds')
setnames(phecodeSD, 'GRID', 'person_id')
phecodeSD = phecodeSD[person_id %in% demoSD$person_id]

preCalcWeights = getWeights(demoSD, phecodeSD)

usethis::use_data(preCalcWeights, overwrite = TRUE)


#######################
# sample demographic, ICD code, and genotype data

# demographic data
demoSample = data.table(
  person_id = 1:5,
  sex = c('female', 'male', 'male', 'female', 'female'))
usethis::use_data(demoSample, overwrite = TRUE)

# ICD codes
icdSample = data.table(
  person_id = c(rep(1L, 3), rep(2L, 4), rep(3L, 2), 4),
  icd = c('365', '366', '734', '759.82', '524.0', '718.4', '441', '366', '734', '441'),
  flag = 9)
usethis::use_data(icdSample, overwrite = TRUE)

# genotype data
set.seed(1)
npop = 5
gene1 = 'FBN1'
nvar = 10
genoSample = data.table(person_id = 1:npop)
genos = replicate(
  nvar, sample(c(0, 1, 2), replace = TRUE, size = npop, prob = c(80, 15, 5)))
colnames(genos) = paste0('snp', 1:nvar)
genos = as.data.table(genos)
genoSample = cbind(genoSample, genos)

usethis::use_data(genoSample, overwrite = TRUE)
