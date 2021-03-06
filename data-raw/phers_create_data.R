library('data.table')

rawDir = 'data-raw'

########################
# ICD code to phecode map

icdPhecodeMap = fread(
  file.path(rawDir, 'icd_phecode_map.csv.gz'),
  colClasses = list(character = c('icd', 'phecode')))
setkeyv(icdPhecodeMap, c('icd', 'flag', 'phecode'))

usethis::use_data(icdPhecodeMap, overwrite = TRUE)

#######################
# OMIM disease ID to HPO term map

diseaseHpoMap = fread(file.path(rawDir, 'disease_hpo_map_omim.csv.gz'))
setkeyv(diseaseHpoMap, c('disease_id', 'hpo_term_id'))

usethis::use_data(diseaseHpoMap, overwrite = TRUE)

#######################
# HPO term to phecode map

hpoPhecodeMap = fread(
  file.path(rawDir, 'hpo_phecode_map.csv.gz'),
  colClasses = list(character = 'phecode'))
setkeyv(hpoPhecodeMap, c('hpo_term_id', 'phecode'))

usethis::use_data(hpoPhecodeMap, overwrite = TRUE)

#######################
# OMIM disease ID to diagnosic ICD code map

diseaseDxIcdMap = fread(
  file.path(rawDir, 'disease_dx_icd_map_omim.csv.gz'),
  colClasses = list(character = 'icd'))
setkeyv(diseaseDxIcdMap, c('disease_id', 'icd', 'flag'))

usethis::use_data(diseaseDxIcdMap, overwrite = TRUE)

#######################
# pre-calculated weights

# queries for extracting ICD code and demographic data from
# Vanderbilt University Medical Center Synthetic Derivative
# are stored in data-raw/all_icds_sd.sql and data-raw/all_demos_sd.sql

preCalcWeights = fread(
  file.path(rawDir, 'pre_calculated_weights.csv.gz'),
  colClasses = list(character = 'phecode'))
setkeyv(preCalcWeights, 'phecode')

usethis::use_data(preCalcWeights, overwrite = TRUE)

#######################
# sample demographic, ICD code, and genotype data
npop = 50
set.seed(1)

# demographic data
demoSample = data.table(
  person_id = 1:npop,
  sex = sample(
    c('male', 'female'), size = npop, replace = TRUE, prob = c(0.5, 0.5)))
setkeyv(demoSample, 'person_id')

usethis::use_data(demoSample, overwrite = TRUE)

# ICD codes
maxIcdCount = 5
npopAll = 45
npopMarfan = 4
marfanId = 154700
flag1 = 9
icdCodes = icdPhecodeMap[
  flag == flag1 & !(icd %in% diseaseDxIcdMap[disease_id == marfanId]$icd)]$icd
entryDates = seq(as.Date('2000/01/01'), as.Date('2010/01/01'), by = 'day')

icdCounts = replicate(npopAll,  sample(1:maxIcdCount, 1))
icdSampleAll = lapply(
  icdCounts, sample, x = icdCodes, replace = TRUE)

icdSampleAll = data.table(
  person_id = rep(npopMarfan + 1:npopAll, icdCounts),
  icd = unlist(icdSampleAll), flag = flag1)
icdSampleAll[, entry_date := sample(entryDates, nrow(icdSampleAll))]

icdSampleMarfan = data.table(
  person_id = c(rep(1L, 4), rep(2L, 5), rep(3L, 2), 4),
  icd = c('759.82', '365', '366', '734', '759.82', '759.82',
          '524.0', '718.4', '441', '366', '734', '441'),
  flag = flag1)
icdSampleMarfan[, entry_date := sample(entryDates, nrow(icdSampleMarfan))]

icdSample = rbind(icdSampleMarfan, icdSampleAll)
setkey(icdSample)

usethis::use_data(icdSample, overwrite = TRUE)
