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

ageParams = data.table(
  min = c(0, 10, 25, 50, 70),
  max = c(10, 25, 50, 70, 90),
  prop = c(0.25, 0.15, 0.30, 0.20, 0.10))
recordLenParams = data.table(
  min = c(0, 0.001, 0.1, 1, 5, 10, 15, 20),
  max = c(0.001, 0.1, 1, 5, 10, 15, 20, 30),
  prop = c(0.25, 0.10, 0.15, 0.20, 0.14, 0.10, 0.05, 0.01))
ages = demoSample[, .(
  person_id,
  rowsF = sample(nrow(ageParams), npop, replace = TRUE, prob = ageParams$prop),
  rowsR = sample(nrow(recordLenParams), npop, replace = TRUE, prob = recordLenParams$prop))]
ages[, `:=`(first_age = ageParams$min[rowsF] + runif(npop) * (
  ageParams$max[rowsF] - ageParams$min[rowsF]),
  record_len = recordLenParams$min[rowsR] + runif(npop) * (
    recordLenParams$max[rowsR] - recordLenParams$min[rowsR]))]
ages[first_age + record_len > 90,
     record_len := record_len - (first_age + record_len - 90)]

lastDates = seq(as.Date('2010/01/01'), as.Date('2020/01/01'), by = 'day')
ages[, last_visit_date := sample(lastDates, npop)]
ages[, first_visit_date := last_visit_date - (record_len * 365)]
ages[, dob := first_visit_date - (first_age * 365)]

demoSample = merge(
  demoSample, ages[, .(person_id, dob, first_visit_date, last_visit_date)], by = 'person_id')

usethis::use_data(demoSample, overwrite = TRUE)

# ICD codes
maxIcdCount = 5
npopAll = 45
npopMarfan = 4
marfanId = 154700
flag1 = 9
icdCodes = icdPhecodeMap[
  flag == flag1 & !(icd %in% diseaseDxIcdMap[disease_id == marfanId]$icd)]$icd

icdCounts = replicate(npopAll,  sample(1:maxIcdCount, 1))
icdSampleAll = lapply(
  icdCounts, sample, x = icdCodes, replace = TRUE)

icdSampleAll = data.table(
  person_id = rep(npopMarfan + 1:npopAll, icdCounts),
  icd = unlist(icdSampleAll), flag = flag1)

icdSampleMarfan = data.table(
  person_id = c(rep(1L, 4), rep(2L, 5), rep(3L, 2), 4),
  icd = c('759.82', '365', '366', '734', '759.82', '759.82',
          '524.0', '718.4', '441', '366', '734', '441'),
  flag = flag1)

icdSample = rbind(icdSampleMarfan, icdSampleAll)

icdSample = merge(
  icdSample,
  demoSample[, .(person_id, first_visit_date, last_visit_date)],
  by = 'person_id')
icdSample[, entry_date := sample(
  seq(first_visit_date, last_visit_date, by = 'day'), 1), by = seq_len(nrow(icdSample))]
icdSample[, `:=`(first_visit_date = NULL, last_visit_date = NULL)]
setkey(icdSample)

usethis::use_data(icdSample, overwrite = TRUE)
