library('data.table')
library('glue')
library('BEDMatrix')

rawDir = 'data-raw'

########################
# ICD code to phecode map

icdPhecodeMap = fread(file.path(rawDir, 'icd_phecode_map.csv.gz'),
                      colClasses = list(character = c('icd', 'phecode')))
usethis::use_data(icdPhecodeMap, overwrite = TRUE)

#######################
# OMIM disease ID to HPO term map

diseaseHpoMap = fread(file.path(rawDir, 'disease_hpo_map_omim.csv.gz'))
usethis::use_data(diseaseHpoMap, overwrite = TRUE)

#######################
# HPO term to phecode map

hpoPhecodeMap = fread(file.path(rawDir, 'hpo_phecode_map.csv.gz'),
                      colClasses = list(character = 'phecode'))
usethis::use_data(hpoPhecodeMap, overwrite = TRUE)

#######################
# OMIM disease ID to diagnosic ICD code map

diseaseDxIcdMap = fread(
  file.path(rawDir, 'disease_dx_icd_map_omim.csv.gz'),
  colClasses = list(character = 'icd'))

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
npop = 50
set.seed(1)


# demographic data
demoSample = data.table(
  person_id = 1:npop,
  sex = sample(c('male', 'female'),
               size = npop, replace = TRUE, prob = c(0.5, 0.5)))
usethis::use_data(demoSample, overwrite = TRUE)


# ICD codes
maxIcdCount = 5
npopAll = 45
npopMarfan = 4
marfanId = 154700
flag1 = 9
IcdCodes = icdPhecodeMap[flag == flag1 & !(icd %in% diseaseDxIcdMap[disease_id == marfanId]$icd)]$icd
entryDates = seq(as.Date('2000/01/01'), as.Date('2010/01/01'), by = 'day')

icdCounts = replicate(npopAll,  sample(1:maxIcdCount, 1))
icdSampleAll = lapply(
  icdCounts, sample, x = IcdCodes, replace = TRUE)

icdSampleAll = data.table(
  person_id = rep(npopMarfan + 1:npopAll, icdCounts),
  icd = unlist(icdSampleAll), flag = flag1)
icdSampleAll = icdSampleAll[, entry_date := sample(entryDates, nrow(icdSampleAll))]

icdSampleMarfan = data.table(
  person_id = c(rep(1L, 4), rep(2L, 5), rep(3L, 2), 4),
  icd = c('759.82', '365', '366', '734', '759.82', '759.82',
          '524.0', '718.4', '441', '366', '734', '441'),
  flag = flag1, entry_date = sample(entryDates, nrow(icdSampleMarfan)))

icdSample = rbind(icdSampleMarfan, icdSampleAll)

usethis::use_data(icdSample, overwrite = TRUE)


# genotype data
nvar = 10
alleleFreq = 0.1
plinkSimPath = file.path(rawDir, 'wgas.sim')
plinkOut = file.path(rawDir, 'geno_sample')

plinkSim = glue('{nvar} snp {alleleFreq} {alleleFreq} 1 1')
writeLines(plinkSim, plinkSimPath)
plinkCmnd = glue('--simulate {plinkSimPath} --make-bed --out {plinkOut} --simulate-ncases {npop/2} --simulate-ncontrols {npop/2} --seed 1')
system(glue('~/plink_mac_20220402/plink {plinkCmnd}'))

genoSample = BEDMatrix(file.path(rawDir, 'geno_sample'))
colnames(genoSample) = paste0('snp', 1:nvar)
rownames(genoSample) = 1:npop

usethis::use_data(genoSample, overwrite = TRUE)
