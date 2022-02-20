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
usethis::use_data(hpoPhecodeMap, overwrite = TRUE)
