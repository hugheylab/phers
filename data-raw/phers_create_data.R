library(data.table)

# create sample demographic data

demoSample = data.table('person_ID'=c('1', '2', '3', '4', '5'))
demoSample[,sex:=c('F','M','M','F','F')]
demoSample[,uniq_age:=c(3,2,4,2,4)]
demoSample[,first_age:=rnorm(n=5, mean=30*365, sd=20*365)]
demoSample[,last_age:=rnorm(n=5, mean=50*365, sd=20*365)]

usethis::use_data(demoSample, overwrite = TRUE)


#######################
# create sample icd data (Marfan Syndrome phenotypes)

icdSample = data.table('person_ID'=c('1', '1', '1', '2', '2', '2', '3', '3', '4'),
                     'ICD'=c('365', '366', '734', '524.0', '718.4', '441',
                             '366', '734', '441'))
icdSample[,flag:=9]
usethis::use_data(icdSample, overwrite = TRUE)

########################
# map of icd and phecode

ICDPhecodeMap = fread(file.path('data-raw', 'ICD_phecode_map.csv.gz'),
                      colClasses = list(character=c('ICD', 'phecode')))
usethis::use_data(ICDPhecodeMap, overwrite = TRUE)

#######################
# map of disease ID and HPO terms

diseaseHPOMap = fread(file.path('data-raw', 'disease_HPO_map.csv.gz'))
usethis::use_data(diseaseHPOMap, overwrite = TRUE)

#######################
# map of HPO terms and phecodes

HPOPhecodeMap = fread(file.path('data-raw', 'HPO_phecode_map.csv.gz'),
                      colClasses = list(character=c('phecode')))
usethis::use_data(HPOPhecodeMap, overwrite = TRUE)





