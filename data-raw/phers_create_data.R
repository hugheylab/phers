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

#######################
# map of genes and variants

geneVarMap = fread(file.path('data-raw', 'gene_variant_map.csv.gz'),
                   select = c('Gene', 'SNP', 'rsID'))
setnames(geneVarMap, c('Gene', 'SNP', 'rsID'), c('gene', 'vid', 'rsid'))
usethis::use_data(geneVarMap, overwrite = TRUE)

#######################
# table of disease info (map of disease and gene)

diseaseInfo = fread(file.path('data-raw', 'disease_info.csv.gz'),
                    select = c('dID', 'disease', 'gene', 'sex_inc', 'skip'))
diseaseInfo = unique(
  diseaseInfo[skip == 0][sex_inc == 'B'][, .(dID, disease, gene)])
setnames(diseaseInfo, c('dID', 'disease'),
         c('disease_id', 'disease_name'))

diseaseInfo = diseaseInfo[, .(disease_id, disease_name, gene)]

usethis::use_data(diseaseInfo, overwrite = TRUE)


