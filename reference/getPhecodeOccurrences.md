# Map ICD code occurrences to phecode occurrences

This is typically the first step of an analysis using phenotype risk
scores, the next is
[`getWeights()`](https://phers.hugheylab.org/reference/getWeights.md).

## Usage

``` r
getPhecodeOccurrences(
  icdOccurrences,
  icdPhecodeMap = phers::icdPhecodeMap,
  dxIcd = phers::diseaseDxIcdMap
)
```

## Arguments

- icdOccurrences:

  A data.table of occurrences of ICD codes for each person in the
  cohort. Must have columns `person_id`, `icd`, and `flag`.

- icdPhecodeMap:

  A data.table of the mapping between ICD codes and phecodes. Must have
  columns `icd`, `phecode`, and `flag`. Default is the map included in
  this package.

- dxIcd:

  A data.table of ICD codes to exclude from mapping to phecodes. Must
  have columns `icd` and `flag`. Default is the table of Mendelian
  diseases and the corresponding ICD codes that indicate a genetic
  diagnosis. If `NULL`, no ICD codes will be excluded.

## Value

A data.table of phecode occurrences for each person.

## See also

[`getWeights()`](https://phers.hugheylab.org/reference/getWeights.md),
[`getScores()`](https://phers.hugheylab.org/reference/getScores.md)

## Examples

``` r
library('data.table')

# map ICD codes to phecodes
phecodeOccurrences = getPhecodeOccurrences(icdSample)

# calculate weights (using the prevalence method)
weights = getWeights(demoSample, phecodeOccurrences)

# OMIM disease IDs for which to calculate phenotype risk scores
diseaseId = 154700

# map diseases to phecodes
diseasePhecodeMap = mapDiseaseToPhecode()

# calculate scores
scores = getScores(weights, diseasePhecodeMap[disease_id == diseaseId])

# calculate residual scores
rscores = getResidualScores(demoSample, scores, lmFormula = ~ sex)
```
