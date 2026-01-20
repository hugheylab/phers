# Calculate phenotype risk scores

A person's phenotype risk score for a given disease corresponds to the
sum of the weights of the disease-relevant phecodes that the person has
received.

## Usage

``` r
getScores(weights, diseasePhecodeMap)
```

## Arguments

- weights:

  A data.table of phecodes and their corresponding weights. Must have
  columns `person_id`, `phecode` and `w`.

- diseasePhecodeMap:

  A data.table of the mapping between diseases and phecodes. Must have
  columns `disease_id` and `phecode`.

## Value

A data.table containing the phenotype risk score for each person for
each disease.

## See also

[`mapDiseaseToPhecode()`](https://phers.hugheylab.org/reference/mapDiseaseToPhecode.md),
[`getPhecodeOccurrences()`](https://phers.hugheylab.org/reference/getPhecodeOccurrences.md),
[`getWeights()`](https://phers.hugheylab.org/reference/getWeights.md),
[`getResidualScores()`](https://phers.hugheylab.org/reference/getResidualScores.md)

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
