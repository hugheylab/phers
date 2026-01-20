# Calculate residual phenotype risk scores

The residual score indicates to what extent a person's phenotype risk
score for a given disease deviates from the expected score, after
adjusting for the person's characteristics in a linear model.

## Usage

``` r
getResidualScores(demos, scores, lmFormula)
```

## Arguments

- demos:

  A data.table of characteristics for each person in the cohort. Must
  have column `person_id`.

- scores:

  A data.table containing the phenotype risk score for each person for
  each disease. Must have columns `person_id`, `disease_id`, and
  `score`.

- lmFormula:

  A formula representing the linear model to use for calculating
  residual scores. All terms in the formula must correspond to columns
  in `demos`.

## Value

A data.table, based on `scores`, with an additional column
`resid_score`. Residual scores for each disease are standardized to have
unit variance.

## See also

[`stats::rstandard()`](https://rdrr.io/r/stats/influence.measures.html),
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
