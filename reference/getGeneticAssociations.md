# Perform association tests between phenotype risk scores and genotypes

The association test for each disease-variant pair is based on a linear
model, with the phenotype risk score as the dependent variable.

## Usage

``` r
getGeneticAssociations(
  scores,
  genotypes,
  demos,
  diseaseVariantMap,
  lmFormula,
  modelType = c("genotypic", "additive", "dominant", "recessive"),
  level = 0.95,
  dopar = FALSE
)
```

## Arguments

- scores:

  A data.table of phenotype risk scores. Must have columns `person_id`,
  `disease_id`, `score`.

- genotypes:

  A matrix or 'BEDMatrix' object containing genetic data, with rownames
  corresponding to `person_id`s in `demos` and `scores`, and colnames
  corresponding to `variant_id`s in `diseaseVariantMap`.

- demos:

  A data.table of characteristics for each person in the cohort. Must
  have column `person_id`.

- diseaseVariantMap:

  A data.table indicating which genetic variants to test for association
  with phenotype risk scores for which diseases. Must have columns
  `disease_id` and `variant_id`.

- lmFormula:

  A formula representing the linear model (excluding the term for
  genotype) to use for the association tests. All terms in the formula
  must correspond to columns in `demos`.

- modelType:

  A string indicating how to encode genotype in the model.

- level:

  A number indicating the level of the confidence interval. Default is
  0.95.

- dopar:

  Logical indicating whether to run calculations in parallel if a
  parallel backend is already set up, e.g., using
  [`doParallel::registerDoParallel()`](https://rdrr.io/pkg/doParallel/man/registerDoParallel.html).
  Recommended to minimize runtime.

## Value

A data.table of statistics for the association tests (if a model fails
to converge, NAs will be reported):

- `disease_id`: Disease identifier

- `variant_id`: Variant identifier

- `n_total`: Number of persons with non-missing genotype data for the
  given variant.

- `n_wt`: Number of persons homozygous for the wild-type allele.

- `n_het`: Number of persons having one copy of the alternate allele.

- `n_hom`: Number of persons homozygous for the alternate allele.

- `beta`: Coefficient for the association of genotype with score

- `se`: Standard error for `beta`

- `pval`: P-value for `beta` being non-zero

- `ci_lower`: Lower bound of the confidence interval for `beta`

- `ci_upper`: Upper bound of the confidence interval for `beta`

If `modelType` is "genotypic", the data.table will include separate
statistics for heterozygous and homozygous genotypes.

## See also

[`stats::lm()`](https://rdrr.io/r/stats/lm.html),
[`stats::confint()`](https://rdrr.io/r/stats/confint.html),
[`getScores()`](https://phers.hugheylab.org/reference/getScores.md)

## Examples

``` r
library('data.table')
library('BEDMatrix')

# map ICD codes to phecodes
phecodeOccurrences = getPhecodeOccurrences(icdSample)

# calculate weights
weights = getWeights(demoSample, phecodeOccurrences)

# OMIM disease IDs for which to calculate phenotype risk scores
diseaseId = 154700

# map diseases to phecodes
diseasePhecodeMap = mapDiseaseToPhecode()

# calculate scores
scores = getScores(weights, diseasePhecodeMap[disease_id == diseaseId])

# map diseases to genetic variants
nvar = 10
diseaseVariantMap = data.table(disease_id = diseaseId, variant_id = paste0('snp', 1:nvar))

# load sample genetic data
npop = 50
genoSample = BEDMatrix(system.file('extdata', 'geno_sample.bed', package = 'phers'))
#> Extracting number of samples and rownames from geno_sample.fam...
#> Extracting number of variants and colnames from geno_sample.bim...
colnames(genoSample) = paste0('snp', 1:nvar)
rownames(genoSample) = 1:npop

# run genetic association tests
genoStats = getGeneticAssociations(
  scores, genoSample, demoSample, diseaseVariantMap, lmFormula = ~ sex,
  modelType = 'additive')
```
