# Calculate phecode-specific weights for phenotype risk scores

This is typically the second step of an analysis using phenotype risk
scores, the next is
[`getScores()`](https://phers.hugheylab.org/reference/getScores.md).

## Usage

``` r
getWeights(
  demos,
  phecodeOccurrences,
  method = c("prevalence", "logistic", "cox", "loglinear", "prevalence_precalc"),
  methodFormula = NULL,
  negativeWeights = FALSE,
  dopar = FALSE
)
```

## Arguments

- demos:

  A data.table having one row per person in the cohort. Must have a
  column `person_id`. When the `cox` method is used, `demos` must have
  columns `first_age` and `last_age` corresponding to first and last age
  of visit (in years).

- phecodeOccurrences:

  A data.table of phecode occurrences for each person in the cohort.
  Must have columns `person_id` and `phecode` under the "prevalence" or
  "logistic" methods, columns `person_id`, `phecode`, and
  `num_occurrences` under the "loglinear" method, and columns
  `person_id`, `phecode`, and `occurrence_age` under the "cox" method.
  `num_occurrences` refers to the number of unique dates a phecode was
  recorded for a person. `occurrence_age` refers to the first age (in
  years) a person acquired a phecode.

- method:

  A string indicating the statistical model for calculating weights.

- methodFormula:

  A formula representing the right-hand side of the model corresponding
  to `method`. All terms in the formula must correspond to columns in
  `demos`. A method formula is not required for the "prevalence" and
  "prevalence_precalc" methods. Do not use age-related covariates with
  the "cox" method.

- negativeWeights:

  Logical indicating whether to allow negative weights for individuals
  with no occurrences of a phecode. This option is not required for the
  "loglinear" method since under this method, individuals with a nonzero
  phecode occurrence can also have negative weights.

- dopar:

  Logical indicating whether to run calculations in parallel if a
  parallel backend is already set up, e.g., using
  [`doParallel::registerDoParallel()`](https://rdrr.io/pkg/doParallel/man/registerDoParallel.html).
  Recommended to minimize runtime.

## Value

A data.table with columns `person_id`, `phecode`, `pred`, and `w`. The
column `pred` represents a different quantity depending on `method`.
Under the "prevalence" `method`, it is fraction of the cohort that has
at least one occurrence of the given phecode. The "prevalence_precalc"
`method` is similar to the "prevalence" `method` but `pred` is
calculated based on EHR data from the Vanderbilt University Medical
Center. Under "logistic" or "cox" `method`, it is the predicted
probability of given individual having a given phecode based on
`methodFormula`. Under the "loglinear" `method`, it is the predicted
`log2(num_occurrences + 1)` of a given phecode for a given individual
based on `methodFormula`. For the "prevalence", "prevalence_precalc",
"cox", and "logistic" `method`s, weight is calculated as `-log10(pred)`
when an individual has non-zero phecode occurrence and `log10(1 - pred)`
when an individual has zero phecode occurrence. For the "loglinear"
`method` weight is calculated as the difference between the observed
`log2(num_occurrences + 1)` and `pred`.

## See also

[`getPhecodeOccurrences()`](https://phers.hugheylab.org/reference/getPhecodeOccurrences.md),
[`getScores()`](https://phers.hugheylab.org/reference/getScores.md)

## Examples

``` r
library('data.table')
library('survival')

# map ICD codes to phecodes
phecodeOccurrences = getPhecodeOccurrences(icdSample)

# calculate weights using the prevalence method
weightsPrev = getWeights(demoSample, phecodeOccurrences)

# calculate weights using the prevalence method
# (assign negative weights to those with zero phecode occurrence)
weightsPrevNeg = getWeights(
  demoSample, phecodeOccurrences, negativeWeights = TRUE)

# calculate weights using the logistic method
weightsLogistic = getWeights(
  demoSample, phecodeOccurrences, method = 'logistic', methodFormula = ~ sex)

# calculate weights using the loglinear method
phecodeOccurrences2 = phecodeOccurrences[, .(
  num_occurrences = uniqueN(entry_date)), by = .(person_id, phecode)]
weightsLoglinear = getWeights(
  demoSample, phecodeOccurrences2, method = 'loglinear', methodFormula = ~ sex)

# calculate weights using the cox method
phecodeOccurrences3 = phecodeOccurrences[, .(
  first_occurrence_date = min(entry_date)) , by = .(person_id, phecode)]
phecodeOccurrences3 = merge(
  phecodeOccurrences3, demoSample[, .(person_id, dob)], by = 'person_id')
phecodeOccurrences3[,
  occurrence_age := as.numeric((first_occurrence_date - dob)/365.25)]
#> Key: <person_id>
#>      person_id phecode first_occurrence_date        dob occurrence_age
#>          <int>  <char>                <Date>     <Date>          <num>
#>   1:         1     365            2014-03-10 1925-05-24       88.79670
#>   2:         1     366            2014-11-07 1925-05-24       89.45926
#>   3:         1     735            2014-05-28 1925-05-24       89.01299
#>   4:         1   735.1            2014-05-28 1925-05-24       89.01299
#>   5:         2     442            2011-10-10 1976-06-05       35.34601
#>  ---                                                                  
#> 100:        20     531            2017-02-15 2006-09-25       10.39358
#> 101:        20   531.1            2017-02-15 2006-09-25       10.39358
#> 102:        20     661            2019-07-01 2006-09-25       12.76456
#> 103:        20     755            2017-05-10 2006-09-25       10.62356
#> 104:        20   755.3            2017-05-10 2006-09-25       10.62356
phecodeOccurrences3[, `:=`(first_occurrence_date = NULL, dob = NULL)]
#> Key: <person_id>
#>      person_id phecode occurrence_age
#>          <int>  <char>          <num>
#>   1:         1     365       88.79670
#>   2:         1     366       89.45926
#>   3:         1     735       89.01299
#>   4:         1   735.1       89.01299
#>   5:         2     442       35.34601
#>  ---                                 
#> 100:        20     531       10.39358
#> 101:        20   531.1       10.39358
#> 102:        20     661       12.76456
#> 103:        20     755       10.62356
#> 104:        20   755.3       10.62356
demoSample3 = demoSample[, .(
  person_id, sex,
  first_age = as.numeric((first_visit_date - dob)/365.25),
  last_age = as.numeric((last_visit_date - dob)/365.25))]
weightsCox = getWeights(
  demoSample3, phecodeOccurrences3, method = 'cox', methodFormula = ~ sex)
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Loglik converged before variable  1 ; beta may be infinite. 
#> Warning: Loglik converged before variable  1 ; beta may be infinite. 
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Loglik converged before variable  1 ; beta may be infinite. 
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Loglik converged before variable  1 ; beta may be infinite. 
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge
#> Warning: Loglik converged before variable  1 ; beta may be infinite. 
#> Warning: Loglik converged before variable  1 ; beta may be infinite. 
#> Warning: Loglik converged before variable  1 ; beta may be infinite. 
#> Warning: Ran out of iterations and did not converge
#> Warning: Ran out of iterations and did not converge

# calculate weights using pre-calculated weights based on data from
# Vanderbilt University Medical Center
weightsPreCalc = getWeights(
  demoSample, phecodeOccurrences, method = 'prevalence_precalc')
```
