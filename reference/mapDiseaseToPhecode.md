# Map diseases to phecodes via HPO terms

A mapping of diseases to their clinical features, represented as
phecodes, is required for calculating phenotype risk scores.

## Usage

``` r
mapDiseaseToPhecode(
  diseaseHpoMap = phers::diseaseHpoMap,
  hpoPhecodeMap = phers::hpoPhecodeMap
)
```

## Arguments

- diseaseHpoMap:

  A data.table containing the mapping between diseases and HPO terms.
  Must have columns `disease_id` and `term_id`. Default is the map
  included in this package.

- hpoPhecodeMap:

  A data.table containing the mapping between HPO terms and phecodes.
  Must have columns `term_id` and `phecode`. Default is the map included
  in this package.

## Value

A data.table with columns `disease_id` and `phecode`.

## See also

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
