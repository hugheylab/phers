# Pre-calculated weights for calculating phenotype risk scores

The weights are based on EHR data from the Vanderbilt University Medical
Center Synthetic Derivative (SD) and ICD-phecode map version 1.2 and are
calculated using the "prevalence" method.

## Usage

``` r
preCalcWeights
```

## Format

A data.table with the following columns:

- `phecode`: Character vector of phecodes

- `prev`: Numeric vector of prevalences, i.e., fraction of subjects in
  the SD that have at least one occurrence of the given phecode

- `w`: Numeric vector of weights, calculated as `-log10(prev)`

## See also

[`getWeights()`](https://phers.hugheylab.org/reference/getWeights.md)
