# Sample table of ICD occurrences

The data are artificial and do not correspond to real patients.

## Usage

``` r
icdSample
```

## Format

A data.table with the following columns:

- `person_id`: Character vector of the identifier for each person

- `icd`: Character vector of the ICD codes recorded for each person

- `flag`: Integer vector of the vocabulary of the ICD code (**9**:
  ICD-9-CM, **10**: ICD-10-CM)

- `entry_date`: Vector of type `Date` indicating the date each ICD code
  was recorded.

## See also

[`getPhecodeOccurrences()`](https://phers.hugheylab.org/reference/getPhecodeOccurrences.md),
[`getWeights()`](https://phers.hugheylab.org/reference/getWeights.md),
[`getScores()`](https://phers.hugheylab.org/reference/getScores.md)
