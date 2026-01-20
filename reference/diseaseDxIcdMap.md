# Mapping of diseases and diagnostic ICD codes

This table provides a mapping between 27 Mendelian diseases and the
corresponding ICD-9 and ICD-10 codes that indicate a genetic diagnosis.

## Usage

``` r
diseaseDxIcdMap
```

## Format

A data.table with the following columns:

- `disease_id`: Numeric vector of OMIM disease identifiers

- `disease_name`: Character vector of disease names

- `icd`: Character vector of ICD codes indicating a genetic diagnosis

- `flag`: Integer vector of the vocabulary of the ICD code (**9**:
  ICD-9-CM, **10**: ICD-10-CM)

- `icd_name`: Character vector containing the description of each ICD
  code

## See also

[`getPhecodeOccurrences()`](https://phers.hugheylab.org/reference/getPhecodeOccurrences.md),
[`getDxStatus()`](https://phers.hugheylab.org/reference/getDxStatus.md)
