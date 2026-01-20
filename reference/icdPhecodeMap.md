# Mapping of ICD codes and phecodes

This table provides a mapping between International Classification of
Diseases 9th and 10th revisions (ICD-9-CM and ICD-10-CM) and phecodes
(version 1.2).

## Usage

``` r
icdPhecodeMap
```

## Format

A data.table with the following columns:

- `icd`: Character vector of ICD codes

- `flag`: Integer vector of the vocabulary of the ICD code (**9**:
  ICD-9-CM, **10**: ICD-10-CM)

- `icd_name`: Character vector of ICD code descriptions

- `phecode`: Character vector of phecodes

- `phecode_name`: Character vector of phecode descriptions

## Source

<https://phewascatalog.org/phecodes>

## See also

[`getPhecodeOccurrences()`](https://phers.hugheylab.org/reference/getPhecodeOccurrences.md)
