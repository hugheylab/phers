# Mapping of HPO terms and phecodes

This table provides a mapping between Human Phenotype Ontology (HPO)
terms and phecodes, and is useful for using phecodes to represent the
clinical features of Mendelian diseases (version 1.2).

## Usage

``` r
hpoPhecodeMap
```

## Format

A data.table with the following columns:

- `hpo_term_id`: Character vector of HPO term identifiers

- `hpo_term_name`: Character vector of HPO term descriptions

- `phecode`: Character vector of phecodes

- `phecode_name`: Character vector of phecode descriptions

## See also

[`mapDiseaseToPhecode()`](https://phers.hugheylab.org/reference/mapDiseaseToPhecode.md)
