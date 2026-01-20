# Mapping of Mendelian diseases and their clinical features

This table provides a mapping between Mendelian diseases and their
clinical features, represented as Human Phenotype Ontology (HPO) terms.
The mapping is based on annotations from Online Mendelian Inheritance in
Man (OMIM).

## Usage

``` r
diseaseHpoMap
```

## Format

A data.table with the following columns:

- `disease_id`: Numeric vector of OMIM disease identifiers

- `disease_name`: Character vector of disease names

- `hpo_term_id`: Character vector of HPO identifiers corresponding to
  each disease's clinical features

- `hpo_term_name`: Character vector of HPO term descriptions

## Source

<https://hpo.jax.org/app/download/annotation>

## See also

[`mapDiseaseToPhecode()`](https://phers.hugheylab.org/reference/mapDiseaseToPhecode.md)
