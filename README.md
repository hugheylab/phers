# phers

[![check-deploy](https://github.com/hugheylab/phers/workflows/check-deploy/badge.svg)](https://github.com/hugheylab/phers/actions)
[![codecov](https://codecov.io/gh/hugheylab/phers/branch/master/graph/badge.svg)](https://codecov.io/gh/hugheylab/phers)

`phers` is an R package for calculating phenotype risk scores that quantify a person's likelihood of having a Mendelian disease given data from electronic health records. For details about the phenotype risk score model see [Bastarache et. al. (2018)](https://pubmed.ncbi.nlm.nih.gov/29590070/)

## Installation

You can install the development version of phers with:

```r
if (!requireNamespace('remotes', quietly = TRUE))
  install.packages('remotes')
remotes::install_github('hugheylab/phers')
```
