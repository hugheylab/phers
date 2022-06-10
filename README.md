# phers

[![check-deploy](https://github.com/hugheylab/phers/workflows/check-deploy/badge.svg)](https://github.com/hugheylab/phers/actions)
[![codecov](https://codecov.io/gh/hugheylab/phers/branch/main/graph/badge.svg)](https://codecov.io/gh/hugheylab/phers)
[![Netlify Status](https://api.netlify.com/api/v1/badges/353450f6-2feb-49ac-8aa1-35ebbf40e378/deploy-status)](https://app.netlify.com/sites/strong-centaur-770dd1/deploys)
[![CRAN Status](https://www.r-pkg.org/badges/version/phers)](https://cran.r-project.org/package=phers)
[![drat version](https://raw.githubusercontent.com/hugheylab/drat/gh-pages/badges/phers_drat_badge.svg)](https://github.com/hugheylab/drat/tree/gh-pages/src/contrib)

`phers` is an R package for calculating phenotype risk scores that quantify the extent to which a person's clinical diagnoses resemble a given Mendelian disease. For details on the R package, see [Aref et al. (bioRxiv)](https://doi.org/10.1101/2022.06.07.495133). For details on the original application of phenotype risk scores, see [Bastarache et. al. (2018)](https://pubmed.ncbi.nlm.nih.gov/29590070/).

## Installation

### Option 1: CRAN

```r
install.packages('phers')
```

### Option 2: Hughey Lab Drat Repository

1. Install [`BiocManager`](https://cran.r-project.org/package=BiocManager).

    ```r
    if (!requireNamespace('BiocManager', quietly = TRUE))
      install.packages('BiocManager')
    ```

1. If you use RStudio, go to Tools → Global Options... → Packages → Add... (under Secondary repositories), then enter:

    - Name: hugheylab
    - Url: https://hugheylab.github.io/drat/

    You only have to do this once. Then you can install or update the package by entering:

    ```r
    BiocManager::install('phers')
    ```

    Alternatively, you can install or update the package by entering:

    ```r
    BiocManager::install('phers', site_repository = 'https://hugheylab.github.io/drat/')
    ```

## Usage

Check out the [reference documentation](https://phers.hugheylab.org/reference/index.html).
