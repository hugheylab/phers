# Changelog

## phers 1.0.3

- Removed dependency on qs package, which was removed from CRAN.

## phers 1.0.2

CRAN release: 2023-03-26

- Reduced size of example data.

## phers 1.0.1

- Replaced `speedglm()` with [`glm()`](https://rdrr.io/r/stats/glm.html)
  due to compatibility issues with CRAN.

## phers 1.0.0

CRAN release: 2022-12-14

- Added methods to calculate weights based on logistic, log-linear, and
  Cox proportional hazards regression.

## phers 0.0.4

- Updated documentation with the version of `hpoPhecodeMap`.

## phers 0.0.3

CRAN release: 2022-05-31

- Set better key for residual scores table.
- Switched to [`lm()`](https://rdrr.io/r/stats/lm.html) instead of
  [`glm()`](https://rdrr.io/r/stats/glm.html) for speed.
- Simplified genetic association table for genotypic model.
- Switched from `vid` to `variant_id` for clarity and consistency.

## phers 0.0.2

CRAN release: 2022-05-10

- Set keys for data.tables.

## phers 0.0.1

- Updated code and documentation.
