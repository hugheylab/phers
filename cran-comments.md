## R CMD check results

### Local

  0 errors ✓ | 0 warnings ✓ | 0 notes ✓

### R-hub

  * checking examples ... [20s] NOTE
  Examples with CPU (user + system) or elapsed time > 5s
                      user system elapsed
  mapDiseaseToPhecode 7.62   0.08    7.70
  getWeights          7.49   0.18    7.68
  
  * checking for detritus in the temp directory ... NOTE
    'lastMiKTeXException'

See results for [Windows](https://builder.r-hub.io/status/phers_1.0.1.tar.gz-ff03952f81034c6b85960c9b5c51af9c), [Ubuntu](https://builder.r-hub.io/status/phers_1.0.1.tar.gz-57df0038a62b49c5bb0ab659e8d8fab1), and [Fedora](https://builder.r-hub.io/status/phers_1.0.1.tar.gz-f64edfa659864173aae1eeb04a5cc8fd).

### GitHub Actions

  0 errors ✓ | 0 warnings ✓ | 0 notes ✓

See results for Mac, Windows, and Ubuntu [here]().

## Changes from current CRAN release

* Replaced `speedglm()` with `glm()` due to compatibility issues with CRAN.
