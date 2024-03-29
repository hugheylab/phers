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

See results for [Windows](https://builder.r-hub.io/status/phers_1.0.1.tar.gz-957a6d1b9cee448c88426a624e4f531b), [Ubuntu](https://builder.r-hub.io/status/phers_1.0.1.tar.gz-4c1e834e4d124903a622824d3cc3f4e8), and [Fedora](https://builder.r-hub.io/status/phers_1.0.1.tar.gz-41bfe8442b14440298c225a213172a33).

### GitHub Actions

  0 errors ✓ | 0 warnings ✓ | 0 notes ✓

See results for Mac, Windows, and Ubuntu [here]().

## Changes from current CRAN release

* Replaced `speedglm()` with `glm()` due to compatibility issues with CRAN.
* Reduced size of example data.
