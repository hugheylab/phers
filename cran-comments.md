## R CMD check results

### Local check
`devtools::check()` result:

  0 errors ✓ | 0 warnings ✓ | 0 notes ✓

### Online check
`devtools::check_rhub()` Windows result:

  > checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Jake Hughey <jakejhughey@gmail.com>'
    
    Found the following (possibly) invalid DOIs:
      DOI: 10.1126/science.aal4043
        From: DESCRIPTION
        Status: Service Unavailable
        Message: 503
  
  > checking for detritus in the temp directory ... NOTE
    Found the following files/directories:
      'lastMiKTeXException'
  
  0 errors ✔ | 0 warnings ✔ | 2 notes ✖
  

`devtools::check_rhub()` Ubuntu result:

  > checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Jake Hughey <jakejhughey@gmail.com>'
  
    Found the following (possibly) invalid URLs:
      URL: https://hpo.jax.org/app/download/annotation
        From: man/diseaseHpoMap.Rd
        Status: Error
        Message: libcurl error code 60:
          	SSL certificate problem: unable to get local issuer certificate
          	(Status without verification: OK)
    
    Found the following (possibly) invalid DOIs:
      DOI: 10.1126/science.aal4043
        From: DESCRIPTION
        Status: Service Unavailable
        Message: 503
        
  > checking examples ... NOTE
  
    Examples with CPU (user + system) or elapsed time > 5s
                             user system elapsed
    phers                  16.328  0.009   7.041
    getGeneticAssociations  8.557  0.143   4.785
    getWeights              6.654  0.006   2.932
    getResidualScores       6.522  0.004   4.254
    mapDiseaseToPhecode     6.344  0.008   2.682
    getPhecodeOccurrences   6.313  0.017   3.318
    getScores               6.309  0.004   3.417
  
  0 errors ✔ | 0 warnings ✔ | 2 notes ✖
  
  
`devtools::check_mac_release()` result:

  Status: OK
  
  0 errors ✔ | 0 warnings ✔ | 0 notes ✔

Notes:
  - The DOI and URL listed in the notes for the `check_rhub` Windows and Ubuntu results are valid so ignore this.
  - The second windows note only occurs on the Windows Server rhub environment, and from what I have seen about these types of notes they do not occur when building and checking on CRAN.
  - The second note in rhub Ubuntu only occurs in the 2 default rhub Linux environments (Ubuntu and Fedora). It doesn't happen on Windows or Mac builders, and additionally I cannot replicate this behavior locally, so I believe it is related to rhub and not the code itself.

You can also see the results of R CMD check on Windows, Linux, and MacOS by going to the GitHub Actions run labeled `check-deploy` [here](https://github.com/hugheylab/phers/actions).

## Downstream dependencies
There are no downstream dependencies for phers.
