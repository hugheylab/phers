## R CMD check results

### Local check
`devtools::check()` result:

  0 errors ✓ | 0 warnings ✓ | 0 notes ✓

### Online check
`devtools::check_rhub()` Windows result:

  > checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Jake Hughey <jakejhughey@gmail.com>'
  
    New submission
    
    Possibly misspelled words in DESCRIPTION:
      Bastarache (9:61)
      al (9:75)
      et (9:72)
    
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
  
    New submission

    Possibly misspelled words in DESCRIPTION:
      al (9:75)
      Bastarache (9:61)
      et (9:72)
    
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
    phers                  17.268  0.042  11.013
    getPhecodeOccurrences   7.826  0.066   4.391
    getGeneticAssociations  7.686  0.080   5.171
    getScores               7.559  0.011   4.503
    getWeights              7.226  0.008   4.529
    getResidualScores       5.755  0.012   4.355
    mapDiseaseToPhecode     5.386  0.018   3.472
  
  0 errors ✔ | 0 warnings ✔ | 2 notes ✖
  
  
`devtools::check_mac_release()` result:

  Status: OK
  
  0 errors ✔ | 0 warnings ✔ | 0 notes ✔

Notes:
  - This is the first time this package is being submitted to CRAN, the words are names and calculation methods, and the DOI is valid so ignore this.
  - The second windows note only occurs on the Windows Server rhub environment, and from what I have seen about these types of notes they do not occur when building and checking on CRAN.
  - Regarding the Ubuntu notes, the one about the invalid URL is also incorrect. I have visited the URL to confirm. The second note however, only occurs in the 2 default rhub Linux environments (Ubuntu and Fedora). It doesn't happen onn Windows or Mac builders, and additionally I cannot replicate this behavior locally, so I believe it is related to rhub and not the code itself.

You can also see the results of R CMD check on Windows, Linux, and MacOS by going to the GitHub Actions run labeled `check-deploy` [here](https://github.com/hugheylab/phers/actions).

## Downstream dependencies
There are no downstream dependencies for phers.
