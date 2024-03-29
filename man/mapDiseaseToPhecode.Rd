% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map.R
\name{mapDiseaseToPhecode}
\alias{mapDiseaseToPhecode}
\title{Map diseases to phecodes via HPO terms}
\usage{
mapDiseaseToPhecode(
  diseaseHpoMap = phers::diseaseHpoMap,
  hpoPhecodeMap = phers::hpoPhecodeMap
)
}
\arguments{
\item{diseaseHpoMap}{A data.table containing the mapping between diseases and
HPO terms. Must have columns \code{disease_id} and \code{term_id}. Default is the map
included in this package.}

\item{hpoPhecodeMap}{A data.table containing the mapping between HPO terms
and phecodes. Must have columns \code{term_id} and \code{phecode}. Default is the map
included in this package.}
}
\value{
A data.table with columns \code{disease_id} and \code{phecode}.
}
\description{
A mapping of diseases to their clinical features, represented as phecodes,
is required for calculating phenotype risk scores.
}
\examples{
library('data.table')
library('survival')

# map ICD codes to phecodes
phecodeOccurrences = getPhecodeOccurrences(icdSample)

# calculate weights using the prevalence method
weightsPrev = getWeights(demoSample, phecodeOccurrences)

# calculate weights using the prevalence method
# (assign negative weights to those with zero phecode occurrence)
weightsPrevNeg = getWeights(
  demoSample, phecodeOccurrences, negativeWeights = TRUE)

# calculate weights using the logistic method
weightsLogistic = getWeights(
  demoSample, phecodeOccurrences, method = 'logistic', methodFormula = ~ sex)

# calculate weights using the loglinear method
phecodeOccurrences2 = phecodeOccurrences[, .(
  num_occurrences = uniqueN(entry_date)), by = .(person_id, phecode)]
weightsLoglinear = getWeights(
  demoSample, phecodeOccurrences2, method = 'loglinear', methodFormula = ~ sex)

# calculate weights using the cox method
phecodeOccurrences3 = phecodeOccurrences[, .(
  first_occurrence_date = min(entry_date)) , by = .(person_id, phecode)]
phecodeOccurrences3 = merge(
  phecodeOccurrences3, demoSample[, .(person_id, dob)], by = 'person_id')
phecodeOccurrences3[,
  occurrence_age := as.numeric((first_occurrence_date - dob)/365.25)]
phecodeOccurrences3[, `:=`(first_occurrence_date = NULL, dob = NULL)]
demoSample3 = demoSample[, .(
  person_id, sex,
  first_age = as.numeric((first_visit_date - dob)/365.25),
  last_age = as.numeric((last_visit_date - dob)/365.25))]
weightsCox = getWeights(
  demoSample3, phecodeOccurrences3, method = 'cox', methodFormula = ~ sex)

# calculate weights using pre-calculated weights based on data from
# Vanderbilt University Medical Center
weightsPreCalc = getWeights(
  demoSample, phecodeOccurrences, method = 'prevalence_precalc')
}
\seealso{
\code{\link[=getScores]{getScores()}}
}
