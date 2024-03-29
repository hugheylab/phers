% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genetic_diagnosis.R
\name{getDxStatus}
\alias{getDxStatus}
\title{Identify cases and controls for Mendelian diseases}
\usage{
getDxStatus(
  demos,
  icdOccurrences,
  minUniqueAges = 2L,
  diseaseDxIcdMap = phers::diseaseDxIcdMap
)
}
\arguments{
\item{demos}{A data.table having one row per person in the cohort. Must have
a column \code{person_id}.}

\item{icdOccurrences}{A data.table of occurrences of ICD codes for each
person in the cohort. Must have columns \code{person_id}, \code{icd}, \code{flag}, and
\code{occurrence_age}.}

\item{minUniqueAges}{Integer indicating the minimum number of unique
ICD code entry ages required to classify a person as a case. Persons with
at least one, but fewer than \code{minUniqueAges} entry ages, are assigned as
neither cases nor controls.}

\item{diseaseDxIcdMap}{A data.table of the mapping between diseases and
the corresponding ICD codes that indicate a diagnosis. Must have columns
\code{disease_id}, \code{icd}, and \code{flag}. Default is \link{diseaseDxIcdMap}.}
}
\value{
A data.table with columns \code{person_id}, \code{disease_id}, and \code{dx_status}
(1 indicates a case, 0 indicates a control, -1 indicates neither).
}
\description{
This function is useful for verifying that raw or residual phenotype risk
scores of diagnosed individuals (cases) tend to be higher than scores of
undiagnosed individuals (controls).
}
\examples{
library('data.table')


icdSample1 = merge(icdSample, demoSample[, .(person_id, dob)], by = 'person_id')
icdSample1[, occurrence_age := as.numeric((entry_date - dob)/365.25)]
icdSample1[, `:=`(entry_date = NULL, dob = NULL)]

dxStatus = getDxStatus(demoSample, icdSample1)
}
