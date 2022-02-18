
#' Positive control analysis
#'
#' Perform positive control analysis
#'
#' @param scores A data.table of phenotype risk scores. The columns are
#'   `person_id`, `disease_id`, `phers`.
#' @param cases A data.table with cases of each disease in `diseaseDxPhecode`.
#'   The columns are `person_id`, `disease_id`.
#' @param diseaseID Numeric value of the disease identifier to generate
#'   the plot for.
#'
#' @return A box plot showing the difference in distribution of cases and controls.
#'
#' @export
runPositiveControl = function(scores, cases, diseaseID) {
  disease_id = dx_status = phers = NULL

  cases1 = copy(cases)
  cases1[, dx_status := 'case']

  plotInput = merge(
    scores[disease_id == diseaseID][, -c('disease_id')],
    cases1[disease_id == diseaseID][, -c('disease_id')],
    by = 'person_id', all.x = TRUE)

  plotInput[is.na(dx_status), dx_status := 'control']

  ggplot(plotInput) + geom_boxplot(aes(x = dx_status, y = phers)) +
    labs(x = '', title = 'PheRS distribution for cases vs. controls')
}



#' Identify Mendelian disease cases
#'
#' Identify cases of Mendelian diseases.
#'
#' @param phecodes A data.table containing phenotypes stored as phecodes
#'   for each person. The columns are `person_id` and `phecode`.
#' @param diseaseDxPhecode A data.table containing mapping between diseases and
#'   the phecodes that represent being diagnosed with them. The columns are
#'   `disease_id` and `dx_phecode`.
#'
#' @return A data.table with cases of each disease in `diseaseDxPhecode`.
#'   The columns are `person_id`, `disease_id`.
#'
#' @export
getCases = function(phecodes, diseaseDxPhecode = phers::diseaseDxPhecode) {
  person_id = disease_id = `.` = NULL

  cases = merge(
    phecodes, diseaseDxPhecode, by.x = 'phecode', by.y = 'dx_phecode')
  cases = unique(cases[, .(person_id, disease_id)])
return(cases)}
