
#' Perform genotype association tests
#'
#' Perform association tests between the phenotype risk scores and genotypes
#'
#' @param scores A data.table of phenotype risk scores. The columns are
#'   `person_id`, `disease_id`, `phers`.
#' @param genotypes A data.table of genotypes. The columns are `person_id`, and
#'   variant IDs to be tested.
#' @param covariates A data.table of covariates to be used in the association
#'   analysis.
#'
#' @return A data.table containing to results of the genotype association tests
#'   with the following columns. If the model fails to converge,
#'   NAs will be reported.
#'   \item{\code{disease_id}}{disease identifier}
#'   \item{\code{vid}}{variant identifier}
#'   \item{\code{beta}}{The beta coefficient for the variant}
#'   \item{\code{se}}{The standard error for the beta coefficient}
#'   \item{\code{p}}{The p-value for the variant}
#'   \item{\code{lower}}{The lower bound of the `confint` confidence interval}
#'   \item{\code{upper}}{The upper bound of the `confint` confidence interval}
#'
#' @export
genotypeAssociation = function(scores, genotypes, covariates) {
 diseaseID = disease_id = gene = ..genoCols = snp = se = p = lower = upper = NULL

 lmInput = merge(scores, covariates, by = 'person_id')

 statsAll = foreach(
   diseaseID = unique(scores$disease_id), .combine = rbind) %dopar% {

   geneSub = phers::diseaseInfo[disease_id == diseaseID]$gene
   snpSub = unique(phers::geneVarMap[gene == geneSub]$vid)

   genoCols = c('person_id', snpSub)
   genotypesSub = genotypes[, ..genoCols]

   lmInputSub = merge(lmInput, genotypesSub, by = 'person_id')

   statsDisease = foreach(snp=snpSub, .combine = rbind) %dopar% {

     covNames = names(covariates[,-c('person_id')])
     formStr =  sprintf(
       "phers ~ `%s` + %s", snp, paste(covNames, collapse = ' + '))
     fit = glm(formStr, data = lmInputSub)

     stat = data.table('disease_id' = diseaseID, 'vid' = snp)
     if (is.na(fit$coefficients[sprintf("`%s`", snp)])) {
       stat[, beta := NA]
       stat[, se := NA]
       stat[, p := NA]
       stat[, lower := NA]
       stat[, upper := NA]
     }else{
       fitSnpCoefs = summary(fit)$coefficients[sprintf("`%s`", snp),]
       stat[, beta := fitSnpCoefs['Estimate']]
       stat[, se := fitSnpCoefs['Std. Error']]
       stat[, p := fitSnpCoefs['Pr(>|t|)']]
       ci =  suppressMessages(confint(fit))
       stat[, lower := ci[sprintf("`%s`", snp), '2.5 %']]
       stat[, upper := ci[sprintf("`%s`", snp), '97.5 %']]
     }
  }
 }
}
