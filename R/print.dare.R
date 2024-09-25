#' Print DARE model output
#'
#' print method for class "dare"
#'
#' @param object
#'
#' @export print.dare
#' @exportS3Method dare::print

print.dare = function(object){
  cat("DARE: Dose Accrual Risk Estimation\n\n")


  print(object$summary)

  cat("NOTES: 1. Intercept is confounded with dose-response model parameter.\n")
  cat("       2. Exponentiate regression coefficients to get rate of accruing dose due to a unit change in 'x'.\n")
  cat(paste0("       3. CI's are given at ",100 * object$CI_level,"%.  Use summary() to set the CI level differently.\n"))
}
