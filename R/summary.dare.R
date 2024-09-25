#' Summarizing DARE model fits
#'
#' summary method for class "dare"
#'
#' @param object object of class "dare"
#' @param CI_level the level of credible intervals to be provided
#' @param print logical
#'
#' @export summary.dare
#' @exportS3Method dare::summary


summary.dare = function(object,CI_level,print = TRUE){
  summ = object$summary
  if(!missing(CI_level)){

    summ$Lower =
      apply(object$bootstrap_samples,2,quantile,probs = (1 - CI_level) / 2)
    summ$Upper =
      apply(object$bootstrap_samples,2,quantile,probs = 1 - (1 - CI_level) / 2)
  }

  if(print) print(summ)
  invisible(summ)
}
