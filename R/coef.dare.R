#' Extract model coefficients
#'
#' coef method for class "dare"
#'
#' @param object
#'
#' @export coef.dare
#' @exportS3Method dare::coef

coef.dare = function(object){
  ret = object$summary$`Posterior Median`
  names(ret) = object$summary$Variable

  return(ret)
}
