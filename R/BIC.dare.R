#' BIC
#' 
#' Bayesian Information criteria for dare objects.  Lower is better.
#' 
#' @param object object of class dare
#' 
#' @export 
#' @method BIC dare

BIC.dare = function(object){
  -2.0 * object$log_likelihood + 
    log(nrow(object$data)) * nrow(object$summary)
}

