#' AIC
#' 
#' Akaike Information criteria for dare objects.  Lower is better.
#' 
#' @param object object of class dare
#' 
#' @export 
#' @method AIC dare

AIC.dare = function(object,k=2){
  -2.0 * object$log_likelihood + 
    k * nrow(object$summary)
}
