#' HQC
#' 
#' Hannan-Quinn Information criteria for dare objects.  Lower is better.
#' 
#' @param object object of class dare
#' 
#' @export 
#' @method HQC dare


HQC.dare = function(object){
  -2.0 * object$log_likelihood + 
    2.0 * log(log(nrow(object$data))) * nrow(object$summary)
}
