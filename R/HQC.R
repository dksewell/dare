#' Generic HQC function
#'
#' Generic function for computing the Hannan-Quinn Information Criterion.
#'
#' @param object An object.
#' @param ... Additional arguments passed to methods.
#' @export
HQC <- function(object, ...) {
  UseMethod("HQC")
}
