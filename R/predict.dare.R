#' Prediction function for the DARE model
#'
#'
#' @param object object of class 'dare'
#' @param newdata An optional data frame in which to look for variables with which to predict. 
#' If omitted, the original data are used.  NOTE: If not missing, this must include 
#' a variable called 'time_diff'
#' @param n_monte_carlo_draws Number of samples used in Monte Carlo estimation.
#'
#' @returns updated newdata data.frame with y_hat added as a variable.
#'
#' @importFrom mvtnorm rmvnorm
#' @export predict.dare
#' @exportS3Method dare::predict

predict.dare = function(object,
                        newdata,
                        n_monte_carlo_draws = 1e3){
  
  dose_response = 
    ifelse("alpha" %in% object$summary$Variable,
           "beta-poisson",
           "exponential")
  
  if(missing(newdata)) newdata = object$data
  n = nrow(newdata)
  
  # Extract elements from formulas
  {
    varnames = list()
    formula_string = as.character(object$formula)
    ## Get y variables
    varnames$y =
      formula_string[[2]]
    ## Get ID
    varnames$id =
      substr(formula_string[[3]],
             gregexpr("\\|",formula_string[[3]])[[1]] + 1,
             gregexpr("\\)",formula_string[[3]])[[1]] - 1) |>
      trimws()
    ## Get time variable
    varnames$time =
      substr(formula_string[[3]],
             gregexpr("\\(",formula_string[[3]])[[1]] + 1,
             gregexpr("\\|",formula_string[[3]])[[1]] - 1) |>
      trimws()
    ## Get covariates
    plus_locations =
      c(0,gregexpr("\\+",formula_string[[3]])[[1]])
    varnames$covariates =
      sapply(1:(length(plus_locations) - 1),
             function(i){
               substr(formula_string[[3]],
                      plus_locations[i] + 1,
                      plus_locations[i + 1] - 1) |>
                 trimws()
             })
    rm(plus_locations,formula_string)
  }
  
  # Get design matrix
  X =
    model.matrix(as.formula(paste0("~ ",
                                   paste(varnames$covariates,
                                         collapse = "+"))),
                 data = newdata)
  P = ncol(X)
  
  # Get posterior draws of parameters
  mc_draws = 
      mvtnorm::rmvnorm(n_monte_carlo_draws,
                       mean = 
                         c(object$summary$`Posterior Median`[1:P],
                           log(object$summary$`Posterior Median`[P + 1:2])),
                       sigma = object$asymptotic_covariance)
  mc_draws[,P + 1:2] = exp(mc_draws[,P + 1:2])
  
  
  # Get draws from posterior predictive density
  y_new = 
    matrix(0L,n_monte_carlo_draws,n)
  
  for(iter in 1:n_monte_carlo_draws){
    
    if(dose_response == "beta-poisson"){
      
      y_new[iter,] = 
        rbinom(n,1,
               prob =  (1.0 - (1.0 + exp( X %*% mc_draws[iter,1:P] + 
                                           rnorm(n,
                                                 sd = mc_draws[iter,P + 1]) ))^(- mc_draws[iter,P + 2])))
      
    }else{
      
      y_new[iter,] = 
        rbinom(n,1,
               prob =  (1.0 - exp(-exp( X %*% mc_draws[iter,1:P] + rnorm(n,
                                                                         sd = mc_draws[iter,P + 1])))))
      
    }
  }
  
  
  newdata$y_hat = colMeans(y_new)
  
  return(newdata)
}