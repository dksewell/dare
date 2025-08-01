#' Goodness of fit Bayesian p-value
#' 
#' Use Bayesian p-values to determine if the model fit is appropriate.
#' 
#' Bayesian p-values determine whether data generated from the posterior 
#' predictive distribution (i.e., data generated according to the model, 
#' without having to specify specific parameter values) match the 
#' observed data.  A good fit is indicated by values close to 0.5.  Any 
#' values less than, say, 0.05 or greater than 0.95 indicate a poor fit. 
#' ***NOTE: Current implementation is only for the beta-Poisson DR model.
#' Also note that future.apply package is used here, so to parallelize 
#' (highly recommended), use plan(multicore), or for Windows users use 
#' plan(cluster, workers = <number of cores>).  See the future package 
#' for more details.
#' 
#' @param object dare object
#' @param n_posterior_draws integer.  Number of posterior draws for the 
#' Bayesian p-value
#' 
#' @returns a named list giving the Bayesian p-value and the 
#' test statistics for the observed and simulated data.
#' 
#' @export
#' @import future
#' @import future.apply
#' @import mvtnorm


dare_gof = function(object,
                    n_posterior_draws){
  
  # Extract model matrix
  f = paste0("y ~ ",as.character(object$formula)[3])
  f =
    substr(f,
           1,
           gregexpr("\\(",f)[[1]] - 4) |>
    as.formula()
  mm = 
    model.matrix(f,
                 object$data)
  
  # Get posterior draws.
  parm_draws = 
    rmvnorm(n_posterior_draws,
            mean = 
              c(summary(object,
                        print = FALSE)$`Posterior Median`[1:ncol(mm)],
                log( summary(object,
                             print = FALSE)$`Posterior Median`[ncol(mm) + 1:2]) 
              ),
            sigma = 
              object$asymptotic_covariance)
  parm_draws[,ncol(mm) + 1:2] =
    exp(parm_draws[,ncol(mm) + 1:2])
  
  # Get linear predictor term, including time interval
  xbeta = 
    tcrossprod(parm_draws[,1:ncol(mm)],
               mm) +
    matrix(object$data$log_time_diff,
           nrow(parm_draws),nrow(object$data),
           byrow = TRUE)
  
  # Compute probability matrix (this takes awhile)
  # and simulated data
  prob_matrix = 
    sim_matrix = 
    matrix(0.0,nrow(xbeta),ncol(xbeta))
  
  for(j in 1:ncol(prob_matrix)){
    prob_matrix[,j] = 
      future_sapply(1:nrow(prob_matrix),
                    FUN = 
                      function(i){
                        helper = function(dummy){
                          (1.0 - (1.0 + exp(xbeta[i,j] + 
                                              parm_draws[i,ncol(mm) + 1] * dummy))^(-parm_draws[i,ncol(mm) + 2])) * dnorm(dummy)
                        }
                        integrate(helper,-4,4)$value
                      })
    
    sim_matrix[,j] = 
      rbinom(nrow(prob_matrix),
             size = 1,
             prob = prob_matrix[,j])
  }
  
  # Compute test statistics for observed and simulated data
  T_obs = 
    ( matrix(object$data$y,
             nrow(prob_matrix),nrow(object$data),
             byrow = TRUE) - 
        prob_matrix )^2 /
    prob_matrix / (1.0 - prob_matrix)
  T_obs = rowSums(T_obs)
  
  T_sim = 
    ( sim_matrix - 
        prob_matrix )^2 /
    prob_matrix / (1.0 - prob_matrix)
  T_sim = rowSums(T_sim)
  
  
  # Create object to be returned
  results = 
    list(pvalue = mean(T_obs <= T_sim),
         test_statistic_observed = T_obs,
         test_statistic_simulated = T_sim)
  
  cat(paste0("\nBayesian p-value = ",
             results$pvalue,
             "\n"))
  
  return(results)
}