# ****************************************
# Intractable Models Simulation Study
# 
# Data Generation for Input to SIR Model
# 
# Henry Bourne, Rachel Wood, Emma Tarmey
#
# Started:          23/05/2023
# Most Recent Edit: 31/05/2023
# ****************************************


#' Gives a list of starting conditions of the model
#'
#' @param N Size of total population to model
#' @param initial.inf Number of infected people in the first time point
#' @param beta The bite rate 
#' @param kappa The kill rate
#' @param rho The resurrection rate
#'
#' @return a list with elements
#' - `populations` A vector with elements showing the counts of the initial susceptible, zombie and removed populations
#' - `N` The total population size
#' - `beta` The beta supplied as input, , otherwise defaults to 2/N
#' - `kappa` The kappa supplied as input, otherwise defaults to 3/N
#' - `rho` The rho supplied as input, otherwise defaults to 1/N
#' @export
generateStartCond <- function(N = NULL, initial.inf = NULL, beta =  2/(N), kappa = 3/(N), rho = 1/N){
  # central model parameters
  #beta <- rnorm(n = 1, mean = 2/(N), sd = 1/(10 * N^2)) # contact rate, the "R statistic" (R_0)
  #kappa <- rnorm(n = 1, mean = 3/(N), sd = 1/(10 * N^2)) # kill rate
  #rho <- rnorm(n=1, mean =1/(N), sd =1/(10 * N^2))   # resurrection rate
  
  # initial population values
  Z.0 <- as.integer( initial.inf )      # initial infected population total (small)
  S.0 <- as.integer( N - Z.0)     # initial susceptible population total (large)
  R.0 <- as.integer( 0 )                # initial recovered population total (zero)
  
  return.val <- list(
    populations = c(S.0, Z.0, R.0),
    N = N,
    beta = beta,
    kappa = kappa,
    rho = rho
  )
  
  return (return.val)
}



#' Gives simulated data for a deterministic SZR model
#'
#' @param initial_cond The initial conditions of the simulation, as produced by the `generateStartCond()` functionS
#' @param total.T The number of time points to simulate
#' @param stochastic Should simulation be stochastic or deterministic
#'
#' @return A list with elements
#' - `results` A data frame with `total.T` rows and 4 columns showing the population counts for each time point
#' - `N` The total population, as input
#' - `beta` The true bite rate
#' - `kappa` The true kill rate
#' - `rho` The true resurrection rate
#' @export
generateSZRdata <- function(initial_cond, total.T = NULL, stochastic = TRUE) {
  
  # initial model conditions
  cond <- initial_cond
  
  # constant model parameters
  N       <- cond$N
  beta    <- cond$beta
  kappa   <- cond$kappa
  rho     <- cond$rho
  
  
  # results array to hold values of each function at times 1 to total.T
  results <- array(data = NA, dim = c(total.T, length(cond$populations)+3) )
  results[1, ] <- c(1, cond$populations, 0, 0)
  
  # vector to hold current sim values
  values.t <- c(1, cond$populations, 0, 0)
  S <- values.t[2]
  Z <- values.t[3]
  R <- values.t[4]
  
  # Deterministic update functions
  infected.det <- function(beta, s, z) {
    return(as.integer(s*(min(c(beta *z, 1)))))
  }
  
  killed.det <- function(kappa,s, z) {
    return(as.integer(z*(min(kappa *s, 1))))
  }
  
  resurrected.det <-function(rho, r) {
    return ((as.integer(rho*r)))
  }
  
  # Stochastic Update Functions
  infected.stoch <- function(beta,s, z){
    prob <- min(c(beta *z, 1))
    return(sum(rbinom(s, 1, prob)))
  }
  
  killed.stoch <- function(kappa,s, z) {
    prob <- min(c(kappa *s, 1))
    return(sum(rbinom(z, 1, prob)))
  }
  
  resurrected.stoch <-function(rho, r) {
    return(rbinom(1, r, rho))
  }
  # run through SIR simulation for times 2 to total.T
  # results vector indices 1=S, 2=Z, 3=R
  for (t in 2:total.T) {
    
    if (stochastic == TRUE){
      infected.t    <- infected.stoch(   beta,  S, Z)
      killed.t      <- killed.stoch(     kappa, S, Z)
      resurrected.t <- resurrected.stoch(rho,   values.t[4])
    } else {
      infected.t    <- infected.det(   beta,  S, Z)
      killed.t      <- killed.det(     kappa, S, Z)
      resurrected.t <- resurrected.det(rho,   values.t[4])
    }
    
    S <- values.t[2] - infected.t
    Z <- values.t[3] + infected.t - killed.t 
    R <- values.t[4] + killed.t 
    
    values.t     <- c(t, S, Z, R, infected.t, killed.t)
    results[t, ] <- values.t
  }
  
  # convert to data.frame to explicitly record column meaning
  results           <- as.data.frame(results)
  colnames(results) <- c("t", "S.t", "Z.t", "R.t", "infected", "killed")
  
  return.val <- list(
    results = results,
    N = N,
    beta = beta,
    kappa = kappa,
    rho = rho
  )
  return (return.val)
}


#' @importFrom Rcpp evalCpp
#' @importFrom stats rbinom
#' @useDynLib ZombieSim
NULL

