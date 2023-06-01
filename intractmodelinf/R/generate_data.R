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



# ----- Preamble -----

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
})



generate.start.cond <- function(N = NULL, initial.inf = NULL, beta =  2/(N), kappa = 3/(N), rho = 1/N){
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


infected <- function(beta, s, z) {
  return(as.integer(s*(min(c(beta *z, 1)))))
}

killed <- function(kappa,s, z) {
  return(as.integer(z*(min(kappa *s, 1))))
}

resurrected <-function(rho, r) {
  return ((as.integer(rho*r)))
}


generate.SZR.data <- function(N = NULL, initial.inf = NULL, total.T = NULL) {
  
  # initial model conditions
  cond <- generate.start.cond(N = N, initial.inf = initial.inf)
  
  # constant model parameters
  N       <- cond$N
  beta    <- cond$beta
  kappa   <- cond$kappa
  rho     <- cond$rho
  
  
  # results array to hold values of each function at times 1 to total.T
  results <- array(data = NA, dim = c(total.T, length(cond$populations)+1) )
  results[1, ] <- c(1, cond$populations)
  
  # vector to hold current sim values
  values.t <- c(1, cond$populations)
  
  # run through SIR simulation for times 2 to total.T
  # results vector indices 1=S, 2=Z, 3=R
  for (t in 2:total.T) {
    infected.t    <- infected(   beta,  values.t[2], values.t[3])
    killed.t      <- killed(     kappa, values.t[2], values.t[3])
    resurrected.t <- resurrected(rho,   values.t[4])
    
    S <- values.t[2] - infected.t
    Z <- values.t[3] + infected.t - killed.t + resurrected.t
    R <- values.t[4] + killed.t - resurrected.t
    
    values.t     <- c(t, S, Z, R)
    results[t, ] <- values.t
  }
  
  # convert to data.frame to explicitly record column meaning
  results           <- as.data.frame(results)
  colnames(results) <- c("t", "S.t", "Z.t", "R.t")
  
  return.val <- list(
    results = results,
    N = N,
    beta = beta,
    kappa = kappa,
    rho = rho
  )
  return (return.val)
}






