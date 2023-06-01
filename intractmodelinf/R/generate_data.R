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



# ----- Synthetic Data Generation -----

generate.start.cond <- function(total.N = NULL, initial.inf = NULL) {
  # central model parameters
  beta <- rnorm(n = 1, mean = 0.000005, sd = 0.0000001) # contact rate, the "R statistic" (R_0)
  kappa <- rnorm(n = 1, mean = 0.00001, sd = 0.0000002) # kill rate
  rho <- rnorm(n=1, mean = 0.00002, sd = 0.000002)   # resurrection rate
  
  # initial population values
  Z.0 <- as.integer( initial.inf )      # initial infected population total (small)
  S.0 <- as.integer( total.N - Z.0)     # initial susceptible population total (large)
  R.0 <- as.integer( 0 )                # initial recovered population total (zero)
  
  cond <- c(S.0, Z.0, R.0, total.N, beta, kappa, rho)
  
  return (cond)
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


generate.SZR.data <- function(total.N = NULL, initial.inf = NULL, total.T = NULL) {
  
  # initial model conditions
  cond <- generate.start.cond(total.N = total.N, initial.inf = initial.inf)
  
  # constant model parameters
  N       <- total.N
  beta    <- cond[5]
  kappa   <- cond[6]
  rho     <- cond[7]
  
  
  # results array to hold values of each function at times 1 to total.T
  results <- array(data = NA, dim = c(total.T, length(cond)+1) )
  results[1, ] <- c(1, cond)
  
  # vector to hold current sim values
  values.t <- c(1, cond)
  
  # run through SIR simulation for times 2 to total.T
  # results vector indices 1=S, 2=Z, 3=R
  for (t in 2:total.T) {
    infected.t    <- infected(   beta,  values.t[2], values.t[3])
    killed.t      <- killed(     kappa, values.t[2], values.t[3])
    resurrected.t <- resurrected(rho,   values.t[4])
    S.t <- values.t[2] - infected.t
    Z.t <- values.t[3] + infected.t - killed.t + resurrected.t
    R.t <- values.t[4] + killed.t - resurrected.t
    
    values.t     <- c(t, S.t, Z.t, R.t, total.N, beta, kappa, rho)
    results[t, ] <- values.t
  }
  
  # convert to data.frame to explicitly record column meaning
  results           <- as.data.frame(results)
  colnames(results) <- c("t", "S.t", "Z.t", "R.t", "N", "beta", "kappa", "rho")
  
  return ( results )
}






