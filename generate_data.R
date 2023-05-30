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
  b <- rnorm(n = 1, mean = 0.6, sd = 0.05) # contact rate, the "R statistic" (R_0)
  k <- rnorm(n = 1, mean = 0.4, sd = 0.03) # recovery rate
  
  # initial population values
  S.0 <- as.integer( total.N )     # initial susceptible population total (large)
  I.0 <- as.integer( initial.inf ) # initial infected population total (small)
  R.0 <- as.integer( 0 )           # initial recovered population total (zero)
  
  cond <- c(S.0, I.0, R.0, total.N, b, k)
  
  return (cond)
}


change.s <- function(b, s, i) {
  return ((-1 * b * s * i))
}

change.i <- function(b, k, s, i) {
  return ( ( (b * s * i) + (-1 * k * i) ))
}

change.r <-function(k, i) {
  return ((k * i))
}


generate.SIR.data <- function(total.N = NULL, initial.inf = NULL, total.T = NULL) {
  
  # initial model conditions
  cond <- generate.start.cond(total.N = total.N, initial.inf = initial.inf)
  
  # constant model parameters
  N    <- total.N
  b    <- cond[5]
  k    <- cond[6]
  
  # results array to hold values of each function at times 1 to total.T
  results <- array(data = NA, dim = c(total.T, length(cond)+1) )
  results[1, ] <- c(1, cond)
  
  # vector to hold current sim values
  values.t <- NULL
  
  # run through SIR simulation for times 2 to total.T
  # results vector indices 1=S, 2=I, 3=R
  for (t in 2:total.T) {
    S.t <- as.integer( results[t-1, 2] + N*change.s(b,    results[t-1, 2]/N, results[t-1, 3]/N) )
    
    I.t <- as.integer( results[t-1, 3] + N*change.i(b, k, results[t-1, 2]/N, results[t-1, 3]/N) )
    
    R.t <- as.integer( results[t-1, 4] + N*change.r(k,    results[t-1, 3]/N) )
    
    values.t     <- c(t, S.t, I.t, R.t, N, b, k)
    results[t, ] <- values.t
  }
  
  # convert to data.frame to explicitly record column meaning
  results           <- as.data.frame(results)
  colnames(results) <- c("t", "S.t", "I.t", "R.t", "N", "b", "k")
  
  return ( results )
}


# ----- Working Examples -----

bristol.uni.N <- 29434
bristol.N     <- 467099
UK.N          <- 67081234

bristol.uni.example <- generate.SIR.data(total.N     = bristol.uni.N,
                                         initial.inf = 10,
                                         total.T     = 150)

bristol.example <- generate.SIR.data(total.N     = bristol.N,
                                     initial.inf = 10,
                                     total.T     = 150)

UK.example <- generate.SIR.data(total.N     = UK.N,
                                initial.inf = 10,
                                total.T     = 150)

bristol.uni.example



# ----- Save Data -----

save( bristol.uni.example, file = "data/bristol_uni_example.rda" )
save( bristol.example,     file = "data/bristol_example.rda" )
save( UK.example,          file = "data/UK_example.rda" )



