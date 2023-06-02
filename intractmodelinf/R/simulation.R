# ****************************************
# Intractable Models Simulation Study
# 
# Fit the SIR model and determine outcome
# 
# Henry Bourne, Rachel Wood, Emma Tarmey
#
# Started:          23/05/2023
# Most Recent Edit: 23/05/2023
# ****************************************



# ----- Preamble -----

#rm(list = ls())

#suppressPackageStartupMessages({
 # library(dplyr)
  #library(ggplot2)
#  library(knitr)
#  library(stringr)
#  library(tidyverse)
#})

#source("R/generate_data.R")
#abc <- Rcpp::sourceCpp("src/abc.cpp")


# ----- Simulation Functions -----

#SIR.model <- function(start.cond  = NULL,
                      approx.func = NULL) {
  # build SIR model with starting conditions specified
  
  # run model through to end-point
  
  # report outcome
 # return ( c(0.0, 0.0) )
#}


# ----- Simulation Procedure -----

#set.seed(2023)         # reproducibility
#N             <- 1000  # repetitions per scenario
#M             <- 2     # methods being tested
#n             <- 1000  # synthetic data-set size
#current.data  <- NULL

#method.labels <- c("ABC.Approx", "SL.Approx")
#iter.labels   <- paste("i = ", c(1:N))

#results.s1    <- array(data     = NA,
#                       dim      = c(N, M),
#                       dimnames = list(iter.labels, method.labels) )#

#for (i in 1:N) {
  # Generate synthetic data
#  current.data <- generate.start.cond(total.N = n)
  
  # Run SIR model to obtain outcome
#  current.outcomes <- SIR.model(start.cond  = current.data,
#                                approx.func = c(abc, sl))
  
  # Record outcome as result#
#  results.s1[i, ] <- current.outcomes
#}


# check results
#results.s1 %>% head() %>% knitr::kable()

# save data
#save( results.s1, file = "data/results_s1.rda" )


