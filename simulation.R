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

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(knitr)
  library(stringr)
  library(tidyverse)
})

source("generate_data.R")
Rcpp::sourceCpp("abc.cpp")
Rcpp::sourceCpp("sl.cpp")


# ----- Simulation Helper Functions -----

