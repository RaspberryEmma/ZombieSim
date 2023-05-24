# ****************************************
# Intractable Models Simulation Study
# 
# Data Generation for Input to SIR Model
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


# ----- Synthetic Data Generation -----

# example for structure
generate.start.cond <- function(size = NULL) {
  cond <- c(1.0, 2.0, 3.0)
  return (cond)
}

ex.1 <- generate.start.cond()

# save data
save( ex.1 , file = "data/example.rda" )





