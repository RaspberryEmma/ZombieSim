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

# example dummy data for structure
x.1 <- rnorm(100)
x.2 <- runif(100)
y   <- rweibull(100, shape = 1, scale = pi)

data.example <- data.frame(x.1, x.2, y)

# save data
save( data.example, file = "data/example.rda" )

# example dummy plot
png("plots/example.png")
p <- ggplot(data = data.example, mapping = aes(x = x.1, y = y))
p
dev.off()



