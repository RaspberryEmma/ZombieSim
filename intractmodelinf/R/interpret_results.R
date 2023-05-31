# ****************************************
# Intractable Models Simulation Study
# 
# Interpret results from simulation
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
  library(gridExtra)
  library(tidyverse)
})



# ----- Read Results Data from CSV -----

load("data/bristol_uni_example.rda")
load("data/bristol_example.rda")
load("data/UK_example.rda")



# ----- Plot Results -----

legend.colors <- c("Susceptible" = "red", "Infected" = "blue", "Recovered" = "green")

bristol.uni.plot <- ggplot(bristol.uni.example, aes(x = t)) +
  geom_line(aes(y  = S.t, color = "Susceptible")) +
  geom_line(aes(y  = I.t, color = "Infected")) +
  geom_line(aes(y  = R.t, color = "Recovered")) +
  labs(color = "Legend") +
  scale_color_manual(values = legend.colors) +
  ggtitle("SIR Model - Bristol Uni Example") +
  xlab("Time") +
  ylab("SIR Populations")

bristol.plot <- ggplot(bristol.example, aes(x = t)) +
  geom_line(aes(y  = S.t, color = "Susceptible")) +
  geom_line(aes(y  = I.t, color = "Infected")) +
  geom_line(aes(y  = R.t, color = "Recovered")) +
  labs(color = "Legend") +
  scale_color_manual(values = legend.colors) +
  ggtitle("SIR Model - Bristol City Example") +
  xlab("Time") +
  ylab("SIR Populations")

UK.plot <- ggplot(UK.example, aes(x = t)) +
  geom_line(aes(y  = S.t, color = "Susceptible")) +
  geom_line(aes(y  = I.t, color = "Infected")) +
  geom_line(aes(y  = R.t, color = "Recovered")) +
  labs(color = "Legend") +
  scale_color_manual(values = legend.colors) +
  ggtitle("SIR Model - UK Example") +
  xlab("Time") +
  ylab("SIR Populations")



# ----- Save Plots -----

png("plots/bristol_uni_plot.png")
bristol.uni.plot
dev.off()

png("plots/bristol_plot.png")
bristol.plot
dev.off()

png("plots/UK_plot.png")
UK.plot
dev.off()




