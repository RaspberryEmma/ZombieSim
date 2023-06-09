# ZombieSim
The goal of the `ZombieSim` package is to provide tools for inference for models with intractable likelihoods using Approximate Bayesian Computation (ABC).  Specifically, modelling the outbreak of a viral zombie epidemic and using ABC to estimate model parameters from simulated data.  The package uses R with integrated C++ code (using Rcpp and RcppArmadillo) to help with computational speed.

This also contains functions to simulate a zombie epidemic under the model described in ["When zombies attack!: Mathematical modelling
of an outbreak of zombie infection"](https://loe.org/images/content/091023/Zombie%20Publication.pdf). We use the SZR model given in the paper with the natural birth and death rate set to zero. 

## Installation

This package can be downloaded and used with `devtools` as below:
``` r
# install.packages("devtools")
library(devtools)
devtools::install_github("RaspberryEmma/ZombieSim/ZombieSim")
library(ZombieSim)
```

## Example 
Lets now run through an example of how to use various parameters from the package:
``` r
# Let's simulate some data!
N <-5000
initial <- generateStartCond(N, initial.inf = 10)
new.example <- generateSZRdata(initial, total.T = 50)

# Let's plot our data!
plot.SZR(new.example$results)

# Let's get our samples for the posterior of the parameters
simulated.mat <- new.example$results
simulated.mat  <- as.matrix(simulated.mat[,2:4])
head(simulated.mat )
priorMin <- c(0.0        , 0.0001, 0.0        , 0.00009, 0.0049) 
priorMax <- c(0.000000001, 0.00095 , 0.000000001, 0.0001 , 0.005 )
res <- abcRej(simulated.mat , 10000000, 1000, priorMin, priorMax)

# Let's plot our simulated values alongside the true parameters!
library(ggplot2)
library(dplyr)
means <- c(new.example$beta, new.example$kappa, new.example$rho)
samples <- res[,2:4] %>%
  as_tibble()
colnames(samples) <- c("beta", "kappa", "rho")  
plots <- plot.posterior.samples(samples, means)
plots[[1]]
```
Have fun using the package! Any issues with the code, please get in contact!
