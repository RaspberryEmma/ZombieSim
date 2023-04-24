# ****************************************
# Intractable Models Simulation Study
# 
# Idea Board / Pomp Library Example
# 
# Rachel Wood, Emma Tarmey
#
# Started:          24/04/2023
# Most Recent Edit: 24/04/2023
# ****************************************

# documentation example
library(pomp)

simulate(t0     = 0,
         times  = 1:20,
         params = c(r=1.2, K=200, sigma=0.1, N_0=50),
         rinit  = function (N_0, ...) {
           c(N=N_0)
         },
         
         rprocess = discrete_time(
           function (N, r, K, sigma, ...) {
             eps <- rnorm(n=1,mean=0,sd=sigma)
             c(N = r * N * exp(1 - (N/K) + eps))
           },
           delta.t = 1
         )
) -> sim1

sim1
spy(sim1)
plot(sim1)

# https://kingaa.github.io/sbied/ feels relevant


