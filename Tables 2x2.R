# /********************************************************************************************
#
# Simulation of 2x2 tables
# Statistical tests used: Chi-square test, Fisher's exact
# R Version         :  4.0.2
#
# *********************************************************************************************/


### 2x2 Table
# n11  n12
# n21  n22

### Tests used
# TEST 1: Chi-square test without Yates' continuity correction
# TEST 2: Chi-square test with Yates' continuity correction
# TEST 3: Fisher exact test

### Path to R libraries
.libPaths(c("C:/Users/YVanden2/R402", .libPaths())) # directory where libraries are located


###**********************************************************************************************


### Type 1 Error = P(H1|H0) ----
###*****************************

## Set Parameters
set.seed(2021)
p1 = p2 = 0.25    # probability of succes in group 1 and 2. Should be equal since H0 is true
N1 = N2 = 50      # set sample size in both groups; this can be different if you want this
B = 10000         # number of simulations

## Simulation
pvalues.test1 = pvalues.test2 = pvalues.test3 = NULL
for(b in 1:B){
  # simulate group 1 data
  n11 = rbinom(n = 1, size = N1, prob = p1) 
  n12 = N1 - n11
  
  # simulate group 1 data
  n21 = rbinom(n = 1, size = N2, prob = p2) 
  n22 = N2 - n21
  
  # simulated 2x2 table
  sim.table = t(matrix(c(n11, n12, n21, n22), nrow=2))
  
  # tests
  result.test1 = chisq.test(sim.table, correct = FALSE)
  result.test2 = chisq.test(sim.table, correct = TRUE)
  result.test3 = fisher.test(sim.table)
  
  # save p-values
  pvalues.test1[b] = result.test1$p.value
  pvalues.test2[b] = result.test2$p.value
  pvalues.test3[b] = result.test3$p.value
}

mean(pvalues.test1 < 0.05)  # should be around 0.05
mean(pvalues.test2 < 0.05)  # should be around 0.05
mean(pvalues.test3 < 0.05)  # should be around 0.05


###**********************************************************************************************


### Power = P(H1|H1) ----
###*****************************

## Set Parameters
set.seed(2021)
p1 = 0.73
p2 = 0.46       # probability of succes in group 1 and 2. Should be equal since H0 is true
N1 = 15            # set sample size in both groups; this can be different if you want this
N2 = 59    
B = 10000       # number of simulations

## Power calculations 
library(Exact)
power.exact.test(p1=p1, p2=p2, n1=N1, n2=N2, method="chisq")
power.exact.test(p1=p1, p2=p2, n1=N1, n2=N2, method="yates chisq")
power.exact.test(p1=p1, p2=p2, n1=N1, n2=N2, method="Fisher")

## Simulation
pvalues.test1 = pvalues.test2 = pvalues.test3 = NULL
for(b in 1:B){
  # simulate group 1 data
  n11 = rbinom(n = 1, size = N1, prob = p1) 
  n12 = N1 - n11
  
  # simulate group 1 data
  n21 = rbinom(n = 1, size = N2, prob = p2) 
  n22 = N2 - n21
  
  # simulated 2x2 table
  sim.table = t(matrix(c(n11, n12, n21, n22), nrow=2))
  
  # tests
  result.test1 = chisq.test(sim.table, correct = FALSE)
  result.test2 = chisq.test(sim.table, correct = TRUE)
  result.test3 = fisher.test(sim.table)
  
  # save p-values
  pvalues.test1[b] = result.test1$p.value
  pvalues.test2[b] = result.test2$p.value
  pvalues.test3[b] = result.test3$p.value
}

100*mean(pvalues.test1 < 0.05)  # power of test 1
100*mean(pvalues.test2 < 0.05)  # power of test 2
100*mean(pvalues.test3 < 0.05)  # power of test 3










