# /********************************************************************************************
#
# Power calculation of continuous data. Difference in power between (a) using a t-test, and (b) dichotomization of your data.
# Statistical tests used: Student t-test, Fisher's exact
# R Version         :  4.0.2
#
# *********************************************************************************************/

### Tests used
# TEST 1: Student t-test
# TEST 2: Fisher exact test

### 2x2 Table
# n11  n12 (group 1)
# n21  n22 (group 2)

### Path to R libraries
.libPaths(c("C:/Users/YVanden2/R402", .libPaths())) # directory where libraries are located


###**********************************************************************************************


### Type 1 Error = P(H1|H0) ----
###*****************************

## Set Parameters
set.seed(2021)
B = 10000         # number of simulations
mu1 = mu2 = 0.0   # means in group 1 and 2. Should be equal since H0 is true
sd1 = sd2 = 1.0   # sd in group 1 and 2. Equal is an assumption of Student t-test
N1 = N2 = 50      # set sample size in both groups; this can be different if you want this

## Simulation
pvalues.test1 = pvalues.test2 = NULL
for(b in 1:B){
  # simulate group 1 data
  y1 = rnorm(n = N1, mean = mu1, sd = sd1)
  y1.dich = 1*(y1 < qnorm(p = 0.25))
  n11 = sum(y1.dich==0)
  n12 = sum(y1.dich==1)
  
  # simulate group 1 data
  y2 = rnorm(n = N2, mean = mu2, sd = sd2)
  y2.dich = 1*(y2 < qnorm(p = 0.25))
  n21 = sum(y2.dich==0)
  n22 = sum(y2.dich==1)
  
  # Create 2x2 table
  sim.table = t(matrix(c(n11, n12, n21, n22), nrow=2))
  
  # tests
  result.test1 = t.test(y1, y2, var.equal = TRUE)
  result.test2 = fisher.test(sim.table)
  
  # save p-values
  pvalues.test1[b] = result.test1$p.value
  pvalues.test2[b] = result.test2$p.value
}

mean(pvalues.test1 < 0.05)  # should be around 0.05
mean(pvalues.test2 < 0.05)  # should be around 0.05


###**********************************************************************************************


### Power = P(H1|H1) ----
###*****************************

## Set Parameters
set.seed(2021)
B = 10000         # number of simulations
mu1 = 0.0
delta = 0.5      # effect size
mu2 = mu1 + delta
sd1 = sd2 = 1.0   # sd in group 1 and 2. Equal is an assumption of Student t-test
N1 = N2 = 100      # set sample size in both groups; this can be different if you want this


## Power of t-test
power.t.test(n = 50, delta = delta, sd = sd1)


## Simulation
pvalues.test1 = pvalues.test2 = NULL
for(b in 1:B){
  # simulate group 1 data
  y1 = rnorm(n = N1, mean = mu1, sd = sd1)
  y1.dich = 1*(y1 < qnorm(p = 0.25))
  n11 = sum(y1.dich==0)
  n12 = sum(y1.dich==1)
  
  # simulate group 1 data
  y2 = rnorm(n = N2, mean = mu2, sd = sd2)
  y2.dich = 1*(y2 < qnorm(p = 0.25))
  n21 = sum(y2.dich==0)
  n22 = sum(y2.dich==1)
  
  # Create 2x2 table
  sim.table = t(matrix(c(n11, n12, n21, n22), nrow=2))
  
  # tests
  result.test1 = t.test(y1, y2, var.equal = TRUE)
  result.test2 = fisher.test(sim.table)
  
  # save p-values
  pvalues.test1[b] = result.test1$p.value
  pvalues.test2[b] = result.test2$p.value
}

100*mean(pvalues.test1 < 0.05)  # power of test 1
100*mean(pvalues.test2 < 0.05)  # power of test 2


