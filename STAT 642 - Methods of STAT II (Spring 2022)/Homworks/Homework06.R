################################## STAT 642 Assignment 06: Due 3/15/2022 ##################################

# 1.) The porosity index is a measure used by soil scientists to assist in the prediction of water
#     movement, storage, availability, and aeration conditions of soils. A soil scientist utilized a special sampling
#     design to take soil samples from one of the university experimental farms to measure the porosity index of
#     the farm soil. The farm was partitioned into fields of approximately 4 hectares each and then divided into
#     eight sections. The sampling plan included a random selection of fields from which sections were randomly
#     selected. Locations for soil subsamples were randomly selected within the sections. The porosity index of
#     each soil subsample is displayed in the following table. More than one random location was measured for
#     only 6 of the 30 sections.






# 2.)


# (b)
repfixt <- function(alpha, gamma, t, sigma_a, sigma_e)
{   r     <- 1
power <- 0
nu1   <- t-1
while(power < gamma) {
  r      <- r+1
  nu2    <- t*(r-1)
  tau    <- (sigma_a/sigma_e)^2
  lambda <- sqrt(1+r*tau)
  Fcr    <- qf(1-alpha, nu1, nu2)
  C      <- (1/lambda^2)*Fcr
  power  <- 1-pf(C, nu1, nu2)
}
print(cbind(t, r, nu1, nu2, Fcr, lambda, power)) }

repfixt(.01,.9,8,sqrt(2),1)




# 3.)

repfixr <- function(alpha, gamma, r, tau)
{   
  t     <- 2
  power <- 0
  
  while(power < gamma) {
    t      <- t+1
    nu1   <- t-1
    nu2    <- t*(r-1)
    lambda <- sqrt(1+r*tau)
    Fcr    <- qf(1-alpha, nu1, nu2)
    C      <- (1/lambda^2)*Fcr
    power  <- 1-pf(C, nu1, nu2)
  }
  print(cbind(r, t, nu1, nu2, Fcr, lambda, power)) }



r = 5
alpha = 0.01
gamma = 0.90

sigeps = 2
sig2eps = 4
sigAGreaterThan = 2.1
sig2AGreaterThan = 4.41

tau = sig2AGreaterThan/sig2eps
repfixr(alpha,gamma,r,tau)





