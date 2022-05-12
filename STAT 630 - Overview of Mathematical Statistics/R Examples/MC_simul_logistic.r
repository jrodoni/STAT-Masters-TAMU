##### MC_simul_logistic.r
##### evaluate variance and standard deviation of sample mean using simulation


N = 10000  # number of random samples to generate
n = 35     # sample size
mu = 1.5   # population mean
sc = 0.2   # scale parameter

xbar = rep(0,N)  # for storing the sample means
for (i in 1:N) xbar[i] = mean(rlogis(n,mu,sc))  # rlogis --> random logistic values

## Summary for values of xbar
E.xbar.est = mean(xbar)
Var.xbar.est = mean((xbar-E.xbar.est)^2)
cat("The estimated mean of Xbar is",E.xbar.est,"\n")
cat("The estimated variance of Xbar is",Var.xbar.est,"\n")
cat("The estimated standard deviation of Xbar is",sqrt(Var.xbar.est),"\n")

hist(xbar)

## Summary for values of a function of xbar
odds = exp(xbar)/(1+exp(xbar))
E.odds.est = mean(odds)
Var.odds.est = mean((odds-E.odds.est)^2)
cat("The estimated mean of Odds is",E.odds.est,"\n")
cat("The estimated variance of Odds is",Var.odds.est,"\n")
cat("The estimated standard deviation of Odds is",sqrt(Var.odds.est),"\n")

hist(odds)

#####
