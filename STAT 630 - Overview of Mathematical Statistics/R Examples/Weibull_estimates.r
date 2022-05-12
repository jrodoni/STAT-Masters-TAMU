##### Weibull_estimates.r
### obtain MLE and MOME for different random samples, with known shape parameter
### mean = b * Gamma(1+1/a)

a.par = 2.5  # alpha and beta for Weibull cdf 1 - exp(-(x/b)^a)
b.par = 10
n = 50       # sample size
ns = 100000    # number of samples

b.mle = b.mom = rep(0,ns)  # for saving results

for (i in 1:ns){
  sample = rweibull(n,a.par,b.par)          # simulate random sample
  b.mle[i] = mean(sample^a.par)^(1/a.par)   # MLE for b when a is known
  b.mom[i] = mean(sample)/gamma(1+1/a.par)
}

cat("MLE standard error is",sd(b.mle),"; MOM standard error is",sd(b.mom),"\n")

range(c(b.mle,b.mom))   # to determine range for histogram plots
hist(b.mle,breaks=seq(7.4,13.2,.2))
hist(b.mom,breaks=seq(7.4,13.2,.2))

### estimate c = b^{-a}; note: cdf = 1 - exp(c*x^a)
c.mle = b.mle^(-a.par)
c.mom = b.mom^(-a.par)
cat("MLE standard error is",sd(c.mle),"; MOM standard error is",sd(c.mom),"\n")
range(c(c.mle,c.mom))   # to determine range for histogram plots
hist(c.mle,breaks=seq(.0015,.0070,.0002))
hist(c.mom,breaks=seq(.0015,.0070,.0002))

#####
