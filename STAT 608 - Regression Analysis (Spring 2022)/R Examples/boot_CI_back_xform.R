####
#### Bootstrap to back-transform CIs to original scale.
####
#### We observe Y ~ LogNormal(mu, sg), which means X = log(Y) ~ Normal(mu, sg). The mean 
#### and sd of Y are:
####
####   mu_Y = exp(mu + (sg ^ 2) / 2)
####   sg_Y = sqrt((exp(sg ^ 2) - 1) * exp(2 * mu + sg ^ 2))
####
#### We log transform y to get x and compute a 95% CI for mu_X in the usual way. The 
#### question is: how do we back-transform the endpoints of the CI for mu_X to be on the 
#### original scale (to be a CI for mu_Y)? The naive solution would be to exponentiate 
#### the endpoints of the CI on mu_X.
####

library(boot)

## Y is log normal.
mu <- 0
sg <- 1
n <- 100
y <- exp(rnorm(n, mu, sg))
x <- log(y)

par(mfrow = c(1, 2))
hist(y)
hist(x)

## Sample means and sds of log-Normal Y and transformed X.
y_bar <- mean(y) 
s_y <- sd(y)
x_bar <- mean(x)
s_x <- sd(x)

## Population mean and sd of Y.
mu_Y <- exp(mu + (sg ^ 2) / 2)
sg_Y <- sqrt((exp(sg ^ 2) - 1) * exp(2 * mu + sg ^ 2))

## 95% CI on mu_X.
ci_X <- x_bar + c(-1, 1) * qnorm(0.975) * s_x / sqrt(n)
ci_Y_naive <- exp(ci_X)
ci_Y_corr <- exp(ci_X + (s_x ^ 2) / 2)

## Bootstrap-based CI on original scale.
B <- 10000
mean_f <- function(y, ii) mean(y[ii])
y_bar_boot <- boot(y, mean_f, R = B)$t

par(mfrow = c(1, 1))
hist(y_bar_boot)
ci_Y_boot <- quantile(y_bar_boot, c(0.025, 0.975))
ci_Y_boot
ci_Y_naive
ci_Y_corr

## Check coverage probabilities of different CI types via simulation.
set.seed(40194)

S <- 10000
CI_Y_boot_sim <- CI_Y_naive_sim <- CI_Y_corr_sim <- matrix(NA, nrow = S, ncol = 2)
for(s in 1:S) {
  if(s %% 100 == 0)
    print(s)

  y_s <- exp(rnorm(n, mu, sg))
  x_s <- log(y_s)
  
  x_bar_s <- mean(x_s)
  s_x_s <- sd(x_s)
  
  ## CIs by back-transformation.
  CI_X_s <- x_bar_s + c(-1, 1) * qnorm(0.975) * s_x_s / sqrt(n)
  CI_Y_naive_sim[s, ] <- exp(CI_X_s)
  CI_Y_corr_sim[s, ] <- exp(CI_X_s + (s_x_s ^ 2) / 2)
  
  ## CI by bootstrap.
  y_bar_boot_s <- boot(y_s, mean_f, R = B)$t
  CI_Y_boot_sim[s, ] <- quantile(y_bar_boot_s, c(0.025, 0.975))
}

save(list = c("CI_Y_naive_sim", "CI_Y_corr_sim", "CI_Y_boot_sim"), 
  file = "CI_back_xform.Rout")
load("CI_back_xform.Rout")

cvrg_boot <- cvrg_naive <- cvrg_corr <- numeric(S)
for(s in 1:S) {
  cvrg_boot[s] <- as.numeric((CI_Y_boot_sim[s, 1] <= mu_Y) & 
    (CI_Y_boot_sim[s, 2] >= mu_Y))
  cvrg_naive[s] <- as.numeric((CI_Y_naive_sim[s, 1] <= mu) & 
    (CI_Y_naive_sim[s, 2] >= mu))
  cvrg_corr[s] <- as.numeric((CI_Y_corr_sim[s, 1] <= mu_Y) & 
    (CI_Y_corr_sim[s, 2] >= mu_Y))
}
mean(cvrg_boot)
mean(cvrg_naive)
mean(cvrg_corr)


