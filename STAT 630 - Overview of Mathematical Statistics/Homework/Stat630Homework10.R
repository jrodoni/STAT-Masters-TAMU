
# 2(c)
c(qbeta(.05,30,24),qbeta(.95,30,24))
c(qbeta(.025,30,24),qbeta(.975,30,24))

# 2d

x = seq(0,1,.001)


fx = dbeta(x, 2,2)
fx_post = dbeta(x, 30,24)

plot(x, fx, type = "l", col = "skyblue", xlab = "Theta", ylab='Density', lwd = 3.5, main = "Prior and Posterior from 2(b)")
par(new = T)
plot(x, fx_post, type = "l", col = "seagreen",xlab = "",ylab = "", yaxt='n', lwd = 3.5)
legend("topright", c("prior", "posterior"), lty=1, col=c("skyblue", "seagreen"))

# 3,)

#(a)
n = 10
x_bar = 1
sigma_squared_ot = 1
mu_ot = 0
tau_squared_ot = 10

posterior_mean = (((1/tau_squared_ot)+(n/sigma_squared_ot))^-1)*((mu_ot/tau_squared_ot)+(n/sigma_squared_ot))*x_bar
posterior_variance = (((1/tau_squared_ot)+(n/sigma_squared_ot))^-1)

p_mu_post_greater_zero = 1-pnorm(0, posterior_mean, sqrt(posterior_variance))


p_mu_prior_greater_zero = 1-pnorm(0,mu_ot,tau_squared_ot)
p_mu_prior_greater_zero


#(b)
n = 10
x_bar = 1
sigma_squared_ot = 4
mu_ot = 0
tau_squared_ot = 10

posterior_mean = (((1/tau_squared_ot)+(n/sigma_squared_ot))^-1)*((mu_ot/tau_squared_ot)+(n/sigma_squared_ot))*x_bar
posterior_variance = (((1/tau_squared_ot)+(n/sigma_squared_ot))^-1)

p_mu_post_greater_zero = 1-pnorm(0, posterior_mean, sqrt(posterior_variance))
p_mu_post_greater_zero

p_mu_prior_greater_zero = 1-pnorm(0,mu_ot,tau_squared_ot)
p_mu_prior_greater_zero

#(c)
n = 10
x_bar = 1
sigma_squared_ot = 4
mu_ot = 0
tau_squared_ot = 2

posterior_mean = (((1/tau_squared_ot)+(n/sigma_squared_ot))^-1)*((mu_ot/tau_squared_ot)+(n/sigma_squared_ot))*x_bar
posterior_variance = (((1/tau_squared_ot)+(n/sigma_squared_ot))^-1)

p_mu_post_greater_zero = 1-pnorm(0, posterior_mean, sqrt(posterior_variance))
p_mu_post_greater_zero

p_mu_prior_greater_zero = 1-pnorm(0,mu_ot,tau_squared_ot)
p_mu_prior_greater_zero



#(d)
posterior_mean
posterior_variance

c((10/12)-qnorm(.995)*sqrt(1/3),(10/12)+qnorm(.995)*sqrt(1/3))


#4.)
# (d)
c(qgamma(.025,shape = 29,rate = 23),qgamma(.975,shape = 29,rate = 23))





# 6.)
#  (b)
x = rgamma(10000, shape = 4, rate = 1)
range(x)

x = seq(0,16,.01)
fx_16_4 = dgamma(x, shape = 16, rate = 4)
fx_4_1 = dgamma(x, shape = 4, rate = 1)

plot(x, fx_16_4, type = "l", col = "skyblue", xlab = "Lambda", ylab='Density', lwd = 3.5,
     main = "Prior plots for 6(b)")
par(new = T)
plot(x, fx_4_1, type = "l", col = "seagreen",xlab = "",ylab = "", yaxt='n', lwd = 3.5)
legend("topright", c("m = 4,sd = 1", "m = 4, sd = 4"), lty=1, col=c("skyblue", "seagreen"))


# (c)

x = rgamma(10000, shape = 24, rate = 103)
range(x)
x = seq(0,1,.001)

fx_36_106 = dgamma(x, shape = 36, rate = 106)
fx_24_103 = dgamma(x, shape = 24, rate = 103)


plot(x, fx_36_106, type = "l", col = "skyblue", xlab = "Lambda", ylab='Density', lwd = 3.5,
     main="Posterior plots problem 6(c)")
par(new = T)
plot(x, fx_24_103, type = "l", col = "seagreen",xlab = "",ylab = "", yaxt='n', lwd = 3.5)
legend("topright", c("m = 4,sd = 1", "m = 4, sd = 4"), lty=1, col=c("skyblue", "seagreen"))

# (d)
c(qgamma(.05,36,106),qgamma(.95,36,106))
c(qgamma(.05,24,103),qgamma(.95,24,103))

