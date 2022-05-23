# rainfall data
x_seeded <- c(151, 450, 124, 235, 357, 110, 302, 671, 118, 115, 275, 275, 2550, 243, 201, 199, 
         130, 119, 92, 91, 92, 98, 1650, 1200, 1180, 900, 700, 460, 340, 330)
x_unseeded <- c(246, 268, 275, 348, 305, 311, 206, 279, 426, 269, 257, 299, 337, 329, 319, 312, 
         327, 342, 351, 205, 151, 426, 154, 353, 396, 441, 254, 263, 278, 281)

# carbon fiber data
x <- c(2.526, 2.546, 2.628, 2.669, 2.869, 2.710, 2.731, 2.751, 2.771, 2.772, 2.782, 
       2.789, 2.793, 2.834, 2.844, 2.854, 2.875, 2.876, 2.895, 2.916, 2.919, 2.957, 2.977, 
       2.988, 3, 3, 3, 3)

# space shuttle data
x <- c(.18, 3.1, 4.2, 6.0, 7.5, 8.2, 8.5, 10.3, 10.6, 24.2, 29.6, 31.7, 41.9, 44.1, 49.5, 
       50.1, 59.7, 61.7, 64.4, 69.7, 70.0, 77.8, 80.5, 82.3, 83.5, 84.2, 87.1, 87.3, 93.2, 
       103.4, 104.6, 105.5, 108.8, 112.6, 116.8, 118.0, 122.3, 123.5, 124.4, 125.4, 129.5, 
       130.4, 131.6, 132.8, 133.8, 137.0, 140.2, 140.9, 148.5, 149.2, 152.2, 152.9, 157.7, 
       160.0, 163.6, 166.9, 170.5, 174.9, 177.7, 179.2, 183.6, 183.8, 194.3, 195.1, 195.3, 
       202.6, 220.0, 221.3, 227.2, 251.0, 266.5, 267.9, 269.2, 270.4, 272.5, 285.9, 292.6, 
       295.1, 301.1, 304.3, 316.8, 329.8, 334.1, 346.2, 351.2, 353.3, 369.3, 372.3, 381.3, 
       393.5, 451.3, 461.5, 574.2, 656.3, 663.0, 669.8, 739.7, 759.6, 894.7, 974.9)

# braided cord data
  
x <- c(19.7, 21.6, 21.9, 23.5, 24.2, 24.4, 24.9, 25.1, 26.4, 26.9, 27.6, 27.7, 27.9, 
       28.4, 29.8, 30.7, 31.1, 31.1, 31.7, 31.8, 32.6, 34.0, 34.8, 34.9, 35.1, 36.6, 37.0, 
       37.7, 38.7, 38.7, 39.0, 39.6, 40.0, 41.4, 41.4, 41.8, 42.2, 43.5, 44.5, 45.0, 45.5, 
       45.9, 46.3, 46.7, 46.7, 47.0, 47.0, 47.4, 47.6, 48.6, 48.8, 57.9, 58.3, 67.9, 84.2, 
       97.3)


# 1.)  An experiment was designed to evaluate whether or not rainfall can be increased by treating
#      clouds with silver iodide. Rainfall was measured from 60 clouds, of which 30 were chosen randomly 
#      to be seeded with silver iodide. The objective is to describe the effect that seeding has on 
#      rainfall. The measurements are the amounts of rainfall in acre-feet from the 60 clouds.


# NOTE: In what follows, you should first check whether the data are Normally distributed. If not, apply 
#       a Box-Cox transformation

x_seeded = sort(x_seeded)

# carbon fiber data ()
x_unseeded = sort(x_unseeded)

##### NOTE: x_seeded needs a boxcox transformation, x_unseeded is already normal

y = x_seeded
n = length(y)
yt0 = log(y)
s = sum(yt0)
varyt0 = var(yt0)
Lt0 = -1*s - .5*n*(log(2*pi*varyt0)+1)
th = 0
Lt = 0
t = -3.01
i = 0
while(t < 3)
{t = t+.001
i = i+1
th[i] = t
yt = (y^t -1)/t
varyt = var(yt)
Lt[i] = (t-1)*s - .5*n*(log(2*pi*varyt)+1)
if(abs(th[i])<1.0e-10)Lt[i]<-Lt0
if(abs(th[i])<1.0e-10)th[i]<-0
}

# The following outputs the values of the likelihood and theta and yields
# the value of theta where likelihood is a maximum
out = cbind(th,Lt)
Ltmax= max(Lt)
Ltmax
imax= which(Lt==max(Lt))
thmax= th[imax]
thmax


iLtci = which(Ltmax - Lt <= 0.5 * qchisq(0.95, 1))
iLtciL = min(iLtci)
iLtciU = max(iLtci)
thLci = th[iLtciL]
thUci = th[iLtciU]

# NOTE: b/c our 95% ci for theta max contains 0, I will use a log transformation instead. 

x_seeded_logtrans = log(x_seeded)
shapiro.test(x_seeded_logtrans)


# (1) Place 95/95 lower tolerance intervals on the amount of rainfall amounts from both seeded and unseeded
#     clouds. If you had to transform one or both of the datasets, create a bound for the transformed data,
#     then back-transform to get a bound on the original scale.

# look at tables to get value of 2.22 (h.O 11 pg 45)
length(x_unseeded)
length(x_seeded)

upper_unseeded = mean(x_unseeded) - 2.22*sd(x_unseeded)
upper_unseeded

upper_seeded = mean(x_seeded_logtrans) + 2.22*sd(x_seeded_logtrans)
upper_seeded = exp(upper_seeded)
upper_seeded


# (2) Place 95% confidence intervals on the average rainfall from both seeded and unseeded clouds. If you
#     had to transform one or both of the datasets, use the studentized bootstrap, because our confidence
#     interval procedures for a mean are not appropriate for transformed data.

tval_.975_df29 = 2.04523
ub_unseeded = mean(x_unseeded) + tval_.975_df29*sd(x_unseeded)/sqrt(length(x_unseeded))
lb_unseeded = mean(x_unseeded) - tval_.975_df29*sd(x_unseeded)/sqrt(length(x_unseeded))
CI95_unseeded = c(lb_unseeded, ub_unseeded)
CI95_unseeded


n= length(x_seeded)
thest = mean(x_seeded)
V = var(x_seeded)/n
B = 9999
W = numeric(B)
W = rep(0,times =B)
for (i in 1:B)
  W[i] = mean(sample(x_seeded,replace=T))
Z = sqrt(n)*(W-thest)/W
Z = sort(Z)
LZ = Z[250]
UZ = Z[9750]
thL = thest-UZ*sqrt(V)
thU = thest-LZ*sqrt(V)
CI95_seeded = c(thL,thU)
CI95_seeded

# (3) Place 95% confidence intervals on the median rainfall from both seeded and unseeded clouds. Note
#     that, since with the Normal distribution, the median equals the mean, you can just apply confidence
#     interval approaches for a mean. If you had to transform one or both of the datasets, go ahead and use
#     the confidence interval approach for a mean and back-transform.

ub_unseeded = median(x_unseeded) + tval_.975_df29*sd(x_unseeded)/sqrt(length(x_unseeded))
lb_unseeded = median(x_unseeded) - tval_.975_df29*sd(x_unseeded)/sqrt(length(x_unseeded))
CI95_unseeded = c(lb_unseeded, ub_unseeded)
CI95_unseeded


ub_seeded = median(log(x_seeded)) + tval_.975_df29*sd(log(x_seeded))/sqrt(length(log(x_seeded)))
ub_seeded = exp(ub_seeded)
lb_seeded = median(log(x_seeded)) - tval_.975_df29*sd(log(x_seeded))/sqrt(length(log(x_seeded)))
lb_seeded = exp(lb_seeded)

CI95_seeded = c(lb_seeded, ub_seeded)
CI95_seeded



# (4) What can you conclude about the effect of the seeding on the amount of rainfall?

# It increases the variance of the rainfall. 



# 2.) Twenty-eight bundles of impregnated carbon fibers of length 20 mm are exposed to gradually
#     increasing stress until they finally fail. The stress at failure are recorded as follows. The maximum stress
#     that can be applied to the fibers is 3 and four of the fibers had not failed at that stress so a value of 3 was
#     assigned to the four fibers:
library(survival)  
library(MASS)

x <- c(2.526, 2.546, 2.628, 2.669, 2.869, 2.710, 2.731, 2.751, 2.771, 2.772, 2.782, 
       2.789, 2.793, 2.834, 2.844, 2.854, 2.875, 2.876, 2.895, 2.916, 2.919, 2.957, 2.977, 
       2.988, 3, 3, 3, 3)
length(x)

# 1.) Estimate with a 95% confidence interval the average stress to failure for the carbon fibers without
#     specifying the distribution of the stress to failure values. Do this two ways: (i) using an asymptotic
#     CI based on the results of the R function survfit, and (ii) using the studentized bootstrap, treating
#     the censored observations as true stress values (i.e., ignoring the censoring).

# (i)
xcens = c(rep(1,times = length(x) - 4), rep(0, times = 4))

Surv(x, xcens)

cords.surv <- survfit(Surv(x, xcens) ~ 1,conf.type="log-log")
summary(cords.surv)
print(cords.surv,print.rmean=TRUE)

# (ii)
n= length(x)
thest = mean(x)
V = var(x)/n
B = 9999
W = numeric(B)
W = rep(0,times =B)
for (i in 1:B)
  W[i] = mean(sample(x,replace=T))
Z = sqrt(n)*(W-thest)/W
Z = sort(Z)
LZ = Z[250]
UZ = Z[9750]
thL = thest-UZ*sqrt(V)
thU = thest-LZ*sqrt(V)
CI95_x = c(thL,thU)
CI95_x

# (2) Estimate with a 95% confidence interval the average stress to failure for the carbon fibers assuming
#      the distribution of the stress to failure values has a Weibull distribution. Do this using the parametric
#      bootstrap. To estimate the Weibull parameters, use the survreg function

fit <- survreg(Surv(x, xcens) ~ 1, dist = "weibull")
shape_est <- 1 / fit$scale
scale_est <- exp(fit$coef)

mu = scale_est*gamma(1+1/shape_est)
mu
sigma = sqrt((scale_est^2)*(gamma(1+2/shape_est) - gamma(1+1/shape_est)^2))
sigma



aD = shape_est
bD = scale_est

n = length(x)
B = 9999
W = matrix(0,B,n)
cv = numeric(B)
cv = rep(0,B)
a = numeric(B)
a = rep(0,B)
b = numeric(B)
b = rep(0,B)
mleest = matrix(0,B,2)
{
  for (i in 1:B)
    W[i,] = rweibull(n,shape = aD, scale = bD)
}
{
  for (i in 1:B)
    mleest[i,] = coef(fitdistr(W[i,],"weibull"))
}
a = mleest[,1]
b = mleest[,2]
mu = (b*gamma(1+1/a))
R = sort(mu)
L = R[250]
U = R[9750]
ci = c(L, U)
ci

mean(x)  


qnorm(.975)  

  

#3.) The National Institute for Standards and Technology conducted a study to develop standards
#    for asbestos concentration. Asbestos dissolved in water was spread on a filter, and punches of 3-mm
#    diameter were taken from the filter and mounted on a transmission electron microscope. An operator
#    counted the number of asbestos fibers on each of 200 grid squares yielding the following counts: (the
#    researcher no longer had the original counts just the following grouped data and the mean of the 200 counts

1-pchisq(4.3694,4)

UB = (2*(200)*(27.7)+qnorm(.975)^2 + qnorm(.975)*sqrt(4*200*27.7-qnorm(.975)^2))/(400)
LB = (2*(200)*(27.7)+qnorm(.975)^2 - qnorm(.975)*sqrt(4*200*27.7-qnorm(.975)^2))/(400)

CI=c(LB,UB)
CI

# 4.) The space shuttle uses epoxy spherical vessels in an environment of sustained pressure. A study
#     of the lifetimes of epoxy strands subjected to sustained stress was conducted. The data giving the lifetimes
#     (in hours) of 100 strands tested at a prescribed level of stress is given in the following table


x <- c(.18, 3.1, 4.2, 6.0, 7.5, 8.2, 8.5, 10.3, 10.6, 24.2, 29.6, 31.7, 41.9, 44.1, 49.5, 
       50.1, 59.7, 61.7, 64.4, 69.7, 70.0, 77.8, 80.5, 82.3, 83.5, 84.2, 87.1, 87.3, 93.2, 
       103.4, 104.6, 105.5, 108.8, 112.6, 116.8, 118.0, 122.3, 123.5, 124.4, 125.4, 129.5, 
       130.4, 131.6, 132.8, 133.8, 137.0, 140.2, 140.9, 148.5, 149.2, 152.2, 152.9, 157.7, 
       160.0, 163.6, 166.9, 170.5, 174.9, 177.7, 179.2, 183.6, 183.8, 194.3, 195.1, 195.3, 
       202.6, 220.0, 221.3, 227.2, 251.0, 266.5, 267.9, 269.2, 270.4, 272.5, 285.9, 292.6, 
       295.1, 301.1, 304.3, 316.8, 329.8, 334.1, 346.2, 351.2, 353.3, 369.3, 372.3, 381.3, 
       393.5, 451.3, 461.5, 574.2, 656.3, 663.0, 669.8, 739.7, 759.6, 894.7, 974.9)


# (1)  Estimate with a 99% confidence interval the probability that an epoxy strand subjected to the 
#      prescribed stress will survive for 300 hours. Use the Agresti-Coull approach.

y_tild = length(which(x>=300)) + ((0.5*qnorm(.995)^2))
n_tild = length(x)+(qnorm(.995)^2)
p_tild = y_tild/n_tild
p_tild

U_Agresti_Coull = p_tild + qnorm(.995)*sqrt(p_tild*(1-p_tild))/sqrt(n_tild) 
L_Agresti_Coull = p_tild - qnorm(.995)*sqrt(p_tild*(1-p_tild))/sqrt(n_tild) 

CI_Agresti_Coull = c(L_Agresti_Coull,U_Agresti_Coull)
CI_Agresti_Coull


# (2)  Estimate with 99% certainty the time, L.95, such that at least 95% of epoxy strands under the prescribed
#      stress would have lifetimes greater than L.95. You can assume that the lifetimes follow an exponential
#      distribution. (see pg 47 on H.O. 11)



p = .95
gamma = .99
n = length(x)
y_bar = mean(x)

U_tol_bound =  - y_bar*(2*n/qchisq(1-gamma,2*n))*log(p)
U_tol_bound




# (3)  Using the above data, predict with 95% certainty the lifetime of a strand subjected to the prescribed
#      stress. Again, assume an exponential distribution.

LB = y_bar*qf(1-.975,2,2*n)
UB = y_bar*qf(.975,2,2*n)
PI_95 = c(LB,UB)
PI_95


# 5.)  An experiment was conducted to determine the strength of a certain type of braided cord after
#      weathering. The strengths of 56 pieces of cord that had been weathered for 30 days were investigated. The
#      56 pieces of cord were placed simultaneously in a tensile strength device. The device applies an increasing
#      amount of force until the cord fails. The following strength readings (psi) are given below


x <- c(19.7, 21.6, 21.9, 23.5, 24.2, 24.4, 24.9, 25.1, 26.4, 26.9, 27.6, 27.7, 27.9, 
       28.4, 29.8, 30.7, 31.1, 31.1, 31.7, 31.8, 32.6, 34.0, 34.8, 34.9, 35.1, 36.6, 37.0, 
       37.7, 38.7, 38.7, 39.0, 39.6, 40.0, 41.4, 41.4, 41.8, 42.2, 43.5, 44.5, 45.0, 45.5, 
       45.9, 46.3, 46.7, 46.7, 47.0, 47.0, 47.4, 47.6, 48.6, 48.8, 57.9, 58.3, 67.9, 84.2, 
       97.3)



# (1)  The manufacturer of the cords would like to estimate the proportion of weathered cords having tensile
#      strength less than 50 psi. Provide a 95% confidence interval based on the information from the failure
#      data from the 56 cords.


# Using Wald CI

p_hat = length(which(x<=50))/length(x)
n = length(x)

UB = p_hat + qnorm(.975)*sqrt(p_hat*(1-p_hat))/sqrt(n)
LB = p_hat - qnorm(.975)*sqrt(p_hat*(1-p_hat))/sqrt(n)
CI_Phat = c(LB,UB)
CI_Phat

# (2)  The manufacturer is planning a sales campaign to promote its cord and would like to state a tensile
#      strength value for its cord. Provide the manufacturer with a 95% confidence interval that would provide
#      an estimate of the median tensile strength value for the weathered braided cord.

# Use distribution free extimate for Q(u) (H.O. 11 pg 31)

n=length(x)
L=.95
P=.5
s=ceiling(n*P)-1
r=floor(n*P)+1
cov=0
while(s<n-1 && r>1 && cov<L)
{s=s+1
cov=pbinom(s-1,n,P)-pbinom(r-1,n,P)
if(cov>=L) break;
r=r-1
cov=pbinom(s-1,n,P)-pbinom(r-1,n,P)
}
r
s
cov

x = sort(x)

CI = c(x[r], x[s])
CI


# (3) In order to determine if the braided cord has tensile strength that falls within federal specifications, 
#     the manufacturer wants to determine an interval of strength values, (TL, TU ), such that the manufacturer
#     would be 95% confident that the interval would contain at least 90% of all strength values for its
#     braided cords

shapiro.test(x)
hist(x)

# do box-cox transformation to normality

y = x
n = length(y)
yt0 = log(y)
s = sum(yt0)
varyt0 = var(yt0)
Lt0 = -1*s - .5*n*(log(2*pi*varyt0)+1)
th = 0
Lt = 0
t = -3.01
i = 0
while(t < 3)
{t = t+.001
i = i+1
th[i] = t
yt = (y^t -1)/t
varyt = var(yt)
Lt[i] = (t-1)*s - .5*n*(log(2*pi*varyt)+1)
if(abs(th[i])<1.0e-10)Lt[i]<-Lt0
if(abs(th[i])<1.0e-10)th[i]<-0
}

# The following outputs the values of the likelihood and theta and yields
# the value of theta where likelihood is a maximum
out = cbind(th,Lt)
Ltmax= max(Lt)
Ltmax
imax= which(Lt==max(Lt))
thmax= th[imax]
thmax


iLtci = which(Ltmax - Lt <= 0.5 * qchisq(0.95, 1))
iLtciL = min(iLtci)
iLtciU = max(iLtci)
thLci = th[iLtciL]
thUci = th[iLtciU]
thLci
thUci

# note that our 95% CI for thmax contains 0 => use log trans of the data.
length(x)
x = log(x)
shapiro.test(x)

# look at tables to get value of 1.99 (h.O 11 pg 45)
UB = mean(x) + 1.999*sd(x)
UB = exp(UB)

LB = mean(x) - 1.999*sd(x)
LB = exp(LB)

TI_95 = c(LB,UB)
TI_95



















