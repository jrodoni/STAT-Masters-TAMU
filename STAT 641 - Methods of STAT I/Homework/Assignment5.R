# Stat 641 Assignment # 5

# 2.) Nylon bars were tested for brittleness. Each of 500 bars was molded under similar conditions
#     and was tested by placing a specified stress at 5 locations on the bar. Assuming that each bar has uniform
#     composition, the number of breaks on a given bar should be binomially distributed with an unknown
#     probability p of breaking. The following table summarizes the outcome of the experiment:

# Use a GOF test to evaluate whether the data appears to be from a binomial model.

### our data
n = 0:5
data = c(140,197,115,41,5,2)
theta_hat = sum((n*data))/(5*500)



sumtable = matrix(nrow = 5, ncol = max(n+1))
rownames(sumtable) = c("i","p_hat_sub_i","E_hat_sub_i","O_sub_i","Q_hat")
for(i in n){
  datatemp = c(i+1,
               dbinom(i,max(n),prob = theta_hat),
               500*dbinom(i,max(n),theta_hat),
               data[i+1],
               (data[i+1]-500*dbinom(i,max(n),theta_hat))^2/(500*dbinom(i,max(n),theta_hat)))
  sumtable[,i+1] = datatemp
}
sumtable


# NOTE: Need to combine last two cells becasue E6 < 1
q_hat_5_or_6 = ((7-sum(sumtable[3,c(5,6)]))^2)/sum(sumtable[3,c(5,6)])
sumtable[,5] = sumtable[,5]+sumtable[,6]
sumtable = sumtable[,-6]
sumtable[5,5] = q_hat_5_or_6
sumtable

q_hat = sum(sumtable["Q_hat",])
q_hat

p_val = 1-pchisq(q_hat,df=3)
p_val

# 3.) A random sample of 500 data values are selected from four separate processes having cdf's,
#     F1, F2, F3, F4. The plot of the sample quantile versus a standard normal quantile for each of the 
# four samples is given below. For each of these plots, SELECT ONE of the following distributions to
# describe the pdf which generated the data. Hint: make sure to take into consideration the size of
# values associated with each distribution.

# logistic(theta1 = 0.35, theta2 = 1)
x = rlogis(n = 500, location = 0.35, scale = 1)
qqnorm(x)
qqline(x)

# Beta(alpha = 0.1, Beta = 0.7)
x = rbeta(n = 500,0.1, .7)
qqnorm(x)
qqline(x)

# Exponential(Beta = 100)
x = rexp(500, rate = 1/100)
qqnorm(x)
qqline(x)

# Weibull(gamma = 1.2, alpha = 25)
x = rweibull(500, shape = 1.2, scale = 25)
qqnorm(x)
qqline(x)

qweibull(.5, shape = 1.2, scale = 25)


# Gamma(a = 2, B = 25)

x = rgamma(500, shape = 2, scale = 25)
qqnorm(x)
qqline(x)

# Mixture of 90% Normal(10,1) & 10% Normal(30,(3)^2)
x = c(rnorm(90,mean = 10, sd = 1),rnorm(10,30,3))
qqnorm(x)
qqline(x)

# Mixture of 80% Normal(5,1) & 20% Normal(25,(3)^2)
x = c(rnorm(80,mean = 5, sd = 1),rnorm(20,25,3))
qqnorm(x)
qqline(x)


# # 4.) An experiment was conducted to investigate if the impact of the carcinogen DMBA could be delayed
#       by treatment with a potential beta-blocker. Fifty mature rats of the same general health were 
#       given the beta-blocker and then were injected with DMBA. The times in days, after exposure, at 
#       which the carcinoma was diagnosed for the rats are given below. From past studies, 150 days 
#       after exposure, a carcinoma was detected in untreated rats.

#       Does a Weibull Distribution appear to provide an adequate fit to the data? Justify your answer 
#       using both a GOF test and a graphical plot.


library(MASS)

data = c(10,12,13,16,37,42,43,45,55,63,66,82,99,100,100,101,107,117,122 
         ,135,138,140,142,149,150,151,154,165,170,183,194,214,218,219,
         224,229,232,247,268,268,298,299,325,332,379,400,434,464,499,537)
n = length(data)
i = seq(1,n,1)

# Anderson Darling
fit = fitdistr(data,"weibull")
shape = fit$estimate[1]
scale = fit$estimate[2]

ln_tans_data = -log(data)
y = sort(ln_tans_data)

a = -log(scale)
b = 1/shape

z = exp(-exp(-(y-a)/b))
A1i = (2*i-1)*log(z)
A2i = (2*n+1-2*i)*log(1-z)

s1 = sum(A1i)
s2 = sum(A2i)

AD = -n-(1/n)*(s1+s2)
ADM = AD*(1+.2/sqrt(n))
AD
ADM

ad.test(data)


# QQ Plot
n
n = length(y)
weib= -y
weib= sort(weib)
i= 1:n
ui= (i-.5)/n
QW= log(-log(1-ui))
plot(QW,weib,abline(lm(weib~QW)),
     main="Weibull Reference Plot",cex=.75,
     xlab="Q=ln(-ln(1-ui))",
     ylab="y=ln(W(i))")



# 5.) A major problem in the Gulf of Mexico is the excessive capture of game fish by shrimpers.
#     A random sample of the catch of 50 shrimpers yield the following data concerning the catch per unit effort
#     (CPUE) of Red Snappers, a highly sought game fish. Let Ci be the CPUE for the ith shrimper. The data,
#     C1, C2, . . . , C50 is given next.

data = c(0.6,0.7,1.1, 1.3,1.8,2.0,2.3,2.7,2.9,
         3.1,3.9,4.3,4.4,4.9,5.2,5.4,6.1,6.8,
         7.1,8.0,9.4,10.3,12.9,15.9,16.0,22.0,
         22.2,22.5,23.0,23.1,23.9,26.5,26.7,28.4,
         28.5,32.2,40.2,42.5,47.2,48.3,55.8,57.0,
         57.2,64.9,67.6,71.3,79.5,114.5,128.6,293.5)
# (1) CPUE data is often modeled using a Log-Normal distribution. Does the above data appear to be from
#     a Log-Normal distribution? Explain your answer with both a normal reference distribution plot and a
#     GOF test.

ldata = log(data)
x = sort(ldata)

# Shapiro-Wilks computations:
# NOTE: the values of u and sigma do NOT need to be specifed for the shapiro wilks test
shapiro.test(x)

# QQplot and correlation Test
qqnorm(x)
qqline(x)
n = length(data)
i = seq(1,n,1)
u = (i-.375)/(n+.25)
q = qnorm(u)
r = cor.test(q,x)

# cor .9906 with n = 50 => p-value = 0.50


# (2) Use the Box-Cox transformation of the CPUE data to determine the most appropriate power transformation to transform the CPUE distribution to Normality. How does the fit from the Box-Cox
#     transformation compare to the fit for the log transformation?

y = data
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

data = data^thmax
shapiro.test(data)

# plot of thetas with confidence intervals
plot(th,Lt,lab=c(30,50,7), type = "l",
     main="Box-Cox Transformations",
     xlab=expression(theta),
     ylab=expression(Lt(theta)))

#the following plots a 95\% c.i. for theta
cic = Ltmax-.5*qchisq(.95,1)
del= .01
iLtci = which(abs(Lt-cic)<=del)
iLtciL= min(iLtci)
iLtciU= max(iLtci)
thLci= th[iLtciL]
thUci= th[iLtciU]
abline(h=cic)
abline(v=thLci)
abline(v=thUci)
abline(v=thmax)





# (3) Use the R program from Handout 10 (or any other program of your choice) to draw 10,000 bootstrap
#     samples from the CPUE data. From the 10,000 samples, estimate the standard error of the sample
#     mean for the Yi = log(Ci), data. Compare this estimate to the usual estimate Sy/sqrt(n), where SY is the
#     sample standard deviation computed from the n = 50 values of Yi = log(Ci)


data = c(0.6,0.7,1.1, 1.3,1.8,2.0,2.3,2.7,2.9,
         3.1,3.9,4.3,4.4,4.9,5.2,5.4,6.1,6.8,
         7.1,8.0,9.4,10.3,12.9,15.9,16.0,22.0,
         22.2,22.5,23.0,23.1,23.9,26.5,26.7,28.4,
         28.5,32.2,40.2,42.5,47.2,48.3,55.8,57.0,
         57.2,64.9,67.6,71.3,79.5,114.5,128.6,293.5)
x = log(data)
x = sort(x)
n = length(x)
# usual way to calculate the standard error of the sample 
std_error_sample = sd(x)/sqrt(n)
std_error_sample
# calculate the standard error of the sample mean 
r = 10000
m = rep(0,r)
for(i in 1:r){
  y = sample(x,replace = T)
  m[i] = mean(y)
}
sd(m)


# (4) Use your bootstrap samples to estimate the mean and standard deviation of the following sample
#     statistics for Y = log(C)

data = c(0.6,0.7,1.1, 1.3,1.8,2.0,2.3,2.7,2.9,
         3.1,3.9,4.3,4.4,4.9,5.2,5.4,6.1,6.8,
         7.1,8.0,9.4,10.3,12.9,15.9,16.0,22.0,
         22.2,22.5,23.0,23.1,23.9,26.5,26.7,28.4,
         28.5,32.2,40.2,42.5,47.2,48.3,55.8,57.0,
         57.2,64.9,67.6,71.3,79.5,114.5,128.6,293.5)
x = log(data)
x = sort(x)
n = length(x)
r=10000

q = NULL
SD = NULL
MAD = NULL
for (i in 1:r){
  y = sample(x,replace = T)
  q[i] = median(y)
  SD[i] = sd(y)
  MAD[i] = mad(y)
}


means = c(mean(q),mean(SD),mean(MAD))
sds = c(sd(q),sd(SD),sd(MAD))
means_and_sds = cbind(means,sds)
means_and_sds



# 6.) A company has designed a new battery system for electric powered automobiles.  
#     To estimate the lifetime of the system, the design engineers place the batteries in 
#     25 electric powered cars and test themunder simulated city driving.  Let Yi be the 
#     time to failure of the batteries of the ith car,i= 1,...,25.The  failure  times  
#     are  recorded  in  units  of  20,000  miles.   The  company  wants  to  know  the  
#     probability that the sample mean based on 25 observations will estimate the true mean
#     within a margin of error of + or - .2 (4000 miles), provided that the true mean has a
#     value of 5 (100,000 miles), that is, approximate, P[-0.2<= Ybar - 5<=0.2].

# (2) Simulate 10,000 random samples of size 25 from the an exponential distribution with ?? = 5. Compute
#     the sample mean from each of the 10,000 samples. Display a normal distribution reference plot for
#     the 10,000 sample means. Does the plot suggest that the sampling distribution of Y¯ is approximately
#     normal?
  
r = 10000
m = NULL

for(i in 1:r){
  x = rexp(25, rate = 1/5)
  m[i] = mean(x)
}


qqnorm(m)
qqline(m)

hist(m)
hist(rgamma(10000,shape = 25, scale = 1/5))
qqnorm(sort(m))
qqline(sort(m))

shapiro.test(sample(m, size = 5000, replace = T))

# (3) Compute or estimate P[???0.2 ??? Y¯ ??? 5 ??? 0.2] in each of the following manners:

#     Exact dist of Y bar
pgamma(5.2, shape = 25, scale = 1/5) - pgamma(4.8, shape = 25, scale = 1/5) 

#     Central Limit Theorem
pnorm(.2) - pnorm(-.2)
pnorm(5.2, mean = 5) - pnorm(4.8,mean = 5)


#     Simulated 10000 Y bars
(length(which(m <= 5.2)) - length(which(m <= 4.8)))/10000

