######################################## STAT 642 Assignment 3 #####################################

########## Problem 1

#### 1(c)
# install.packages("lsmeans")
library(lsmeans)

treatment = as.factor(c(rep("Premolt", 5), rep("Fasting", 5), rep("60Grams", 5), rep("80Grams", 5), rep("Laymash",5)))
counts = c(94.09, 90.45, 99.38, 73.56, 74.39,
           98.81, 103.55, 115.23, 129.06, 117.61,
           197.18, 207.31, 177.50, 226.05, 222.74,
           102.93, 117.51, 119.92, 112.01, 101.10,
           83.14, 89.59, 87.76, 96.43, 82.94)

data = data.frame(Treat=treatment, TCOUNT=counts)

model = with(data,(lm(TCOUNT ~ Treat)))
summary(model)

A = aov(model)
summary(A)

#### 1(d)

lsmeans(model, "Treat")



########## Problem 2


treatment = as.factor(c(rep("Premolt", 4), rep("Fasting", 5), rep("60Grams", 3), rep("80Grams", 5), rep("Laymash",4)))
counts = c(94.09, 90.45, 99.38, 73.56,
           98.81, 103.55, 115.23, 129.06, 117.61,
           197.18, 207.31, 177.50,
           102.93, 117.51, 119.92, 112.01, 101.10,
           83.14, 89.59, 87.76, 82.94)

data = data.frame(Treat=treatment, TCOUNT=counts)

model = with(data,(lm(TCOUNT ~ Treat)))
summary(model)

A = aov(model)
summary(A)


#### 2(b)
lsmeans(model, "Treat")



########## Problem 3
means = c(20,18,16)
power.anova.test(groups = 3, n =, between.var = var(means),within.var = 12,sig.level = .01,power = .9)


########## Problem 4
alpha = 0.05
gamma = 0.90
t = 5
D = 30
sigma = sqrt(150)

rm(alpha,gamma,t,D,sigma)

repApp5 <- function(alpha, gamma, t, D, sigma)
{ r <- 1
power <- 0
nu1 <- t-1
while(power < gamma) {
  r <- r+1
  nu2 <- t*(r-1)
  L <- r*D^2/(2*sigma^2)
  Phi <- sqrt(L/t)
  Fcr <- qf(1-alpha, nu1, nu2)
  power <- 1-pf(Fcr, nu1, nu2,L) }
print(cbind(D,t,r, nu2, L, Fcr, Phi, power)) }



repApp5(alpha,gamma,t,D,sigma)


########### Problem 7

#### (b)
qt(.975,df=196)
CI_95_B = c((37.5-11.5)-qt(0.975,196)*(19.45/sqrt(50)),(37.5-11.5)+qt(0.975,196)*(19.45/sqrt(50)))
CI_95_B

#### (C)
CI_95_B1 = c(-11.5-qt(.975,196)*19.54/5,-11.5+qt(.975,196)*19.54/5)
CI_95_B1












