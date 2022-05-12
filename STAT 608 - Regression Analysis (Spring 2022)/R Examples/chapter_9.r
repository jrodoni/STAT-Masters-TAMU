####
#### Sales vs. Price example
####
rm(list = ls())


dta <- read.delim("C:/Users/jackr/OneDrive/Desktop/Graduate School Courses/STAT 608 - Regression Analysis/Data/confood2.txt")
attach(dta)
detach(dta)
n <- nrow(dta)


## Figure 9.1
plot(log(Price), log(Sales), xlab = expression(log(Price[t])), 
  ylab = expression(log(Sales[t])), type = "n")
points(log(Price)[Promotion == 0], log(Sales)[Promotion == 0], pch = 2)
points(log(Price)[Promotion == 1], log(Sales)[Promotion == 1], pch = 3)
legend(-0.3, 8.5, legend = c("Promotion = No", "Promotion = Yes"), pch = c(2, 3))

## Figure 9.2
plot(Week, log(Sales), xlab = expression(Week), ylab = expression(log(Sales[t])), 
  type = "n")
points(Week[Promotion == 0], log(Sales)[Promotion == 0], pch = 2)
points(Week[Promotion == 1], log(Sales)[Promotion == 1], pch = 3)
lines(Week, log(Sales))
legend(0, 8.5, legend = c("Promotion = No", "Promotion = Yes"), pch = c(2, 3))

## Figure 9.3
plot(log(Sales)[2:n], log(Sales)[1:(n - 1)], xlab = expression(log(Sales[t-1])), 
  ylab = expression(log(Sales[t])))

cor(log(Sales)[2:n], log(Sales)[1:(n - 1)])
  
## Figure 9.4
acf(log(Sales))[1]



## Figure 9.5
fit <- lm(log(Sales) ~ log(Price) + Week + Promotion)
ee <- rstandard(fit)
y_hat <- fitted(fit)

cor.test(ee[2:n],ee[1:(n-1)])

par(mfrow = c(2, 2), mar = c(5, 4, 1, 2) + 0.1)
plot(log(Price), ee, xlab = expression(log(Price[t])), ylab = "Standardized Residuals")
plot(Week, ee, type = "b", xlab = "Week", ylab = "Standardized Residuals")
plot(Promotion, ee, xlab = "Promotion", ylab = "Standardized Residuals")
plot(y_hat, ee, xlab = "Fitted Values", ylab = "Standardized Residuals")

## Figure 9.6
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
acf(ee, main = "Autocorrelation of Std. Residuals")

## GLS
library(nlme)

fit_gls <- gls(log(Sales) ~ log(Price) + Promotion + Week, method = "ML", 
  correlation = corAR1(form = ~ Week))
ee <- residuals(fit_gls)

## Figure 9.7
acf(ee, main = "Autocorrelation of Residuals")

## LS on transformed AR(1) data
Sigma <- corAR1(0.5503593, form = ~ Week)
Sigma <- Initialize(Sigma, data = dta)
Sigma <- 0.2740294 ^ 2 * corMatrix(Sigma)

SS <- chol(Sigma)

X <- model.matrix(~ log(Price) + Promotion + Week)

Ystar <- solve(t(SS)) %*% log(Sales)
Xstar <- solve(t(SS)) %*% X

fit_lm <- lm(Ystar ~ Xstar - 1)
summary(fit_lm)

## Plots of log(Sales)* vs. transformed model terms
par(mfrow = c(2, 2), mar = c(5, 4, 1, 2) + 0.1)
plot(Xstar[, 1], Ystar, xlab = "Intercept*", ylab = "log(Sales)*")
text(3.55, 23.4, "1")
plot(Xstar[, 2], Ystar, xlab = "log(Price)*", ylab = "log(Sales)*")
text(-1.55, 23.4, "1")
plot(Xstar[, 3], Ystar, xlab = "Promotion*", ylab = "log(Sales)*")
text(0.3, 23.4, "1")
plot(Xstar[, 4], Ystar, xlab = "Week*", ylab = "log(Sales)*")
text(8, 23.4, "1")

## ACF plot for LS residuals
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
ee <- rstandard(fit)
acf(ee, main = "LS Standardized Residuals")

## Diagnostic plots for LS model
par(mfrow = c(2, 2), mar = c(5, 4, 2, 2) + 0.1)
plot(fit)

detach(dta)












