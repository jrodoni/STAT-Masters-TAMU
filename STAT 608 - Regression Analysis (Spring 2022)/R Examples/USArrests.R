####
#### Data on crime statistics for 50 U.S. states. The data come from the book
#### Introduction to Statistical Learning and are obtained through the ISLR R
#### package that accompanies this book. The variables Murder, Assault, and
#### Rape are numbers of crimes per 100,000 people (I think), and UrbanPop is
#### the percentage of the population that lives in an urban environment. We
#### will consider models for UrbanPop in terms of the other three explanatory
#### variables.
####

library(ISLR)
library(car)

data(USArrests)
attach(USArrests)

##
## Basic exploration.
##

## Means, medians, and sds.
apply(USArrests, 2, mean)
apply(USArrests, 2, median)
apply(USArrests, 2, sd)

## Histograms and boxplots. Murder and Rape are slightly right-skewed. No
## obvious outliers.
par(mfrow = c(1, 2))
hist(Murder)
boxplot(Murder)
hist(Assault)
boxplot(Assault)
hist(Rape)
boxplot(Rape)
hist(UrbanPop)
boxplot(UrbanPop)

## Pairwise scatterplots. Murder and Assault apparently correlated. Also Rape
## to a lesser extent. UrbanPop looks weakly linearly associated with the
## explanatory variables.
plot(USArrests)

## Correlation matrix. Supports our conclusion from the previous plots.
cor(USArrests)

##
## See 'pca.R' for PCA analysis of these data.
##

##
## See 'cluster.R' for cluster analysis of these data.
##

##
## Model building.
##

fit_1 <- lm(UrbanPop ~ Murder + Assault + Rape)

## Rape is statistically significant, and Murder is nearly. Assault is not.
summary(fit_1)

## A little bit of a pattern to the plot of residuals vs. fitted values,
## suggesting that either a transformation or additional explanatory variables
## may be merited. Also a little bit of a decreasing pattern to the plot of
## square root of standardized residuals vs. fitted values, suggesting unequal
## variances.
par(mfrow = c(2, 2))
plot(fit_1)

## Added variable plots. Assault looks weakest, as suggested by the relatively
## large p-value for its regression coefficient above.
avPlots(fit_1)

## VIFs. Nothing exceeds our threshold of 5, so no major concern about
## multicollinearity.
vif(fit_1)

## Drop Assault from the model. Residual diagnostics look pretty good. Still a
## bit of a decreasing pattern to the third plot (suggesting non-constant
## variance).
fit_2 <- lm(UrbanPop ~ Murder + Rape)
summary(fit_2)
par(mfrow = c(2, 2)); plot(fit_2)

## Marginal model plots. Both plots look reasonable.
my.mmplot <- function(model, y, variable, xlabel="Predictor", by.var=0.01){
  loessfit1 <- loess(y ~ variable, degree=1, span=2/3)
  loessfit2 <- loess(model$fitted.values ~ variable, degree=1, span=2/3)
  xx <- seq(min(variable), max(variable), by.var)
  plot(variable, y, xlab=xlabel)
  lines(xx,predict(loessfit1,data.frame(variable=xx)), col="blue", lwd=2)
  lines(xx,predict(loessfit2,data.frame(variable=xx)), lty=2, col="red", lwd=2)
}

par(mfrow = c(1, 2))
my.mmplot(model = fit_2, y = UrbanPop, variable = Murder, xlabel = "Murder")
my.mmplot(model = fit_2, y = UrbanPop, variable = Rape, xlabel = "Rape")

## Use Box-Cox to explore possible power transformations of the variables.
## We might try log transformations for Murder and Rape and a square root
## transformation for Assault. No transformation is apparently needed for
## UrbanPop.
X <- cbind(Murder, Assault, Rape)
tranx <- powerTransform(X, family="yjPower")
summary(tranx)

tranmod <- powerTransform(fit_1, family = "yjPower")
summary(tranmod)

Murder_x <- log(Murder)
Assault_x <- sqrt(Assault)
Rape_x <- log(Rape)
fit_3 <- lm(Murder_x + Assault_x + Rape_x)
