####
#### Attempt to reproduce the inverse response plot and generated example of
#### Figure 3.28 in the book.
####

library(minpack.lm)
library(car)

## Simulate the data.
n <- 250
beta_0 <- 0
beta_1 <- 1
sg <- 0.1

x <- runif(n, 0, 4.5)
y <- (beta_0 + beta_1 * x + rnorm(n, 0, sg)) ^ 3
y <- y + min(y) + 1e-5

plot(x, y)

plot(x, y ^ 0.33)
abline(beta_0, beta_1)

## Fit linear regression model to the data.
fit_lin <- lm(y ~ x)
y_hat_lin <- fitted(fit_lin)

## Inverse response plot.
plot(y, y_hat_lin)

## Use 'inverseResponsePlot' function from 'car' package to use 'nls' to find
## optimal lambda in the function
##
## y_hat = b0 + b1 x y ^ lambda
##
## Note that the 'alr' package mentioned in the book and some of the class code
## has been deprecated.

inverseResponsePlot(fit_lin)

####
#### Demonstrate property of leverage statistics: h_{ii} = \sum_j h_{ij}^2.
####

n <- 10
sg <- 0.1
beta_0 <- 0
beta_1 <- 1

x <- runif(n, 0, 1)
y <- beta_0 + beta_1 * x + rnorm(n, 0, sg)

plot(x, y)

## Design and hat matrices.
X <- cbind(1, x)
H <- X %*% solve(t(X) %*% X) %*% t(X)

H[1, 1]
sum(H[, 1] ^ 2)
