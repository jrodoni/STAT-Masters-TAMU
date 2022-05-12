####
#### Script for chapter 2 notes.
####

##
## A simulated simple linear regression problem.
##

## Parameters.
beta <- c(0.5, 1.5)
sg <- 0.25
n <- 50

## Simulate x variable and use it to generate y variable.
x <- runif(n)
X <- cbind(1, x)
y <- X %*% beta + rnorm(n, 0, sg)

## Scatterplot with population line overlaid.
plot(x, y)
abline(beta[1], beta[2])

## Fit SLR model using 'lm' function.
fit <- lm(y ~ x)
summary(fit)

## Match estimates manually.
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
ee <- residuals(fit)
sg_hat <- sqrt(var(ee) * (n - 1) / (n - 2))
se_hat <- sqrt(diag(sg_hat ^ 2 * solve(t(X) %*% X)))
