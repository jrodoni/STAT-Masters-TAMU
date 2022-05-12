####
#### Impacts of changing logistic regression coefficients on logistic curve.
####

##
## Changing the model intercept.
##

beta_0 <- 0
beta_1 <- 1
x <- seq(from = -5, to = 5, length = 100)
log_odds <- beta_0 + beta_1 * x
p <- exp(log_odds) / (1 + exp(log_odds))

plot(x, p, type = "l")

beta_0 <- 1
log_odds <- beta_0 + beta_1 * x
p <- exp(log_odds) / (1 + exp(log_odds))

lines(x, p, col = "red")

beta_0 <- -1
log_odds <- beta_0 + beta_1 * x
p <- exp(log_odds) / (1 + exp(log_odds))

lines(x, p, col = "blue")

legend(-4, 0.9, legend = c("b0 = 0", "b0 = 1", "b0 = -1"), lty = rep(1, 3),
  col = c("black", "red", "blue"))

##
## Changing the model slope.
##

beta_0 <- 0
beta_1 <- 1
x <- seq(from = -5, to = 5, length = 100)
log_odds <- beta_0 + beta_1 * x
p <- exp(log_odds) / (1 + exp(log_odds))

plot(x, p, type = "l")

beta_1 <- 0.5
log_odds <- beta_0 + beta_1 * x
p <- exp(log_odds) / (1 + exp(log_odds))

lines(x, p, col = "red")

beta_1 <- 1.5
log_odds <- beta_0 + beta_1 * x
p <- exp(log_odds) / (1 + exp(log_odds))

lines(x, p, col = "blue")

legend(-4, 0.9, legend = c("b1 = 1", "b1 = 0.5", "b1 = 1.5"), lty = rep(1, 3), 
  col = c("black", "red", "blue"))
  
####
#### Michelin guide and model diagnostic.
####

dta <- read.csv("MichelinNY.csv")

## Fit logistic regression model.
fit <- glm(InMichelin ~ Food, data = dta, family = binomial)
summary(fit)

##
## Compare fitted values to non-parametric fit in plot of residuals.
##

plot(dta$Food, jitter(dta$InMichelin, amount = 0.01))

## Fitted values from model.
p_hat <- predict(fit, type = "response")
food_oo <- order(dta$Food)
food_sort <- dta$Food[food_oo]
p_hat_sort <- p_hat[food_oo]

lines(food_sort, p_hat_sort, col = "blue", lwd = 2)

## Add lowess curve.
lines(lowess(dta$Food, dta$InMichelin), col = "red", lwd = 2)

####
#### Can you exponentiate endpoints of CI on log(OR) scale to get CI on OR scale? YES.
####

set.seed(23401)

beta_0 <- 0.5
beta_1 <- 1.25

## Odds ratio for one-unit increase in x.
OR_0 <- exp(beta_1)
log_OR_0 <- beta_1

B <- 5000
n <- 50
CI_log_OR <- CI_OR <- matrix(NA, nrow = B, ncol = 2)
for(b in 1:B) {
  x <- runif(n)
  p_x <- exp(beta_0 + beta_1 * x) / (1 + exp(beta_0 + beta_1 * x))
  y <- rbinom(n, size = 1, prob = p_x)
  
  ## Logistic regression.
  fit <- glm(y ~ x, family = binomial)
  cf_fit <- summary(fit)$coef
  beta_1_hat <- cf_fit[2, 1]
  se_b1_hat <- cf_fit[2, 2]
  
  ## CIs.
  CI_log_OR[b, ] <- beta_1_hat + c(-1, 1) * 1.96 * se_b1_hat
  CI_OR[b, ] <- exp(CI_log_OR[b, ])
}

## CI coverage.
cvrg_log_OR <- log_OR_0 >= CI_log_OR[, 1] & log_OR_0 <= CI_log_OR[, 2]
cvrg_OR <- OR_0 >= CI_OR[, 1] & OR_0 <= CI_OR[, 2]
mean(cvrg_log_OR)
mean(cvrg_OR)

