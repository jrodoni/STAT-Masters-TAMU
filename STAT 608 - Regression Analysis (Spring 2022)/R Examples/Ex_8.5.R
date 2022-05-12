dta <- read.csv("Ex_8.5.csv")
n <- nrow(dta)

## Randomly split data into half for training and testing.
set.seed(93420)
ii_train <- sample(1:n, n / 2, replace = FALSE)
dta_train <- dta[ii_train, ]
dta_test <- dta[-ii_train, ]
n_train <- n_test <- nrow(dta_train)

####
#### PART 1
####

attach(dta_train)

##
## (a)
##

fit_a <- glm(TARGET_B ~ homeowner.dummy + NUMCHLD + INCOME + gender.dummy + WEALTH + 
  HV + Icmed + Icavg + IC15 + NUMPROM + RAMNTALL + MAXRAMNT + LASTGIFT + totalmonths + 
  TIMELAG + AVGGIFT, family = binomial)
  
##
## (b)
##

## Marginal model plots.
my.mmplot <- function(model, variable, xlabel="Predictor", by.var=0.01){
  y <- dta_train[, 23]
  loessfit1 <- loess(y ~ variable, degree=1, span=2/3)
  loessfit2 <- loess(model$fitted.values ~ variable, degree=1, span=2/3)
  xx <- seq(min(variable), max(variable), by.var)
  plot(variable, y, xlab=xlabel)
  lines(xx,predict(loessfit1,data.frame(variable=xx)), col="blue", lwd=2)
  lines(xx,predict(loessfit2,data.frame(variable=xx)), lty=2, col="red", lwd=2)
}

## Plots marked as * look bad. Overall, evidence of invalid model.
my.mmplot(model = fit_a, variable = homeowner.dummy, xlabel = "homeowner.dummy")
my.mmplot(model = fit_a, variable = NUMCHLD, xlabel = "NUMCHLD")
my.mmplot(model = fit_a, variable = INCOME, xlab = "INCOME")
my.mmplot(model = fit_a, variable = gender.dummy, xlab = "gender.dummy")
my.mmplot(model = fit_a, variable = WEALTH, xlab = "WEALTH")
my.mmplot(model = fit_a, variable = HV, xlab = "HV")
my.mmplot(model = fit_a, variable = Icmed, xlab = "Icmed") 
my.mmplot(model = fit_a, variable = Icavg, xlab = "Icavg") 
my.mmplot(model = fit_a, variable = IC15, xlab = "IC15") ## *
my.mmplot(model = fit_a, variable = NUMPROM, xlab = "NUMPROM") ## *
my.mmplot(model = fit_a, variable = RAMNTALL, xlab = "RAMNTALL") ## *
my.mmplot(model = fit_a, variable = MAXRAMNT, xlab = "MAXRAMNT") ## *
my.mmplot(model = fit_a, variable = LASTGIFT, xlab = "LASTGIFT") ## *
my.mmplot(model = fit_a, variable = totalmonths, xlab = "totalmonths")
my.mmplot(model = fit_a, variable = TIMELAG, xlab = "TIMELAG") ## *
my.mmplot(model = fit_a, variable = AVGGIFT, xlab = "AVGGIFT") ## *

##
## (c)
##

## Density plots.
my.density <- function(variable, xlabel = "Predictor") {
  par(mfrow = c(1, 2))
  
  plot(density(variable[TARGET_B == 0]), xlab = xlabel, main = "TARGET_B = 0")
  plot(density(variable[TARGET_B == 1]), xlab = xlabel, main = "TARGET_B = 1")
}

my.density(NUMCHLD, xlabel = "NUMCHLD") # Right-skewed
my.density(INCOME, xlab = "INCOME") # Approximately symmetric with equal variances
my.density(WEALTH, xlab = "WEALTH") # Left-skewed. Multiply by -1 to make right-skewed
my.density(-WEALTH, xlab = "-WEALTH")
my.density(HV, xlab = "HV") # Right-skewed
my.density(Icmed, xlab = "Icmed") # Right-skewed
my.density(Icavg, xlab = "Icavg") # Approximately symmetric with unequal variances
my.density(IC15, xlab = "IC15") # Right-skewed
my.density(NUMPROM, xlab = "NUMPROM") # Right-skewed
my.density(RAMNTALL, xlab = "RAMNTALL") # Right-skewed
my.density(MAXRAMNT, xlab = "MAXRAMNT") # Right-skewed
my.density(LASTGIFT, xlab = "LASTGIFT") # Right-skewed
my.density(totalmonths, xlab = "totalmonths") # Left-skewed. Multiply by -1
my.density(-totalmonths, xlab = "-totalmonths")
my.density(TIMELAG, xlab = "TIMELAG") # Right-skewed
my.density(AVGGIFT, xlab = "AVGGIFT") # Right-skewed

## For the skewed variables, I will just log-transform them. Need to add a small number 
## if minimum value = 0. For Icavg (Normal with different variances), I will add a 
## squared term to the model. For the variables that are still right-skewed after 
## transformation, I will add a log term to the model.
NUMCHLD <- log(NUMCHLD) + 1 # Still right-skewed
INCOME <- log(INCOME) + 1
WEALTH <- log(-WEALTH + max(WEALTH) + 1) + 1 # Right-skewed
HV <- log(HV + 1) + 1 # Left-skewed
HV <- -HV + max(HV) + 1
Icmed <- log(Icmed + 1) + 1 # Left-skewed
Icmed <- -Icmed + max(Icmed) + 1
IC15 <- log(IC15 + 1) + 1
NUMPROM <- log(NUMPROM) + 1
RAMNTALL <- log(RAMNTALL) + 1
MAXRAMNT <- log(MAXRAMNT) + 1
LASTGIFT <- log(LASTGIFT + 1) + 1 
totalmonths <- log(-totalmonths + max(totalmonths) + 1) + 1
TIMELAG <- log(TIMELAG + 1) + 1
AVGGIFT <- log(AVGGIFT) + 1

##
## (d)
##

## Create single factor variable for zip code.
zipconvert <- rep(0, n_train)
zipconvert[zipconvert_2 == 1] <- 2
zipconvert[zipconvert_3 == 1] <- 3
zipconvert[zipconvert_4 == 1] <- 4
zipconvert[zipconvert_5 == 1] <- 5
table(zipconvert)

## Remove two observations with zipconvert = 0.
ii_remove <- (1:n_train)[zipconvert == 0]
dta_train <- dta_train[-ii_remove, ]
n_train <- nrow(dta_train)

TARGET_B <- TARGET_B[-ii_remove]
homeowner.dummy <- homeowner.dummy[-ii_remove]
NUMCHLD <- NUMCHLD[-ii_remove]
INCOME <- INCOME[-ii_remove]
gender.dummy <- gender.dummy[-ii_remove]
WEALTH <- WEALTH[-ii_remove]
HV <- HV[-ii_remove]
Icmed <- Icmed[-ii_remove]
Icavg <- Icavg[-ii_remove]
IC15 <- IC15[-ii_remove]
NUMPROM <- NUMPROM[-ii_remove]
RAMNTALL <- RAMNTALL[-ii_remove]
MAXRAMNT <- MAXRAMNT[-ii_remove]
LASTGIFT <- LASTGIFT[-ii_remove]
totalmonths <- totalmonths[-ii_remove]
TIMELAG <- TIMELAG[-ii_remove]
AVGGIFT <- AVGGIFT[-ii_remove]
zipconvert <- zipconvert[-ii_remove]

## Convert zip code variable to factor.
zipconvert <- factor(zipconvert)

fit_d <- glm(TARGET_B ~ homeowner.dummy + NUMCHLD + INCOME + gender.dummy + WEALTH + 
  HV + Icmed + Icavg + IC15 + NUMPROM + RAMNTALL + MAXRAMNT + LASTGIFT + totalmonths + 
  TIMELAG + AVGGIFT + zipconvert + zipconvert * WEALTH, family = binomial)

## Side-by-side boxplots of WEALTH by zipconvert and TARGET_B.
par(mfrow = c(1, 2))
boxplot(WEALTH[TARGET_B == 0] ~ zipconvert[TARGET_B == 0])
boxplot(WEALTH[TARGET_B == 1] ~ zipconvert[TARGET_B == 1])

## None of the interaction terms of statistically significant, so I am going to remove 
## them for simplicity in what follows.

##
## (e)
##

## Forward stepwise variable selection. Appropriate since we are interested in prediction.
m_null <- glm(TARGET_B ~ 1, family = binomial)
m_full <- glm(TARGET_B ~ homeowner.dummy + NUMCHLD + log(NUMCHLD) + INCOME + 
  gender.dummy + WEALTH + log(WEALTH) + HV + log(HV) + Icmed + log(Icmed) + Icavg + 
  I(Icavg ^ 2) + IC15 + NUMPROM + RAMNTALL + MAXRAMNT + LASTGIFT + totalmonths + 
  TIMELAG + AVGGIFT + zipconvert, family = binomial)

## Use 'step' to do forward stepwise regression and select variables.
step_f <- step(m_null, scope = list(lower = formula(m_null), upper = formula(m_full)), 
  direction = "forward")

## Best model according to AIC. Also corresponds to only leaving the variables with p-
## values < 0.05 in fit_d.
fit_e <- glm(TARGET_B ~ INCOME + RAMNTALL + MAXRAMNT + totalmonths, family = binomial)

##
## (f)
##

## Marginal model plots. The one for MAXRAMNT looks bad. Could be that we need a 
## transformation, or we might consider removing one or two leverage points.
par(mfrow = c(2, 2))
my.mmplot(model = fit_e, variable = INCOME, xlab = "INCOME")
my.mmplot(model = fit_e, variable = RAMNTALL, xlab = "RAMNTALL")
my.mmplot(model = fit_e, variable = MAXRAMNT, xlab = "MAXRAMNT") ## *
my.mmplot(model = fit_e, variable = totalmonths, xlab = "totalmonths")

## Densities of MAXRAMNT plausibly Normal, with different variances. Will try adding a 
## squared term.
my.density(MAXRAMNT, xlab = "MAXRAMNT") 

fit_e2 <- glm(TARGET_B ~ INCOME + RAMNTALL + MAXRAMNT + I(MAXRAMNT ^ 2) + totalmonths, 
  family = binomial)

## Adding MAXRAMNT ^ 2 seems to have done the trick.
par(mfrow = c(2, 2))
my.mmplot(model = fit_e2, variable = INCOME, xlab = "INCOME")
my.mmplot(model = fit_e2, variable = RAMNTALL, xlab = "RAMNTALL")
my.mmplot(model = fit_e2, variable = MAXRAMNT, xlab = "MAXRAMNT") 
my.mmplot(model = fit_e2, variable = totalmonths, xlab = "totalmonths")

##
## (g) I'll go with the model I found in part (e). We might consider interactions 
## between INCOME and the two AMNT variables.
##

####
#### PART 2
####

rm(TARGET_B)
rm(homeowner.dummy)
rm(NUMCHLD)
rm(INCOME)
rm(gender.dummy)
rm(WEALTH)
rm(HV)
rm(Icmed)
rm(Icavg)
rm(IC15)
rm(NUMPROM)
rm(RAMNTALL)
rm(MAXRAMNT)
rm(LASTGIFT)
rm(totalmonths)
rm(TIMELAG)
rm(AVGGIFT)
rm(zipconvert)

detach(dta_train)
attach(dta_test)

##
## (a)
##

INCOME <- log(INCOME) + 1
RAMNTALL <- log(RAMNTALL) + 1
MAXRAMNT <- log(MAXRAMNT) + 1
totalmonths <- log(-totalmonths + max(totalmonths) + 1) + 1

new_data <- data.frame(INCOME = INCOME, RAMNTALL = RAMNTALL, MAXRAMNT = MAXRAMNT, 
  totalmonths = totalmonths)
theta_hat <- predict(fit_e, newdata = new_data, type = "response")

## Predict Y = 1 if theta_hat > 0.5. 
predict_Y <- as.numeric(theta_hat > 0.5)

##
## (b) Overall classification accuracy = 0.56, not much better than flipping a coin.
##

table(TARGET_B, predict_Y)
mean(TARGET_B == predict_Y)


