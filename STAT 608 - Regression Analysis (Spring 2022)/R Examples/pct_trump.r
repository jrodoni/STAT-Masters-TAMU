####
#### State percent votes for Trump in 2016 and 2020 elections.
####

## Load data.
dta <- read.csv(paste0("C:/Users/jackr/OneDrive/Desktop/Graduate School Courses",
                       "/STAT 608 - Regression Analysis/Data/pct_trump.csv"))

##
## Exploratory data analysis.
##

## Scatterplot. DC is an outlier.
with(dta, plot(Trump_2016, Trump_2020))
fit <- with(dta, lm(Trump_2020 ~ Trump_2016))
abline(fit)

## Remove DC.
dta <- dta[dta$ï..State != "DC", ]
n <- 50

## Remake scatterplot
with(dta, plot(Trump_2016, Trump_2020, xlab = "% Trump 2016", ylab = "% Trump 2020"))
fit <- with(dta, lm(Trump_2020 ~ Trump_2016))
abline(fit)

summary(fit)

## Correlation
with(dta, cor(Trump_2016, Trump_2020))

##
## Inference about slope.
##

## CI.
0.8860 + c(-1, 1) * qt(0.975, 48) * 0.0718

## Hypothesis test for H_0: beta_1 = 0.
tt <- (0.8860 - 0) / 0.0718
pp <- 2 * (1 - pt(tt, 48))

## Hypothesis test for H_0: beta_1 = 1.
tt <- (0.8860 - 1) / 0.0718
pp <- 2 * (1 - pt(abs(tt), 48))

##
## CI and PI.
##

## CI for states voting 50% Trump in 2016. 
predict(fit, newdata = data.frame(Trump_2016 = 0.5), interval = "confidence")
with(dta, 0.4985 + c(-1, 1) * qt(0.975, 48) * 0.0514 * 
  sqrt(1 / n + (0.50 - mean(Trump_2016)) ^ 2 / sum((Trump_2016 - mean(Trump_2016)) ^ 2)))

## PI for states voting 50% Trump in 2016.
predict(fit, newdata = data.frame(Trump_2016 = 0.5), interval = "prediction")
with(dta, 0.4985 + c(-1, 1) * qt(0.975, 48) * 0.0514 * 
  sqrt(1 + 1 / n + (0.50 - mean(Trump_2016)) ^ 2 / 
  sum((Trump_2016 - mean(Trump_2016)) ^ 2)))


