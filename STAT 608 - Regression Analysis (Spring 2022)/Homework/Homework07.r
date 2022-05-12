################################ STAT 608 HW07 ################################
library("dplyr") # for using the between function
library("ggplot2") # for my cool plot on 2
#library("plyr") # for rounding to nearest 25 in part 3 (used for graph limits)

rm(list = ls())
########## Problem 2 ###########

# given
n = 50
B0 = 0.5
B1 = 1.5
x = 0.5
mean.resp = B0+B1*(x)
sigma_v2 = 0.5
p = seq(0.1, 0.9, 0.1)
M = 1000


Coverage = NULL
for(i in seq_along(p)){
  
  Mean_Resp_In_CI = NULL
  for(j in 1:M){
    
    set.seed(j)
    vt = rnorm(n = 50, mean = 0, sd=sqrt(sigma_v2))
    set.seed(j)
    X = runif(n=50, min = 0, max = 1)
    
    et = NULL
    et[1] = vt[1]
    for(k in 2:n){
      et[k] = p[i]*et[k-1]+vt[k]
    }
    
    y = B0+B1*X+et
    fit = lm(y~X)
    pred = predict(fit, newdata = data.frame(X = 0.5), interval = "confidence")
    Mean_Resp_In_CI[j] = between(mean.resp, pred[2], pred[3])
    
    if(j == M){
      Coverage[i] = mean(Mean_Resp_In_CI)
    }
    
  }
  
  if(i == length(p)){
    Cov.data = data.frame(P = p, Coverage = Coverage)
  }
}


ggplot(data = Cov.data, mapping = aes(P,Coverage)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0.25,0.95, by = 0.1), limits = c(0.25,1)) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0,1)) +
  geom_text(aes(label = paste0("(",P,","," ",Coverage,")")), y = Coverage+.05, size = 3.5) +
  labs(title = paste0("Coverage Probabilities of 95% Confidence Interval for the Mean Response \n",
                      "When Errors Follow an AR(1) Process"),
       subtitle = expression(atop("Model:"~ y[t] == beta[0] + beta[1]*x[t] + epsilon[t],
                                  "where"~ epsilon[t] == rho*epsilon[t-1]+nu[t])),
       x = expression("Autocorrelation Coefficent"~ (rho)),
       y = "True Coverage Probability") +
  theme(axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 4),
        plot.title = element_text(hjust = 0.5, vjust = 3.5),
        plot.subtitle = element_text(hjust = 0.5, vjust = 3),
        plot.margin = margin(rep(20,4)))

detach("package:ggplot2", unload = TRUE)
detach("package:dplyr", unload = TRUE)

########## Problem 2 ###########
##### Exercise 9.2 in book #####

# This problem is based on an exercise from Abraham and Ledolter (2006, pp. 335-337)
# which focuses on monthly sales from a bookstore in the city of Vienna, Austria. The
# available data consisted of 93 consecutive monthly observations on the following variables

# Sales = Sales (in hundreds of dollars) 
# Advert = Advertising spend in the current month 
# Lag1Advert = Advertising spend in the previous month 
# Time = Time in months 
# Month_ i = Dummy variable which is 1 for month i and 0 otherwise  ( i = 2, 3, ., 12) 
# The data can be found on the book website in the file bookstore.txt.


bookstore.dta = read.delim("C:/Users/jackr/OneDrive/Desktop/Graduate School Courses/STAT 608 - Regression Analysis/Data/bookstore.txt")
View(bookstore.dta)


          
# (a) Follow the advice of Abraham and Ledolter (2006, pp. 336-337) and first 
#     build a model for Sales ignoring the effects due to Advert and Lag1Advert. 
#     Ensure that you produce diagnostic plots to justify your choice of model. 
#     Describe any weaknesses in your model.
Sales = bookstore.dta[,1]



Sales_1 = Sales[-1]
Sales_lag1 = Sales[1:92]

#################################### EDA Stuff #################################################################
###### lag plot
plot(Sales_lag1, Sales_1, 
     xlim = c(round_any(min(Sales),25, f = floor), round_any(max(Sales),200, f = ceiling)),
     ylim = c(round_any(min(Sales),25, f = floor), round_any(max(Sales),200, f = ceiling)))
# identify(x = Sales_lag1,y = Sales_1)

Sales_Lag_Model = lm(Sales_1~Sales_lag1)
Sales_Lag_Model
abline(Sales_Lag_Model)
cor(Sales_lag1,Sales_1)

# this plots sort of interesting.  When Sales_Lag1 is small, there seems to be a strong positive correlation
# between Sales_Lag1 and Sales.  However, when Sales lag1 is very large, there seems to be an opposite relationship.
# large value of sales in the previous period seem to be negatively correlated with the value of sales in the next 
# period.  However, the correlation and the linear model seem to indicate a positive relationship between Sales and 
# SalesLag1.  


##### acf plot of Sales
acf(Sales)
# looking at the acf plot for Sales, there seems to be significant auto-corrleation between Sales and Sales_Lag12
# interesting, because later on, when we fit the model, we do not see this significant auto-correlation appear in 
# the residuals.


#######################################################################################################################

##### Fitting Model, not including advert or lagadvert
book.mod1 = bookstore.dta[,-c(1,2,3)]
mod.1 = lm(Sales ~.,data = book.mod1)



##### plots of std residuals vs all our EVs in mod1 and the fitted values
#### NOTE: I collapsed the 11 indicator variables for each month into a factor vector "month" that indicates the 
#### month of each observation
month = c(seq(2,12),rep(seq(1,12),length.out = 82))
month = factor(month)

ee <- rstandard(mod.1)
y_hat <- fitted(mod.1)



par(mfrow = c(3,1))
plot.default(month, ee, type = "p")
plot(book.mod1$Time, ee, xlab = expression(Time[t]), type = "b",
     ylab = "Standardized Residuals",
     main = "Time Series Plot of Standardized Residuals")
plot(y_hat, ee, xlab = "Fitted Values", ylab = "Standardized Residuals")
par(mfrow = c(1,1))

# looking at the first plot, there doesn't seem to be a relationship between the residuals and the month
# I.e. the variance seems constant across the months (there do seem to be a few outliers)

# looking at the second plot, the time series plot of the standardized residuals, it is not clear if there is 
# a pattern (i.e. no clear autocorrelation)

# looking at the 3rd plot, the constant variance assumption doesn't seem to be violated


plot(book.mod1$Month_2, ee)
plot(book.mod1$Month_3,ee)
plot(book.mod1$Month_4,ee)
plot(book.mod1$Month_5,ee)
plot(book.mod1$Month_6,ee)
plot(book.mod1$Month_7,ee)
plot(book.mod1$Month_8,ee)
plot(book.mod1$Month_9,ee)
plot(book.mod1$Month_10,ee)
plot(book.mod1$Month_11,ee)
plot(book.mod1$Month_12,ee)


##### looking at acf plot of residuals and standardized residuals


ee <- rstandard(mod.1)
a = acf(ee, plot = FALSE)
plot(a, main = "ACF plot of Standardized Residuals")



# the acf for the residuals and standardized residuals are essentially the same

a$acf[2]
# we see from the acf plot that the lag1 autocorrelation exceeds the normal cutoff value of -2/sqrt(n), 
# however, it does not exceed it by much
a$acf[2] 
2/sqrt(93)
# the lag1 autocorrelation is -0.2247081 is barely larger (in abs value) that the cutoff value of -0.2073903

shapiro.test(ee)
cor.test(ee[2:93],ee[1:92], method = "spearman")
SpearmanRho(ee[2:93],ee[1:92], conf.level = 0.95)


# the variance seems constant with one outlier (obs 33)
par(mfrow=c(2,2))
plot(mod.1)
par(mfrow = c(1,1))

# the standardized residuals seem to violate normality, variance seems constant.  We seem to have a few influential
# points, 11,32,33,59,89

highleverage = 2*(length(coef(mod.1)))/nrow(bookstore.dta)
highleverage
hatvalues(mod.1)

highinfl = 4/(nrow(bookstore.dta)-length(coef(mod.1)))
highinfl
which(cooks.distance(mod.1)>highinfl)


SalesLag1 = c(NA,Sales[1:92])
cbind(Sales,SalesLag1)[which(cooks.distance(mod.1)>highinfl),]

###########################################################################################################

# (b) Add the effects due to Advert and Lag1Advert to the model you have 
#     developed in (a). Last month's advertising (Lag1Advert) is thought to have 
#     an impact on the current month's sales. Obtain a final model for predicting 
#     Sales. Ensure that you produce diagnostic plots to justify your choice of 
#     model. Describe any weaknesses in your model. 

plot(bookstore.dta$Lag1Advert, Sales)
plot(bookstore.dta$Lag1Advert, bookstore.dta$Advert)

book.mod2 = bookstore.dta[,-1]
mod.2 = lm(Sales ~., data = book.mod2)
b = acf(mod.2$residuals, main = "ACF plot of Std. Residuals for Model 2")

ee <- rstandard(mod.2)
y_hat <- fitted(mod.2)

plot(book.mod2$Time, ee, xlab = expression(Time[t]), ylab = "Standardized Residuals", type = "b",
     main = "TS Plot of Std. Residuals for Model 2")
# there seems to be negative correlation: high values followed by low values and vice versa
plot(y_hat, ee, xlab = "Fitted Values", ylab = "Standardized Residuals")
# the variance seems constant with one outlier (obs 33)
par(mfrow=c(2,2))
plot(mod.2)
par(mfrow = c(1,1))



anova(mod.1,mod.2)



