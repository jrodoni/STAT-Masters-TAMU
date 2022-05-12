########## STAT 608 HW03 ###############


### Problem 6
x  = c(-3,-2,-1,0,6)
y = c(10,6,5,3,12)

y_hat = 7.2+0.5*x
e_i = y-y_hat

e_i

n = length(x)

leverages = (1/n)+(x-mean(x))^2/(sum((x-mean(x))^2))
leverages



########### Problem 8
company = read.csv("C:/Users/jackr/OneDrive/Desktop/Graduate School Courses/STAT 608 - Regression Analysis/Data/company.csv")
View(company)

n = length(company$Company)

######(a) Begin by creating a scatterplot of Sales and Assets and fit a simple linear regression
###### line. What transformations does your scatterplot suggest? Create diagnostic plots for
###### this model (Model 1). Discuss any weaknesses of this model.

attach(company)
lm1 = lm(Assets~Sales)

# Scatter plot
plot(x = Sales, y = Assets)
abline(lm1)
#

# Other diagnostic Plots
par(mfrow=c(2,2))
plot(lm1)
par(mfrow = c(1,1))

## better than the fourth plot shown above
cd<-cooks.distance(lm1)
par(mar=c(5,5,3,3), cex.axis=1.5,cex.lab=1.5,cex.main=3,pch=19, mfrow=c(1,1))
plot(Sales,cd, xlab="Sales", ylab="Cook's Distance")
abline(h=(4/(n-2)), lty=2)
#identify(Sales, cd)

# We see points 16,40,43,48 & 54 are influential points.  

dev.off() # resets plotting parameters


####### (b) Box Cox transformation on X

shapiro.test(Sales)

library(car)
summary(powerTransform(Sales))
shapiro.test(log(Sales))


####### (C)
summary(powerTransform(Assets))
lm2 = lm(Assets~log(Sales))
summary(powerTransform(lm2))

shapiro.test(log(Assets))



####### (D)

model2 = lm(log(Assets)~log(Sales))
plot(log(Sales), log(Assets))
abline(model2)

par(mar=c(4,5,2,1),  mfrow=c(2,2), cex.axis=1.5,cex.lab=1.5,cex.main=3,pch=19)
plot(model2)


cd<-cooks.distance(model2)
par(mar=c(5,5,3,3), cex.axis=1.5,cex.lab=1.5,cex.main=3,pch=19, mfrow=c(1,1))
plot(log(Sales),cd, xlab="Sales", ylab="Cook's Distance")
abline(h=(4/(n-2)), lty=2)
#identify(log(Sales),cd)
dev.off()




######## (G)

CI_95 = predict(model2, newdata = data.frame(Sales = 6571), interval = "confidence")
CI_95

modelsum = summary(model2)
MSE = (1/(n-2))*sum(modelsum$residuals^2)
MSE = modelsum$sigma^2 # another way of calculating MSE

c(exp(CI_95[2]+ MSE/2),exp(CI_95[3]+MSE/2))

c(exp(CI_95[2]+ modelsum$sigma^2/2),exp(CI_95[3]+modelsum$sigma^2/2))


modelsum$sigma


