#library(dvmisc)

clean<-read.csv("clean.csv",header=TRUE)

attach(clean)

Crews_0 <- clean$Crews

m1<-lm(Rooms ~ Crews)
par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(5,5,1,1), mfrow=c(1,1))
plot(Crews, Rooms, pch=19, xlab="Crews", ylab="Rooms")
abline(m1)

#Just to show that the simple linear model is terrible:
par(mar=c(5,5,2,1), cex.axis=1.5,cex.lab=1.5,cex.main=3,pch=19, mfrow=c(2,2))
plot(m1)

#Let's create a better model:
x2<-sqrt(Crews)
y2<-sqrt(Rooms)
m2<-lm(y2~x2)

par(mar=c(5,5,2,1), cex.axis=1.5,cex.lab=1.5,cex.main=3,pch=19, mfrow=c(2,2))
plot(m2)



#Now make prediction intervals for the regression line.
x2=c(2,4)  #We transformed both x and y; if we want to predict for # of crews = 4 and 16, we use the transformed (square roots) 2 and 4.
dt<-data.frame(x2)
pi<-predict(m2,newdata=dt,interval="prediction")
pi  #Remember these predictions are still in square root of # of rooms cleaned; square them to get back into original units (counts).






#Now make confidence intervals for the regression line.
x2=seq(1, 4, by=0.1)
dt<-data.frame(x2)
ci<-predict(m2,newdata=dt,interval="confidence")

par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(5,5,1,1), mfrow=c(1,1))
plot(Crews, Rooms, pch=19, xlab="Crews", ylab="Rooms")
lines((x2^2), (ci[,2]^2))  #Transformed, but Uncorrected
lines((x2^2), (ci[,3]^2))

#compare to confidence intervals based on original (untransformed) model:
Crews = seq(2, 16, by=0.2)
dt <- data.frame(Crews)
ci.untransformed<- predict(m1, newdata=dt, interval="confidence")
lines(Crews, (ci.untransformed[,2]), col="red", lty=2)  #Not transformed (assumes constant variance)
lines(Crews, (ci.untransformed[,3]), col="red", lty=2)



#Back-transform the end points properly, adding in the correction factors!!
anova(m2)   #To find Residual Mean Square (or MSE)
clb<-ci[,2]^2 + 0.353
cub<-ci[,3]^2 + 0.353

par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(5,5,1,1), mfrow=c(1,1))
plot(Crews_0, Rooms, pch=19, xlab="Crews", ylab="Rooms")
lines((x2^2), (ci[,2]^2))  #Transformed, but Uncorrected
lines((x2^2), (ci[,3]^2))
lines((x2^2), clb, col="red", lty=2) #Corrected
lines((x2^2), cub, col="red", lty=2)




#If we're interested in #Crews = 4, x2 = sqrt(4) = 2, the 11th element:
#CI = (14.63, 18.22)



#Chapter 4:
#Weighted Least Squares Regression:

#First, try weights inversely proportional to number of crews:
w.lm1 <- lm(Rooms ~ Crews, weights= 1/Crews)
par(mar=c(5,5,1,1), cex.axis=1.5,cex.lab=1.5,cex.main=3,pch=19, mfrow=c(2,2))
plot(w.lm1)

#Next, try weights calculated based on data:
cleanwt<-read.csv("cleanwt.csv",header=TRUE)
#I'm sorting the data set by the number of crews, from 2 to 16.
sorted <- cleanwt[order(cleanwt$Crews),]
w.Crews <- sorted$Crews
w.lm2<- lm(sorted$Rooms ~ w.Crews, weights = (1/sorted$StdDev^2), x=TRUE)

par(mar=c(5,5,1,1), cex.axis=1.5,cex.lab=1.5,cex.main=3,pch=19, mfrow=c(2,2))
plot(w.lm2)


#Visually compare the three models:
m1<-lm(Rooms ~ Crews)
par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(5,5,1,1), mfrow=c(1,1))
plot(Crews, Rooms, pch=19, xlab="Crews", ylab="Rooms")
abline(m1, lwd=2)
abline(w.lm1, lty=2, lwd=2, col="blue")
abline(w.lm2, lty=3, lwd=2, col="red")

#Let's look at the prediction intervals as well (adding to the above plot):
my.Crews <- seq(2, 16, by=0.01)
new <- data.frame(Crews=my.Crews)

fitted.1 <- predict(m1, newdata=new, interval="prediction")

fitted.w1 <- predict(w.lm1, newdata=new, interval="prediction")  #REALLY.  #See 4.1.1 p.118 in text.
incorrect.se <- (fitted.w1[,3] - fitted.w1[,1])/qnorm(0.975)
correct.se <- sqrt(incorrect.se^2 - anova(w.lm1)[2,3] + anova(w.lm1)[2,3]*my.Crews)
w1.lb <- fitted.w1[,1] - qnorm(0.975)*correct.se
w1.ub <- fitted.w1[,1] + qnorm(0.975)*correct.se

fitted.w2 <- predict(w.lm2, interval="prediction")  #But it's okay if we only make intervals on given data.

lines(my.Crews, fitted.1[,2], lty=1)
lines(my.Crews, fitted.1[,3], lty=1)   #Upper and lower bounds on SLR
lines(my.Crews, w1.lb, lty=2, col="blue")
lines(my.Crews, w1.ub, lty=2, col="blue")   #Upper and lower bounds on WLS, var prop. to x
lines(w.Crews, fitted.w2[,2], lty=3, col="red")
lines(w.Crews, fitted.w2[,3], lty=3, col="red")   #Upper and lower bounds on WLS, var calculated



#Just for kicks, showing off the method of creating the "New" variables; gives same result as above:
#Multiply by sqrt(wi):
ynew <- sorted$Rooms/sorted$StdDev
x2new <- sorted$Crews/sorted$StdDev
x1new <- w.lm2$x[,1]/sorted$StdDev  #The first column of the design matrix: a 1-vector multiplied element-wise by the square root of the weights.

w.lm3 <- lm(ynew ~ x1new + x2new - 1, x=TRUE)  #Be sure not to fit the intercept!  We have a transformed model.
w.lm3$x


#Just use matrix multiplication from scratch:
W <- diag(dim(w.lm2$x)[1])*(1/sorted$StdDev^2)  #The diag function creates a diagonal matrix.
X <- w.lm2$x
beta.hat <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% sorted$Rooms
