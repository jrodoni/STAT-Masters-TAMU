salary<-read.csv("profsalary.csv",header=TRUE)


y1<-salary$Salary
x1<-salary$Experience

par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(9,5,1,3), pch=19)
plot(x1, y1, xlab="Years Experience", ylab="Salary")

m1<-lm(y1~x1)

par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(9,5,1,3), pch=19)
plot(x1, rstandard(m1), xlab="Years Experience", ylab="Standardized Residuals")


#We may need to use the I() function when fitting polynomial terms:
m2<-lm(y1~x1+I(x1^2))
par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(9,5,1,3), pch=19)
plot(x1, rstandard(m2), xlab="Years Experience", ylab="Standardized Residuals")

par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(9,5,1,3), pch=19)
plot(x1, hatvalues(m2), xlab="Years Experience", ylab="Leverage")
abline(h=(6/length(x1)), lty=2)


par(cex.main=1.5, cex.axis = 1.5, cex.lab=1.5, mar=c(10,5,2,3), pch=19, mfrow=c(2,2))
plot(m2)




x2 <- x1 - mean(x1)
m3 <- lm(y1~x2+I(x2^2))
par(cex.main=2, cex.axis = 2, cex.lab=2, mar=c(9,5,1,3), pch=19)
plot(x2, hatvalues(m3), xlab="Years Experience", ylab="Leverage")
abline(h=(6/length(x2)), lty=2)
