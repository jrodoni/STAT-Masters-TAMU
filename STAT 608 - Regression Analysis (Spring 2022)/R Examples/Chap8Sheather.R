#March 17, 2009

#Please change the file path in the command below to coincide with where you have stored the data files
setwd("/Users/home/Documents/TAMU/stat608/Data/")

MichelinFood <- read.table("MichelinFood.txt", header=TRUE)
attach(MichelinFood)

#Figure 8.1 on page 266
par(cex=1.2, pch=19, mar=c(5,5,1,1), mfrow=c(1,1))
plot(Food,proportion,ylab="Sample proportion",xlab="Zagat Food Rating")

#R output on page 267
m1 <- glm(cbind(InMichelin,NotInMichelin)~Food,family=binomial)
summary(m1)

#Figure 8.2 on page 268
x <- seq(15,28,0.05)
y <- 1/(1+exp(-1*(m1$coeff[1] + m1$coeff[2]*x)))
plot(Food,proportion,ylab="Probability of inclusion in the Michelin Guide",xlab="Zagat Food Rating")
lines(x,y)

#Table 8.2 on page 269
thetahat <- m1$fitted.values
odds_ratio <- m1$fitted.values/(1-m1$fitted.values)
cbind(Food,round(thetahat,3),round(odds_ratio,3))

#p-value on page 272
pchisq(m1$deviance,m1$df.residual,lower=FALSE)

#Value of the difference in devinace and associated p-value on page 273
m1$null.deviance-m1$deviance
pchisq(m1$null.deviance-m1$deviance,1,lower=FALSE)

#Logistic regression output on page 274
print(paste("Pearson's X^2 =",round(sum(residuals(m1,type="pearson")^2),3)))

#Table 8.3 on page 276
cbind(round(residuals(m1,"response"),3),round(residuals(m1,"pearson"),3),round(residuals(m1,"deviance"),3))

#Figure 8.3 on page 276
hvalues <- influence(m1)$hat
stanresDeviance <- residuals(m1)/sqrt(1-hvalues)
stanresPearson <- residuals(m1,"pearson")/sqrt(1-hvalues)
par(mfrow=c(1,2))
plot(Food,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Food Rating",ylim=c(-2,2))
plot(Food,stanresPearson,ylab="Standardized Pearson Residuals",xlab="Food Rating",ylim=c(-2,2))

detach(MichelinFood)


MichelinNY <- read.csv("MichelinNY.csv", header=TRUE)
attach(MichelinNY)

y <- InMichelin

#Figure 8.4 on page 278
par(mfrow=c(1,1))
plot(jitter(Food,amount=.15),jitter(y,amount=0.03),xlab="Food Rating",
ylab="In Michelin Guide? (0=No, 1=Yes)")

#Figure 8.5 on page 279
boxplot(Food~y, ylab="Food Rating",xlab="In Michelin Guide? (0=No, 1=Yes)")

#Logistic regression output on page 279
m1 <- glm(y~Food,family=binomial(),data=MichelinNY)
summary(m1)

#Figure 8.6 on page 281
hvalues <- influence(m1)$hat
stanresDeviance <- residuals(m1)/sqrt(1-hvalues)
#Alternatively we could use 
#stanresDeviance < rstandard(m1)
stanresPearson <- residuals(m1,"pearson")/sqrt(1-hvalues)
par(mfrow=c(1,2))
plot(Food,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Food Rating",ylim=c(-4.1,4.1))
plot(Food,stanresPearson,ylab="Standardized Pearson Residuals",xlab="Food Rating",ylim=c(-4.1,4.1))

#Figure 8.7 on page 282
par(mfrow=c(1,1))
xx <- seq(15,28.2,0.05)
yy <- 1/(1+exp(-1*(m1$coeff[1] + m1$coeff[2]*xx)))
loessfit1 <- loess(y ~ Food,degree=1,span=2/3)
plot(jitter(Food,amount=.15),jitter(y,amount=0.03),xlab="Food Rating",
ylab="In Michelin Guide? (0=No, 1=Yes)")
lines(xx,yy)
lines(xx,predict(loessfit1,data.frame(Food=xx)),lty=2)

#Figure 8.8 on page 286
par(mfrow=c(2,2))
boxplot(Food~y, ylab="Food Rating",xlab="In Michelin Guide? (0=No, 1=Yes)")
boxplot(Decor~y, ylab="Decor Rating",xlab="In Michelin Guide? (0=No, 1=Yes)")
boxplot(Service~y, ylab="Service Rating",xlab="In Michelin Guide? (0=No, 1=Yes)")
boxplot(Price~y, ylab="Price",xlab="In Michelin Guide? (0=No, 1=Yes)")


#Check for unequal variances for food:
par(mfrow=c(1,1))
plot(density(Food[y==0],bw="SJ",kern="gaussian"),type="l",
     main="Gaussian kernel density estimate",xlab="Food")
rug(Food[y==0])
lines(density(Food[y==1],bw="SJ",kern="gaussian"),lty=2)
rug(Food[y==1])


#Figure 8.9 on page 288 (Also added squared food term)
m2 <- glm(y~Food+Decor+Service+Price+log(Price)+I(Food^2),family=binomial(),data=MichelinNY)
loessfit1 <- loess(y ~ Food,degree=1,span=2/3)
loessfit2 <- loess(m2$fitted.values ~ Food,degree=1,span=2/3)
xx <- seq(15,28.2,0.05)
summary(m2)
par(mfrow=c(1,2))
plot(Food,y,xlab="Food Rating, x1", ylab="Y, In Michelin Guide? (0=No, 1=Yes)")
lines(xx,predict(loessfit1,data.frame(Food=xx)))
#lines(lowess(Food,y,iter=1,f=2/3))
plot(Food,m2$fitted.values,ylab=expression(hat(Y)),xlab="Food Rating, x1")
lines(xx,predict(loessfit2,data.frame(Food=xx)))

#Figure 8.10 on page 288
library(alr3)
mmps(m2,layout=c(2,3))

#Welp.  That doesn't work.  Let's create our own mmplot function.
#This only makes one at a time.  Run for variable = each predictor.
my.mmplot <- function(model, variable, xlabel="Predictor", by.var=0.01){
  y <- model$data[,1]
  loessfit1 <- loess(y ~ variable, degree=1, span=2/3)
  loessfit2 <- loess(model$fitted.values ~ variable, degree=1, span=2/3)
  xx <- seq(min(variable), max(variable), by.var)
  plot(variable, y, xlab=xlabel)
  lines(xx,predict(loessfit1,data.frame(variable=xx)), col="blue", lwd=2)
  lines(xx,predict(loessfit2,data.frame(variable=xx)), lty=2, col="red", lwd=2)
}

#Check that this works:
my.mmplot(model=m2, variable=Food)

#Now run it on all five predictors:
par(mfrow=c(2,3))
my.mmplot(model=m2, variable=Food, xlabel="Food")
my.mmplot(model=m2, variable=Decor, xlabel="Decor")
my.mmplot(model=m2, variable=Service, xlabel="Service")
my.mmplot(model=m2, variable=Price, xlabel="Price")
my.mmplot(model=m2, variable=log(Price), xlabel="log(Price)")

#Figure 8.11 on page 289
par(mfrow=c(1,1))
plot(Decor,Service,pch=y+1,col=y+1,xlab="Decor Rating",ylab="Service Rating")
abline(lsfit(Decor[y==0],Service[y==0]),lty=1,col=1)
abline(lsfit(Decor[y==1],Service[y==1]),lty=2,col=2)
legend(14, 28,legend=c("No","Yes"),pch=1:2,col=1:2,title="In Michelin Guide?")

#Making our own marginal model plots:
m3 <- glm(y~Food+Decor+Service+Price+log(Price)+Service:Decor,family=binomial(),data=MichelinNY)
par(mfrow=c(2,3))
my.mmplot(model=m3, variable=Food, xlabel="Food")
my.mmplot(model=m3, variable=Decor, xlabel="Decor")
my.mmplot(model=m3, variable=Service, xlabel="Service")
my.mmplot(model=m3, variable=Price, xlabel="Price")
my.mmplot(model=m3, variable=log(Price), xlabel="log(Price)")

#Output from R on page 290
anova(m2,m3,test="Chisq")

#Figure 8.13 on page 291
par(mfrow=c(1,1))
hvalues <- influence(m3)$hat
stanresDeviance <- residuals(m3)/sqrt(1-hvalues)
plot(hvalues,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Leverage Values",ylim=c(-3,3),xlim=c(-0.05,0.7))
abline(v=2*7/length(y),lty=2)
identify(hvalues,stanresDeviance,labels=Restaurant.Name,cex=0.75)

#Output from R on pages 291 and 292
summary(m3)

#Output from R on pages 292 and 293
m4 <- glm(y~Food+Decor+Service+log(Price)+Service:Decor,family=binomial(),data=MichelinNY)
anova(m4,m3,test="Chisq")
summary(m4)

#Figure 8.14 on page 294
mmps(m4,layout=c(2,3),key=NULL)

#Figure 8.15 on page 295
par(mfrow=c(1,1))
hvalues <- influence(m4)$hat
stanresDeviance <- residuals(m4)/sqrt(1-hvalues)
plot(hvalues,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Leverage Values",ylim=c(-3,3),xlim=c(-0.05,0.35))
abline(v=2*6/length(y),lty=2)
identify(hvalues,stanresDeviance,labels=Restaurant.Name,cex=0.75)

#Table 8.5 on page 295
fits4 <- m4$fitted.values
round(fits4[c(14,37,69,133,135,138,160)],3)

detach(MichelinNY)

