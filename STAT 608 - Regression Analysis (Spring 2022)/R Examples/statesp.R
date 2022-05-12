state<-read.csv("statesp.csv", header=TRUE)

attach(state)

par(pch=19, cex=1.3, mar=c(4,4,1,1))
plot(state)


library(car)
#Let's try Box-Cox.
#I do indeed need this family = "yjPower" option because there exist states with 0% in
#metropolitan areas.
#West is an indicator variable; doesn't need a power transformation.
X<-cbind(MET, ECAB, YOUNG, OLD)  #Stacks these four predictors into a single matrix.
tranx<-powerTransform(X, family="yjPower") #Searches for multivariate normality.
summary(tranx)
testTransform(tranx, lambda=c(1, 1, 1, 1))

lm.1<-lm(EX ~ MET + ECAB + YOUNG + OLD + WEST)
tranmod <- powerTransform(lm.1, family="yjPower")
summary(tranmod)

library(alr3)
inverseResponsePlot(lm.1,key=TRUE)

y2<-EX^(1/4)
lm2<- lm(y2 ~ MET + ECAB + YOUNG + OLD + WEST)

par(cex.axis=1.5,cex.lab=1.5, mar=c(5.1,5.1,2,2),lwd=2, pch=19, mfcol=c(2,2))
plot(lm2)

par(cex.axis=1.5,cex.lab=1.5, mar=c(5.1,5.1,2,2),lwd=2, pch=19, mfcol=c(2,2))
plot(MET, lm2$residuals)
plot(ECAB, lm2$residuals)
plot(YOUNG, lm2$residuals)
plot(OLD, lm2$residuals)

cd2 <- cooks.distance(lm2)
par(cex.axis=1.5,cex.lab=1.5, mar=c(5.1,5.1,2,2),lwd=2, pch=19, mfcol=c(1,1))
plot(lm2$fitted,cd2,xlab="Fitted Values", ylab="Cook's Distance")
abline(h=4/48,lty=2)
identify(lm2$fitted, cd2, STATE)

##
## PCA. Don't include the indicator variable WEST.
##

X <- cbind(MET, ECAB, YOUNG, OLD)
X_std <- scale(X, center = TRUE, scale = TRUE)
S <- var(X_std)

eigen_out <- eigen(S)
eigen_vecs <- eigen_out$vectors
eigen_vals <- eigen_out$values

## PC loadings. The first PC is a contrast between MET, ECAB, and OLD on the
## one hand and YOUNG on the other. The second PC is a contrast mostly between
## MET and OLD. PC 1 will highlight cities with high values for MET, ECAB, and
## OLD and low values for YOUNG (or vice versa). PC 2 will highlight cities
## with high values of MET and low values of YOUNG (or vice versa).
eigen_vecs

## Proportion of variance explained by each PC.
eigen_vals / sum(eigen_vals)

## Compute the first two PCs.
PC_1 <- X_std %*% eigen_vecs[, 1]
PC_2 <- X_std %*% eigen_vecs[, 2]

## Scatterplot. No obvious "structure."
plot(PC_1, PC_2)

## Extreme values of PC 1 and 2.
PC_1_min <- which.min(PC_1)
PC_1_max <- which.max(PC_1)
PC_2_min <- which.min(PC_2)
PC_2_max <- which.max(PC_2)

X_std[PC_1_min, ]
X_std[PC_1_max, ]
X_std[PC_2_min, ]
X_std[PC_2_max, ]
