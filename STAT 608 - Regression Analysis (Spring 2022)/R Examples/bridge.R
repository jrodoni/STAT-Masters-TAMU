bridge<-read.csv("bridge.csv", header=TRUE)
n <- nrow(bridge)

attach(bridge)

#Step 1:  Explore the data.
plot(bridge)

#Looks like everything is skewed.
hist(Time)
hist(DArea)
hist(CCost)
hist(Dwgs)
hist(Length)
hist(Spans)

#Notice Spans is discrete, with not too many distinct values:
table(Spans)



#Step 2:  Transform it all.  First the X's:
X<-cbind(DArea,CCost,Dwgs,Length,Spans)
library(car)
tranx<-powerTransform(X)
summary(tranx)

hist(Length)
hist(log(Length))

#Then the response variable:

lm1<-lm(Time ~ log(DArea) + log(CCost)+log(Dwgs)+log(Length)+log(Spans))
trany<-powerTransform(lm1)
summary(trany)

#Step 3:  Fit our transformed model, and see whether our model is valid:
lm.logs<-lm(log(Time) ~ log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans))
pairs(log(Time) ~ log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans))

par(cex.axis=2,cex.lab=2, mar=c(5.1,5.1,2,2),lwd=2, pch=19, mfrow=c(2,2))
plot(lm.logs)
add_conservative_cooks_line <- function(model){
  dc <- 4/model$df.residual
  h <- seq(0.01, max(hatvalues(model))*1.1, by = 0.01)
  r <- sqrt(length(model$coefficients) * dc * (1-h)/h)
  lines(h, r, col ='purple', lty='dotdash')
  lines(h, -r, col ='purple', lty='dotdash')
  #mtext(side = 4,las=1,at = c(max(hatvalues(model)), max(r))+1, cex=0.7, text = bquote(.(dc))) 
  #mtext(side = 4,las=1,at = c(max(hatvalues(model)), max(r))-1, cex=0.7, text = bquote(.(dc)))
}
add_conservative_cooks_line(lm.logs)


par(cex.axis=2,cex.lab=2, mar=c(5.1,5.1,2,2),lwd=2, pch=19, mfrow=c(3,2))
plot(lm.logs$fitted, lm.logs$residuals)
plot(log(DArea), lm.logs$residuals)
plot(log(CCost), lm.logs$residuals)
plot(log(Dwgs), lm.logs$residuals)
plot(log(Length), lm.logs$residuals)
plot(log(Spans), lm.logs$residuals)

par(cex.axis=2,cex.lab=1.5, mar=c(5.2,5.2,2,2),lwd=2, pch=19, mfcol=c(3,2))
library(alr3)
mmps(lm.logs)

#Looks good; let's check out the regression output:
summary(lm.logs)
#Notice log(DArea) and log(Length) have the wrong sign!

#Look at the correlation between the predictor variables:
X<-cbind(log(DArea), log(CCost), log(Dwgs), log(Length), log(Spans))
cor(X)
#Remember this correlation matrix won't pick up on everything, e.g. x3 + x2 = x1 + x5.

#Added variable plots:
library(car)
par(cex.axis=1.5,cex.lab=1.5, mar=c(6,7,4,4),lwd=2, pch=19, mfcol=c(3,2))
avPlots(lm.logs)

#Variance Inflation Factors:
vif(lm.logs)
#Several larger than 5.  Use methods in Chapter 7.

##
## PCA. To conduct PCA, we perform an eigen decomposition using the sample
## covariance matrix. We might alternatively do PCA on the log-transformed
## versions of the variables.
##

X_raw <- cbind(DArea, CCost, Dwgs, Length, Spans)
X_raw_std <- scale(X_raw, center = TRUE, scale = TRUE)
S <- var(X_raw_std)

eigen_out <- eigen(S)
eigen_vecs <- eigen_out$vectors
eigen_vals <- eigen_out$values

## The first PC is an 'index' variable, an average of all the variables. The
## second PC (which doesn't explain much variance) is a contrast, mostly
## between CCost and Dwgs on the one hand and Length and Spans on the other.
eigen_vecs
eigen_vals

## The proportion of variance explained by each PC. Really just the first PC is
## necessary apparently to summarize the X variables.
eigen_vals / sum(eigen_vals)

## Compute PCs.
PC_1 <- X_raw_std %*% eigen_vecs[, 1]
PC_2 <- X_raw_std %*% eigen_vecs[, 2]
var(PC_1)
var(PC_2)

## Inspect extreme values of PC 1 and PC 2.
PC_1_min <- which.min(PC_1)
PC_1_max <- which.max(PC_1)
PC_2_min <- which.min(PC_2)
PC_2_max <- which.max(PC_2)

X_raw[PC_1_min, ]
X_raw[PC_1_max, ]
X_raw[PC_2_min, ]
X_raw[PC_2_max, ]

X_raw_std[PC_1_min, ]
X_raw_std[PC_1_max, ]
X_raw_std[PC_2_min, ]
X_raw_std[PC_2_max, ]

##
## Cluster analysis.
##

## Hierarchical clustering with complete linkage. You apply the 'hclust'
## function to the distance matrix.
D <- dist(X)
hc_out <- hclust(D, method = "complete")

## Dendrogram.
plot(hc_out)

## Scree plot for deciding how many clusters to use. We look for an "elbow".
## I would go down to the 5th point from the right, which corresponds to 6
## clusters.
plot(1:(n-1), hc_out$height)

## Extract 6 cluster membership. Very few observations in clusters 3-6.
hc_out_6 <- cutree(hc_out, k = 6)
hc_out_6
table(hc_out_6)

## Investigate the handful of observations that were assigned to clusters 3-6.
## All of these bridges were big and expensive.
X[hc_out_6 %in% c(4, 5, 6), ]
summary(X)

## K-means clustering. Based on the hierarchical clustering results, we'll
## ask for 6 clusters.
km_out <- kmeans(X, 6)

## Check agreement between hierarchical and k-means clustering. The ordering
## of the cluster labels is arbitrary, so we don't necessarily expect a
## diagonal matrix. What we see is that most of the groupings according to
## hierarchical clustering are also made by k-means clustering.
table(hc_out_6, km_out$cluster)

## One way to visualize K-means clustering output is to draw a scatterplot of
## the first two PCs and color-code the points based on cluster membership.
cl <- rainbow(6)

plot(PC_1, PC_2)
points(PC_1, PC_2, col = cl_clust, pch = 20)
points(PC_1, PC_2, col = cl[km_out$cluster], pch = 20)
