library(ISLR)
names(Smarket)
summary(Smarket)
?Smarket
pairs(Smarket,col=Smarket$Direction)
# Logistic regression
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket,family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response")#predict response probability 
glm.probs[1:5]
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
attach(Smarket)
table(glm.pred,Direction)#confusion table
mean(glm.pred==Direction)#classification accuracy from logistic regression
sum(Direction=="Up")/length(Direction)#classificatin accuracy if we classify every observation to "UP"

# Make training and test set
train = Year<2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket,family=binomial, subset=train)
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response") 
glm.pred=ifelse(glm.probs >0.5,"Up","Down")
Direction.2005=Smarket$Direction[!train]
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
sum(Direction.2005=="Up")/length(Direction.2005)

#Fit smaller model
glm.fit=glm(Direction~Lag1+Lag2, data=Smarket,family=binomial, subset=train)
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response") 
glm.pred=ifelse(glm.probs >0.5,"Up","Down")
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
106/(76+106)#TPR


require(MASS)

## Linear Discriminant Analysis
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket, subset=Year<2005)
lda.fit
plot(lda.fit)
Smarket.2005=subset(Smarket,Year==2005)
lda.pred=predict(lda.fit,Smarket.2005)
lda.pred[1:5,]
class(lda.pred)
data.frame(lda.pred)[1:5,]
data.frame(lda.pred)[1:5,c(1,3)]
table(lda.pred$class,Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)

## K-Nearest Neighbors
library(class)
?knn
attach(Smarket)
Xlag=cbind(Lag1,Lag2)
train=Year<2005
knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=3)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])

## SVM
library(e1071)
svmfit=svm(Direction~Lag1+Lag2,data=Smarket,subset=Year<2005, kernel="linear", cost=.1)
svm.pred=predict(svmfit, Smarket[!train,])
table(svm.pred, Direction[!train])
mean(svm.pred==Direction[!train])



#################################
#Smaller example to visualize SVM
library(ROCR)
library(e1071)
# Support Vector Classifier (Linear SVM classifier)
# To demonstrate the SVM, it is easiest to work in low dimensions, so we can see the data.
set.seed(1)
# Lets generate some data in two dimensions
x=matrix(rnorm(40),20,2)
y=rep(c(-1,1),c(10,10))
# Make the two labels a little separated.
x[y==1,]=x[y==1,]+1
plot(x,col=y+3,pch=19)
# Need to specify lable Y as a factor to use svm function
dat=data.frame(x=x, y=as.factor(y))
# Notice that we have to specify a `cost` parameter, which is a tuning parameter. 
svmfit=svm(y~., data=dat, kernel="linear", cost=10, scale=FALSE)
print(svmfit)
plot(svmfit, dat)
# The support points (points on the margin, or on the wrong side of the margin) are indexed in the `$index` component of the fit.
svmfit$index
summary(svmfit)
# Rerun the above code with different value for cost, for example cost=1 and 0.1, see what happens


# The plot function is somewhat crude, and plots X2 on the horizontal axis (unlike what R would do automatically for a matrix). 
# Lets see how we might make our own plot.
# The first thing we will do is make a grid of values for X1 and X2. 
# We will write a function to do that, in case we want to reuse it. 
# It uses the handy function `expand.grid`, and produces the coordinates of `n*n` points on a lattice covering the domain of `x`. 
# Having made the lattice, we make a prediction at each point on the lattice. 
# We then plot the lattice, color-coded according to the classification. 
# Now we can see the decision boundary.
make.grid=function(x,n=75){
  grange=apply(x,2,range)
  x1=seq(from=grange[1,1],to=grange[2,1],length=n)
  x2=seq(from=grange[1,2],to=grange[2,2],length=n)
  expand.grid(x.1=x1,x.2=x2)
}
svmplot=function(svmfit,x){
  # Make a grid
  xgrid=make.grid(x)
  # Predict on the grid
  ygrid=predict(svmfit, xgrid)
  # The `svm` function is not too friendly, we have to do some work to get back the linear coefficients, as described in the text. 
  # Probably the reason is that this only makes sense for linear kernels, and the function is more general. 
  # Here we will use a formula to extract the coefficients.
  # For those interested in where this comes from, have a look in chapter 12 of ESL ("Elements of Statistical Learning").
  # We extract the linear coefficients, and then using simple algebra, we include the decision boundary and the two margins.
  beta=drop(t(svmfit$coefs)%*%x[svmfit$index,])
  beta0=svmfit$rho
  plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=.2)
  points(x,col=y+3,pch=19)
  points(x[svmfit$index,],pch=5,cex=2)
  # Plot the decision boundary
  abline(beta0/beta[2],-beta[1]/beta[2])
  # Plot the two margins
  abline((beta0-1)/beta[2],-beta[1]/beta[2],lty=2)
  abline((beta0+1)/beta[2],-beta[1]/beta[2],lty=2)
}
svmfit=svm(y~., data=dat, kernel="linear", cost=10, scale=FALSE)
svmplot(svmfit,x)
# Just like for the other models in this book, the tuning parameter `C` has to be selected.
# Different values will give different solutions. 
# Rerun the code above, but using `C=1`, and see what we mean. 
# One can use cross-validation to do this.
### See how nice to make up your own function to do the plot ###





