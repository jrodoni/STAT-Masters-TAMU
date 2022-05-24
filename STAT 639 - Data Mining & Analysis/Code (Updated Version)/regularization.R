library(ISLR)
?Hitters
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))#number of missing values
Hitters=na.omit(Hitters)#remove observations with missing values
dim(Hitters)
sum(is.na(Hitters))


library(glmnet)
?model.matrix
x=model.matrix(Salary~.,Hitters)[,-1] # take out the first column which are all 1's for intercept
y=Hitters$Salary

x=scale(x)

?glmnet

# Ridge Regression
fit.ridge=glmnet(x,y,alpha=0) # alpha=0 for ridge and alpha=1 for lasso (default alpha=1)
plot(fit.ridge,xvar="lambda",label=TRUE) # plot ridge solution path
?cv.glmnet
cv.ridge=cv.glmnet(x,y,alpha=0) # use cross validation for choosing the penalty parameter lambda
plot(cv.ridge) # plot cv.error v.s a sequence of lambda values; the two dashed lines correspond to the best lambda value and the largest value of lambda such that error is within 1 standard error of the best lambda 
ridge.best.lambda=cv.ridge$lambda.min # find the best lambda value corresponding to min cv.error
log(ridge.best.lambda) # what is log of best lambda, compare to the plot
min(cv.ridge$cvm) # min cv.error
log(cv.ridge$lambda.1se) # the largest value of lambda such that error is within 1 standard error of the best lambda 


# Lasso Regression
fit.lasso=glmnet(x,y)
plot(fit.lasso,xvar="lambda",label=TRUE)
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)
lasso.best.lambda=cv.lasso$lambda.min # find the best lambda value corresponding to min cv.error
log(lasso.best.lambda)
min(cv.lasso$cvm) # min cv.error

lasso.best.lambda

# find the best lambda value corresponding to 1 standard error above the minimum MSE
# usually more conservative (fewer variables) solution than the minimum MSE
lasso.lambda.1se=cv.lasso$lambda.1se 






nlambda=length(cv.lasso$lambda)
MSE=rep(0,nlambda)
for (i in 1:nlambda){
  MSE[i]=my.cv.lasso(cv.lasso$lambda[i],x,y)
}
plot(log(cv.lasso$lambda),MSE)






