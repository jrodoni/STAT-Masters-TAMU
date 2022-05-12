library(glmnet)
library(My.stepwise)
library(lars)


#First I generate a data set of 10000 totally independent numbers, then put them in a matrix of 100 * 100:
#100 predictors that are independently generated.  I'm also generating a y totally independently of the predictors.

data <- rnorm(10000, mean=20, sd=5)
X <- as.data.frame(matrix(data, nrow = 100, ncol=100))
y <- rnorm(100, mean=6, sd=1)
full <- as.data.frame(cbind(X,y))

My.stepwise.lm(Y = "y", variable.list= names(X), data=full, sle=0.05, sls=1)


cvfit <- glmnet::cv.glmnet(Xnew, y)
coef(cvfit, s = "lambda.1se")  #chooses the best model.
cvfit$glmnet.fit #"Dev" is short for Deviance - sort of like RSS; 
# %Dev is the explained % Deviance - think of this as R^2.

plot.cv.glmnet(cvfit)
