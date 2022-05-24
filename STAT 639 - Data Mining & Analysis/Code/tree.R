# Chapter 8 Lab: Decision Trees
# Fitting Classification Trees
# We will have a look at the `Carseats` data using the `tree` package in R.
library(ISLR)
library(MASS)
library(tree)
library(randomForest)
library(gbm)
attach(Carseats)
?Carseats
# We create a binary response variable `High` (for high sales), and we include it in the same dataframe.
hist(Sales)
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats, High)

# Now we fit a tree to these data, and summarize and plot it. 
# Notice that we have to exclude `Sales` from the right-hand side of the formula, because the response is derived from it.
?tree
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)#pretty good training classification error
plot(tree.carseats)
text(tree.carseats,pretty=0)


# Lets create a training and test set (250,150) split of the 400 observations, 
# grow the tree on the training set, and evaluate its performance on the test set.
set.seed(1)
train=sample(1:nrow(Carseats),250) # by default sample is without replacement
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.pred=predict(tree.carseats,Carseats[-train,],type="class")
table(tree.pred, Carseats[-train,]$High)

# This tree was grown to full depth, and might be too variable. 
# We now use CV to prune it.
set.seed(1)
#?cv.tree
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
plot(cv.carseats)
prune.carseats=prune.misclass(tree.carseats,best=17)
#prune.carseats=prune.misclass(tree.carseats,k=0.5) #this gives the same result; k is the tuning parameter alpha in the slides
plot(prune.carseats);text(prune.carseats,pretty=0)

# Now lets evaluate this pruned tree on the test data.
tree.pred=predict(prune.carseats,Carseats[-train,],type="class")
table(tree.pred, Carseats[-train,]$High)
# It has done about the same as our original tree. 
# So pruning did not hurt us wrt misclassification errors, and gave us a simpler tree.


# Random Forests and Boosting
# These methods use trees as building blocks to build more complex models. 
# Here we will use the Boston housing data to explore random forests and boosting. 
# These data are in the `MASS` package.
# It gives housing values and other statistics in each of 506 suburbs of Boston based on a 1970 census.

# Random forests build lots of bushy trees, and then average them to reduce the variance.
set.seed(101)
dim(Boston)
train=sample(1:nrow(Boston),300)
?Boston

# Lets fit a random forest and see how well it performs. 
# We will use the response `medv`, the median housing value (in \$1K dollars)
rf.boston=randomForest(medv~.,data=Boston,subset=train)
rf.boston

# The MSR is based on OOB a very clever device in random forests to get honest error estimates. 
# The model reports that `mtry=4`, which is the number of variables randomly chosen at each split. 
# Since p=13 here, we could try all 13 possible values of `mtry`. 
# We will do so, record the results, and make a plot.
oob.err=double(13)
test.err=double(13)
for(mtry in 1:13){
  fit=randomForest(medv~.,data=Boston,subset=train,mtry=mtry,ntree=400)
  oob.err[mtry]=fit$mse[400]#Mean squared error for 400 trees
  pred=predict(fit,Boston[-train,])
  test.err[mtry]=mean((Boston[-train,]$medv-pred)^2)
}
matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("Test", "OOB"),pch=19,col=c("red","blue"))
# Although the test-error curve drops below the OOB curve, these are estimates based on data, and so have their own standard errors (which are typically quite large). 
# Notice that the points at the end with `mtry=13` correspond to bagging.

# Boosting builds lots of smaller trees. 
# Unlike random forests, each new tree in boosting tries to patch up the deficiencies of the current ensemble.
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=10000,shrinkage=0.01,interaction.depth=4)
summary(boost.boston)
#plot(boost.boston,i="lstat")
#plot(boost.boston,i="rm")

#Lets make a prediction on the test set. 
#We first compute the test error as a function of the number of trees, and make a plot.
n.trees=seq(from=100,to=10000,by=100)
predmat=predict(boost.boston,newdata=Boston[-train,],n.trees=n.trees)
dim(predmat)
berr=colMeans((predmat-Boston[-train,]$medv)^2)
plot(n.trees,berr,pch=19,ylab="Mean Squared Error", xlab="# Trees",main="Boosting Test Error",ylim=range(c(berr,test.err)))
abline(h=min(test.err),col="red")

#With boosting, the number of trees is a tuning parameter, and if we have too many we can overfit. 
#We use cross-validation to choose the number of trees
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=10000,shrinkage=0.01,interaction.depth=4,cv.fold=10)
n.trees.cv=gbm.perf(boost.boston, method = "cv")
gbmpred=predict(boost.boston,newdata=Boston[-train,],n.trees=n.trees.cv)
mean((gbmpred-Boston[-train,]$medv)^2)



fit=randomForest(medv~.,data=Boston,subset=train,mtry=4,ntree=10000)
plot(fit$mse)


