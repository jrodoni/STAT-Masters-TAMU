library(glmnet) #required for lasso

### my.cv.knn function ###
my.cv.lasso = function(lambda, x, y, K=10){
  ### define function input: 
  ### cost: cost parameter in svm
  ### K: K in K-fold CV
  ### x: train data x as a matrix with n rows and p columns
  ### y: train data label as a vector with length n
  set.seed(0)
  n = nrow(x) ### sample size
  folds = sample(1:K, n, replace=TRUE) ### create folds id for CV
  fold.error = rep(0, K) ### place holder to save error for each fold
  
  for (j in 1:K){#loop through 1 to K
    ##replace the following to lines for other prediction algorithm
    #fit lasso on K-1 fold of data
    lassofit=glmnet(x[folds!=j,],y[folds!=j],lambda=lambda)
    #predict on 1 fold of data
    lasso.pred = predict(lassofit,newx=x[folds==j,],s=lambda) # make predictions
    #lasso.pred = predict(lassofit,newx=x[folds==j,],s=lambda,type="class") # make predictions for classification
    ### MSE
    fold.error[j] = sum((lasso.pred - y[folds==j])^2)
    ### Misclassfication rate
    # fold.error[j] = sum(lasso.pred != y[folds==j])
  }
  ### CV error rate is total misclassification rate
  CV.error = sum(fold.error)/n 
  return(CV.error) ### return value
}
