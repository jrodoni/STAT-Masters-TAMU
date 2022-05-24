require(ISLR)
require(boot)
require(glmnet)

?cv.glm
?Auto
plot(mpg~horsepower,data=Auto)
?glm
#fit glm (linear model in this case)
glm.fit=glm(mpg~horsepower, data=Auto)

#K-fold CV (to estimate prediction error)
cvglm = cv.glm(Auto,glm.fit,K=10)

#Validation-set approach (to select tuning parameter)
val.error10=rep(0,10)
degree=1:10
ind = sample(nrow(Auto))
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto,subset = ind[1:(nrow(Auto))/2])
  val.error10[d] = mean((Auto$mpg[ind[((nrow(Auto))/2+1):nrow(Auto)]]-predict(glm.fit,newdata = Auto[ind[((nrow(Auto))/2+1):nrow(Auto)],]))^2)
}
plot(degree,val.error10,type="b")

#repeat it for another 9 times
for (i in 2:10){
  val.error10=rep(0,10)
  degree=1:10
  ind = sample(nrow(Auto))
  for(d in degree){
    glm.fit=glm(mpg~poly(horsepower,d), data=Auto,subset = ind[1:(nrow(Auto))/2])
    val.error10[d] = mean((Auto$mpg[ind[((nrow(Auto))/2+1):nrow(Auto)]]-predict(glm.fit,newdata = Auto[ind[((nrow(Auto))/2+1):nrow(Auto)],]))^2)
  }
  lines(degree,val.error10,type="b",col=i)
}



## 10-fold CV (to select tuning parameter)
cv.error10=rep(0,10)
degree=1:10
for(d in degree){
  set.seed(1)
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
plot(degree,cv.error10,type="b")

#repeat it for another 9 times
for (i in 2:10){
  cv.error10=rep(0,10)
  degree=1:10
  for(d in degree){
    set.seed(i)
    glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
    cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]
  }
  lines(degree,cv.error10,type="b",col=i)
}
 







## 5-fold CV (to select tuning parameter)
cv.error5=rep(0,10)
for(d in degree){
  set.seed(1)
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error5[d]=cv.glm(Auto,glm.fit,K=5)$delta[1]
}
plot(degree,cv.error5,type="b")

#repeat it for another 9 times
for (i in 2:10){
  cv.error5=rep(0,10)
  for(d in degree){
    set.seed(i)
    glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
    cv.error5[d]=cv.glm(Auto,glm.fit,K=5)$delta[1]
  }
  lines(degree,cv.error5,type="b",col=i)
}




## Bootstrap
?boot
?Portfolio
## Minimum risk investment - Section 5.2
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  alpha.hat=(var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y))
  return(alpha.hat)
}
alpha.fn(Portfolio,1:100)

set.seed(1)
sample(1:5,5,replace=T)
alpha.fn(Portfolio,sample(1:100,100,replace=T))
alpha.fn(Portfolio,sample(1:100,100,replace=T))

boot.out=boot(Portfolio,alpha.fn,R=1000)
boot.out
plot(boot.out)

## Estimating the Accuracy of a Linear Regression Model
boot.fn=function(data,index){  
	return(coef(lm(mpg~horsepower,data=data,subset=index)))
}
boot.fn(Auto,1:392)
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,data=Auto))$coef
boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef



