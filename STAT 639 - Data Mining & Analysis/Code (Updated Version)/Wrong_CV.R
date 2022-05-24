set.seed(1)
n=50
p=50000
xx = matrix(rnorm(n*p),n,p)
yy = rbinom(n,1,.5)
c=apply(xx,2,function(x) cor(x,yy))
ind = order(abs(c))[(p-99):p]

train = c(rep(TRUE,n/2),rep(FALSE,n/2))

data = data.frame(x=xx[,ind],y=yy)

library(MASS)
lda.fit=lda(y~.,data=data,subset=train)
lda.pred=predict(lda.fit,data)
mean(yy[!train]!=lda.pred$class[!train])