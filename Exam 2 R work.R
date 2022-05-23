qnorm(.98)
qnorm(.5)

x = rgamma(n = 1000, shape = .8, scale = 25)
qqnorm(x)
qqline(x)

qgamma(.5,shape = .8, scale = 25)

x = rweibull(n = 1000, shape = 1.5, scale = 25)
qqnorm(x)
qqline(x)

qweibull(.5,shape = 1.5, scale = 25)

x = rexp(n = 1000, rate = 1/29)
qqnorm(x)
qqline(x)




y_upper = exp(2.591+1.453*2.285)
y_lower = exp(2.591-1.453*2.285)
c(y_lower, y_upper)

##### AC computation
alpha = 0.05
n = 100
Y = 92

y_tild = Y+.5*(qnorm(1-alpha/2)^2)
n_tild = n+qnorm(1-alpha/2)^2

p_tild = y_tild/n_tild
p_tild

CI = c(p_tild - qnorm(1-alpha/2)*((sqrt(p_tild*(1-p_tild)))/sqrt(n)),p_tild + qnorm(1-alpha/2)*((sqrt(p_tild*(1-p_tild)))/sqrt(n))) 
CI


qt(.005,df=49)


CI = c(exp(2.591-qt(.995,df=49)*(1.453/sqrt(50))),
       exp(2.591+qt(.995,df=49)*(1.453/sqrt(50))))
CI


###### Finding R for CI of median
n = 50
cov = .90
r = 0
imin = 0
i = 0
ans = 0
anst = 0
m = 1:n
ans = pbinom(n-m,n,.5)-pbinom(m-1,n,.5)
while(i<n)
{
  i = i+1
  if(ans[i]<cov) anst[i] = 2
  if(ans[i]>=cov) anst[i] = ans[i]
}
ansmin = min(anst)
imin = which(anst==ansmin)
r = imin
coverage = ans[r]
r
coverage

### EXAM 22

x=rt(100, df=3)
qqnorm(x)
qqline(x)

x = rt(100, df = 40)
qqnorm(x)
qqline(x)

x = rexp(100, rate = 1/20)
qqnorm(x)
qqline(x)

y_tild = 8+.5*qnorm(.025)^2

n_tild = 60+qnorm(0.025)^2
p_tild = y_tild/n_tild
p_tild

CI = c(p_tild+qnorm(.025)*sqrt(p_tild*(1-p_tild)/n_tild),p_tild-qnorm(.025)*sqrt(p_tild*(1-p_tild)/n_tild))
CI       

(qnorm(.995)*(291)/5)^2
