### Functions

######################### Handout 4 #########################

#### gaussian kernal density estimate

gauss.kern.density.estimate = function(data,h,y){
  
  n = length(data)
  
  kern_Density_estimate = NULL
  for(i in seq_along(data)) {
    kern_Density_estimate[i] = (1/(n*h))*(1/sqrt(2*pi))*exp(-0.5*((y-data[i])/h)^2)
  }
  return(sum(kern_Density_estimate))
  
  
}

### alternatively:

# d = density("your data here", kernel = "g", bw = 3, na.rm = TRUE) 
dd = approxfun(d$x, d$y)
dd(3)
dd(16)



###################### Handout 7 ############################
library(survival) # need this library for fitting censored data (See HW4)



###################### Handout 9 ############################
library(nortest) # contains the functions ad.test(x) cvm.test(x) and lillie.test(x) 
### only tests normality... so we would use shapiro.test(x) anyways



##################### Handout 11 ############################
###### Distribution free CI for Population Quantile Q(u)

dist.free.CI.qu = function(n,u = 0.5,conflevel = 0.95){
  
  if(u == 0.5){
    
    cov = conflevel
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
    
    return(data.frame(R = r, DesiredConf = conflevel, TrueCoverage = coverage))
    
    
  }else{
    
    L=conflevel
    P=u # quantile u
    s=ceiling(n*P)-1
    r=floor(n*P)+1
    cov=0
    while(s<n-1 && r>1 && cov<L){
      s=s+1
      cov=pbinom(s-1,n,P)-pbinom(r-1,n,P)
      if(cov>=L) break;
      r=r-1
      cov=pbinom(s-1,n,P)-pbinom(r-1,n,P)
      }
      
    return(data.frame(R = r, S = s, DesiredConf = conflevel, TrueCoverage = cov))
    
    
  

  
  
  }
}

dist.free.CI.qu(n = length(data$Lab2))


#### Agresti-Coull CI (See handout 11 pg 22)

agresti.coullCI = function(y,n,alpha){
  
  if((n < 40) | min(n*(y/n),n*(1-(y/n))) < 5){
    
    return("Assumptions are not met, Agresti-Coull CI is not appropriate")
  }else{
    y_tild = y+.5*(qnorm(1-alpha/2)^2)
    n_tild = n+qnorm(1-alpha/2)^2
    
    p_tild = y_tild/n_tild
    
    CI = c(p_tild - qnorm(1-alpha/2)*((sqrt(p_tild*(1-p_tild)))/sqrt(n)),p_tild +  qnorm(1-alpha/2)*((sqrt(p_tild*(1-p_tild)))/sqrt(n))) 
    
    return(CI)
    
    
  }
  
}




### negative binomial quantile funtion

qneg.binom = function(q,r,prob = 0.5){
  
  sum = 0
  i = r
  
  while(sum<q){
    
    sum = sum + choose(i-1,r-1)*(p^r)*(1-p)^(i-r)
    
    if(sum<q){
      i = i+1
    }
    
  }
  
  
  return(i)
  
}






###################### Handout 12 ############################

#### Test about population proportion P (See H.O. 12 pg 56)
test.pop.proportion = function(y="",p_hat = "",n,p0,alpha, alternative = c("l","g","ne")){
  if(p_hat == ""){
    p_hat = y/n
  }
  
  if(y == ""){
    y = p_hat*n
  }
  
  if((min(p_hat*n,n*(1-p_hat)) < 5) | n<40){
    
    if(alternative == c("g")){
      
      critval = qbinom(1-alpha,n,p0)
      p_val = 1-pbinom(y-1,n,p0)
      
      return(cat("Reject Null if Y=",y,">",critval,
                 "\nP-value =",p_val))
      
    }else if(alternative == c("l")){
      
      critval = qbinom(alpha,n,p0)
      p_val = pbinom(Y,n,p0) 
      
      return(cat("Reject Null if Y=",y,"<",critval,
                 "\nP-value =",p_val))
      
    }else if(alternative == c("ne")){
      
      critval = c(qbinom(alpha/2,n,p0),qbinom(1-alpha/2,n,p0))
      p_val = 2*min(pbinom(y,n,p0),1-pbinom(y-1,n,p0))
      
      return(cat("Reject Null if Y=",y,"<",critval[1],"or Y =",y,">",critval[2],
                 "\nP-value =",p_val))
      
    }
    
    
    
  }else{
    
    TS = (p_hat - p0)/sqrt((p0*(1-p0))/n)
    
    if(alternative == c("g")){
      critval = qnorm(1-alpha)
      p_val = 1-pnorm(TS)
      
      return(cat("Reject Null if TS=",TS,">",critval,
                 "\nP-value =",p_val))
      
    }else if(alternative == c("l")){
      
      critval = qnorm(alpha)
      p_val = pnorm(TS)
      
      return(cat("Reject Null if TS=",TS,"<",critval,
                 "\nP-value =",p_val))
      
    }else if(alternative == c("ne")){
      
      critval = qnorm(1-alpha/2)
      p_val = 2*(1-pnorm(abs(TS)))
      
      return(cat("Reject Null if |TS|=",abs(TS),">",critval,
                 "\nP-value =",p_val))
      
      
    }
    
    
  }
  
}

test.pop.proportion(y=8,n=25,p0=0.20,alpha = .05,alternative = c("g"))

#############################################################################################

### sample size calucaltion for population proportion p (Ho12 pg 60)
sample.size.pop.prop = function(p0,p1,alpha,beta){
  
  delta = abs(p1-p0)
  z_a = abs(qnorm(alpha))
  z_b = abs(qnorm(beta))
  
  n = ceiling((((z_a*sqrt(p0*(1-p0)))+(z_b*sqrt(p1*(1-p1))))/delta)^2)
  
  return(n)
}
sample.size.pop.prop(p0 = .8,p1=.9, alpha = .05, beta = .80)


#### Sign Test-Hypotheses about population median (Ho12 pg 37)
sign.test = function(data, med0,alpha = 0.05, alternative = c("g","l","ne")){
  
  x = data-med0
  n = length(x)-sum(data==med0)
  s_plus = sum(x>0)
  
  if(alternative == c("g")){
    
    B = qbinom(1-alpha,n,0.5)
    if(1-pbinom(B,n,0.5)>alpha){
      B = B+1
    }
    truelevel = 1-pbinom(B,n,0.5)
    p_value = 1-pbinom(s_plus-1,n,0.5)
    
    return(cat("Reject Ho if S+=",s_plus,">",B,
               "\nP-value =",p_value,
               "\nTrue level of test =", truelevel))
    
  }else if(alternative==c("l")){
    
    
    B = qbinom(alpha,n,0.5)
    
    if(pbinom(B,n,0.5)>alpha){
      B = B-1
    }
    truelevel = pbinom(B,n,0.5)
    p_value = pbinom(s_plus,n,0.5)
    
    return(cat("Reject Ho if S+=",s_plus,"<",B,
               "\nP-value =",p_value,
               "\nTrue level of test =", truelevel))
    
    
  }else if(alternative==c("ne")){
    
    Smax = max(s_plus, n-s_plus)
    B = qbinom(1-alpha/2,n,0.5)
    if(1-pbinom(B,n,0.5)>alpha/2){
      B = B+1
    }
    truelevel = 2*(1-pbinom(B,n,0.5))
    p_value = 2*(1-pbinom(Smax-1,n,0.5))
    
    
    return(cat("Reject Ho if Smax=",Smax,">",B,
               "\nP-value =",p_value,
               "\nTrue level of test =", truelevel))
    
    
  }
  
  
}
# alternatively, use 
binom.test(x,n,p=0.5, alternative = c("greater","less","two.sided"), conf.level = 0.95)
# where x = # of values with w>med0



#### Wilcoxon Signed Rank Test (H.O. 12 pg 40)

wilcox.signed.rank.test.one = function(y,med0,alpha,alternative = c("g","l","ne")){
  
  x = y-med0
  x = x[-which(x==0)]
  n = length(x)
  Group = c(x>0)
  
  grouped = as.data.frame(cbind(x,Group))
  grouped=grouped[order(abs(grouped[,1])),]
  grouped$rank = seq(1:length(grouped[,1]))
  
  for(i in abs(grouped$x)){
    rank_temp = grouped$rank[which(abs(grouped$x) == i)]
    if(length(rank_temp>1)){
      grouped$rank[which(abs(grouped$x) == i)] = sum(rank_temp)/length(rank_temp)
    }
  }
  wp = sum(grouped$rank[which(grouped$Group == 1)])
  wn = sum(grouped$rank[which(grouped$Group == 0)])
  wm = max(wp,wn)
  
  if(alternative == c("g")){
    critval = qsignrank(alpha,n,FALSE)
    pval = psignrank(wp-1,n,FALSE)
    true_level = psignrank(qsignrank(alpha,n,FALSE)-1,n,FALSE)
    
    
    return(cat("Reject Null if W+ =",wp,">",critval, 
               "\nP-value = ",pval,
               "\nTrue level of Test=",true_level))
    
  }else if(alternative == c("l")){
    critval = qsignrank(alpha,n,TRUE)
    pval = psignrank(wp,n,TRUE)
    true_level = psignrank(qsignrank(alpha,n,TRUE),n,TRUE)
    
    return(cat("Reject Null if W+ =",wp,"<",critval, 
               "\nP-value = ",pval,
               "\nTrue level of Test=",true_level))
    
  }else if(alternative == c("ne")){
    critval = qsignrank(alpha/2,n,FALSE)
    pval = 2*psignrank(wm-1,n,FALSE)
    true_level = 2*psignrank(qsignrank(alpha/2,n,FALSE)-1,n,FALSE)
    
    return(cat("Reject Null if Wmax =",wm,">",critval, 
               "\nP-value = ",pval,
               "\nTrue level of Test=",true_level))
    
  }
  
  
  
}
### if no ties in the data use:
x = data
y = rep(med0, times = length(x))
wilcox.test(x,y, alternative = "l",paired = T)



#### Test about Normal Population SD (Ho 12 pg 50-51)

sd.norm.test = function(x,sigma0,alpha,alternative = c("l","g","ne")){
  
  n = length(x)
  TS = (n-1)*(sd(x)^2)/(sigma0^2)
  
  if(alternative == c("g")){
    
    t = qchisq(1-alpha,n-1)
    pvalue = 1-pchisq(TS,n-1)
    power = 1-pchisq(((sigma0^2)/(sd(x)^2))*qchisq(1-alpha,n-1),n-1)
    
    return(cat("Reject H0 if TS=",TS,">",t,
               "\nP-value=",pvalue,
               "\nPower =",power))
    
    
  }else if(alternative==c("l")){
    
    t = qchisq(alpha,n-1)
    pvalue = pchisq(TS,n-1)
    power = pchisq(((sigma0^2)/(sd(x)^2))*qchisq(alpha,n-1),n-1)
    
    return(cat("Reject H0 if TS=",TS,"<",t,
               "\nP-value=",pvalue,
               "\nPower =",power))
    
    
    
  }else if(alternative==c("ne")){
    
    tl = qchisq(alpha/2,n-1)
    tu = qchisq(1-alpha/2,n-1)
    pvalue = 2*min(pchisq(TS,n-1),1-pchisq(TS,n-1))
    power = pchisq(((sigma0^2)/(sd(x)^2))*qchisq(alpha/2,n-1),n-1) +
      1-pchisq(((sigma0^2)/(sd(x)^2))*qchisq(1-alpha/2,n-1),n-1)
    
    return(cat("Reject H0 if TS=",TS,"<",tl, "or TS=",TS,">",tu,
               "\nP-value=",pvalue,
               "\nPower =",power))
    
  }
  
  
}


##### Power Calculation for Normal Population w/sigma unknown (Less-than alternative) ##### 
##### Power Calculation for Normal Population w/sigma unknown (two-sided alternative) ##### 

tprod_power_twosideed = function(mu,mu_not,sigma, n, alpha){
  delta = sqrt(n)*(mu-mu_not)/sigma
  power = pt(-qt(1-alpha/2,n-1),n-1,delta) + 1 - pt(qt(1-alpha/2,n-1),n-1,delta)
  result = cbind(mu,delta,power)
  result = as.data.frame(result)
  return(result)
}


pbinom(qbinom(1-0.05,20,.2), 20, .40)





###################### Handout 13 ############################

#### Wilcoxon Rank Sum test about location parameters (both populations nonnormal but belong to same loc-scale family)
wilcoxon.rank.sum.test = function(data1,data2,alpha,alternative = c("g","ne")){
  
  n = length(data1)
  m = length(data2)
  
  Groups = c(rep(0, times = n),rep(1, times = m))
  datac = c(data1, data2)
  
  data = data.frame(Data = datac, Groups = Groups)
  data = data[order(data$Data),]
  rownames(data) = seq(1:length(data[,1]))
  data$Rank = seq_along(datac)
  
  for(i in data$Data){
    
    rank_temp = data$Rank[which(data$Data == i)]
    
    if(length(rank_temp) > 1){
      data$Rank[which(data$Data == i)] = sum(rank_temp)/length(rank_temp)
    }
    
  }
  
  
  w1 = sum(data$Rank[which(data$Groups == 0)])
  w2 = sum(data$Rank[which(data$Groups == 1)])
  
  
  
  if(alternative==c("g")){
    
    W = qwilcox(1-alpha,n,m)+(n*(n+1))/2
    pval = 1-pwilcox(w1-1-(n*(n+1))/2,n,m)
    
    return(cat("Reject Ho (theta1<=theta2) if W1=",w1,">",W,
               "\nP-value =", pval))
    
  }else if(alternative == c("ne")){
    
    W = qwilcox(1-alpha/2,n,m)+(n*(n+1))/2
    pval = 2*min(1-pwilcox(w1-1-(n*(n+1))/2,n,m),pwilcox(w1-(n*(n+1))/2,n,m))
    
    return(cat("Reject Ho (theta1!=theta2) if W1=",w1,">",W, "or W1=",w1,"<", n*(n+m+1) - W,
               "\nP-value =", pval))
    
  }
  
  
}

# if no ties in the data, just use built in R function
wilcoxon.rank.sum.test(UnExposed,PCB_Exposed, alpha = 0.05, alternative = c("g"))
wilcox.test(PCB_Exposed,UnExposed,alternative = "l", paired = F)
# not the difference in the ordering of the data and the difference in the alternatives



##### Runs test (see HW8 # 7)
n=length(data$Lab1)
means = mean(data$Lab1)
diff = data$Lab1-means
n.neg = rep(0,n)
n.pos = rep(0,n)
n.neg = length(diff[diff<0])
n.pos = length(diff[diff>0])
numb.runs = 1
for (j in 2:n) {
  if (sign(diff[j]) != sign(diff[j-1])) {numb.runs = numb.runs + 1}
}
runs.result = as.data.frame(cbind(numb.runs, n.pos, n.neg))
names(runs.result) = c("No. runs", "N+", "N-")
runs.result

mu = 1 + (2*n.neg*n.pos)/(n.neg + n.pos)
sig2 = (2*n.neg*n.pos*(2*n.neg*n.pos - n.neg - n.pos))/((n.neg + n.pos)^2*(n.neg + n.pos-1))
z = (abs(numb.runs-mu)-.5)/sqrt(sig2)
pvalue = 2*(1-pnorm(abs(z)))
results = cbind(mu,sig2,z,pvalue)
results







