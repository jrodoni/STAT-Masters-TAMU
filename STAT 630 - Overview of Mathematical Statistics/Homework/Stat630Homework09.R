poisson_sample <- read.csv("C:/Users/jackr/OneDrive/Desktop/Graduate School Courses/STAT 630 - Overview of Mathematical Statistics/R Examples/poisson_sample.csv")
View(poisson_sample)

x = poisson_sample[,1]

lambda = mean(x)
theta = 1/sqrt(lambda)

asymp_se = sqrt(1/(4*(length(x)^2)*(lambda^2)))

asymp_se
lambda
theta


y = c( 4.7, 5.5, 4.4, 3.3, 4.6, 5.3, 5.2, 4.8, 5.7, 5.3)
mean(y)

qnorm(.975)

mean(y) - qnorm(.975)*sqrt(.5/10)
mean(y) + qnorm(.975)*sqrt(.5/10)


mean(y) - qt(.975,df = 9)*(sd(y)/sqrt(10))
mean(y) + qt(.975, df = 9)*(sd(y)/sqrt(10))

.62 - qnorm(.95)*sqrt((.62*.38)/250)
.62 + qnorm(.95)*sqrt((.62*.38)/250)


n = 250
(.62 + (qnorm(.95)^2) - qnorm(.95)*sqrt(((.62*.38)/n) + (qnorm(.95)/250000)))

num1 = .62 + (qnorm(.95)^2)/(2*n) 

num1

num2 = qnorm(.95)*sqrt(((.62*.38)/n) + (qnorm(.95)^2)/(250000))
num2

denom = (1+(qnorm(.95)^2/n))

(num1- num2)/denom

(num1 + num2)/denom



lambda_hat = mean(c(9,10,8,12,11,12,5,13,9,9,7,5,16,13,9,5,13,8,9,10))
lambda_hat

c(lambda_hat - qnorm(.975)*sqrt(lambda_hat/20),lambda_hat + qnorm(.975)*sqrt(lambda_hat/20))





num1 = 2*20*lambda_hat+qnorm(.975)^2
num1

num2 = qnorm(.975)*sqrt(4*20*lambda_hat+qnorm(.975)^2)

denom = 2*20

(num1-num2)/denom
(num1+num2)/denom





N = 10000
n = 20
Score = matrix(nrow = N, ncol = 2)
Wald = matrix(nrow=N, ncol = 2)

for(i in 1:N){
  lambda_hat_temp = mean(rpois(n = 20, lambda = 11))
  Score[i,1] = (2*n*lambda_hat_temp+qnorm(.975)^2-qnorm(.975)*sqrt(4*n*lambda_hat_temp+qnorm(.975)^2))/(2*n)
  Score[i,2] = (2*n*lambda_hat_temp+qnorm(.975)^2+qnorm(.975)*sqrt(4*n*lambda_hat_temp+qnorm(.975)^2))/(2*n)
  
  Wald[i,1] = lambda_hat_temp - qnorm(.975)*sqrt(lambda_hat_temp/n)
  Wald[i,2] = lambda_hat_temp + qnorm(.975)*sqrt(lambda_hat_temp/n)
  
}
#library(dplyr)

Score_cov = NULL
Wald_cov = NULL

for(k in 1:N){
  Score_cov[k] = between(11, left = Score[k,1], right = Score[k,2])
  Wald_cov[k] = between(11, left = Wald[k,1], right = Wald[k,2])
} 


sum(Score_cov)/N
sum(Wald_cov)/N






N = 10000
n = 20
Scores = NULL
Walds = NULL

for(k in 1:1000){
  
  Score_cov = NULL
  Wald_Cov = NULL

  for(i in 1:N){
    lambda_hat_temp = mean(rpois(n = 20, lambda = 11))
  
    score_low_temp = (2*n*lambda_hat_temp+qnorm(.975)^2-qnorm(.975)*sqrt(4*n*lambda_hat_temp+qnorm(.975)^2))/(2*n)
    score_high_temp = (2*n*lambda_hat_temp+qnorm(.975)^2+qnorm(.975)*sqrt(4*n*lambda_hat_temp+qnorm(.975)^2))/(2*n)
    Score_cov[i] = between(11, score_low_temp, score_high_temp)
  
  
    wald_low_temp = lambda_hat_temp - qnorm(.975)*sqrt(lambda_hat_temp/n)
    wald_high_temp = lambda_hat_temp + qnorm(.975)*sqrt(lambda_hat_temp/n)
    Wald_cov[i] = between(11, wald_low_temp, wald_high_temp)
  
    if(i == N){
      Scores[k] = sum(Score_cov)/N
      Walds[k] = sum(Wald_cov)/N
    }
  }
}


Diff = Scores-Walds

length(which(Diff <= 0))/1000
#library(dplyr)

Score_cov = NULL
Wald_cov = NULL

for(k in 1:N){
  Score_cov[k] = between(11, left = Score[k,1], right = Score[k,2])
  Wald_cov[k] = between(11, left = Wald[k,1], right = Wald[k,2])
} 


sum(Score_cov)/N
sum(Wald_cov)/N



