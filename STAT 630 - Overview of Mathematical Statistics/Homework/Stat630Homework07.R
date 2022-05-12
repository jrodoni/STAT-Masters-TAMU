###### STAT 630 - Assignment 7 (DUE 10/29/2021) ######

# 1.)  Chapter 4 Exercise 4.2.12.  Note:Mn= ??Xn=(1/n)*(X1+···+Xn), the mean of 
# a sample of n data. Use R to generate the random exponential variables with rexp.
# The second argument for rexp(a value that you provide) is the parameter ?? in the 
# book's notation.  The mean of a vector x is given by mean(x).

# CHP 4 Exercise 4.2.12:
#   Generate i.i.d. X1,..., Xn distributed Exponential(5) and compute Mn when
#   n = 20. Repeat this N times, where N is large (if possible, take N = 10^5, otherwise
#   as large as is feasible), and compute the proportion of values of Mn that lie between
#   0.19 and 0.21. Repeat this with n = 50. What property of convergence in probability
#   do your results illustrate?

means_exp = NULL
reps = 100000
for(i in 1:reps){
  x = rexp(20,5)
  means_exp[i] = mean(x)
}

(length(which(means_exp<=0.21)) - length(which(means_exp<=.19)))/reps


means_exp = NULL
reps = 100000
for(i in 1:reps){
  x = rexp(50,5)
  means_exp[i] = mean(x)
}

(length(which(means_exp<=0.21)) - length(which(means_exp<=.19)))/reps

# The property of the central limit theorem


# 2.)  Chapter 4 Exercise 4.4.4:

# CHP 4 Exercise 4.4.4


# 3.) CHP 4 Exercise 4.4.12 Add
# (d-f) Determine the exact distribution of the average time to service for the
# first n customers,when n= 16, 36, 100.  (Hint:  use the mgf.)  Then use the 
# pgamma function in R to find the exact probability and compare it to the normal 
# approximation.

# CHP 4 Exercise 4.4.12:
#   Suppose the service time, in minutes, at a bank has the Exponential distribution
#   with ?? = 1/2. Use the central limit theorem to estimate the probability that the average
#   service time of the first n customers is less than 2.5 minutes, when
   
#   (a) n = 16

# means_exp16 = NULL
# reps = 100000
# for(i in 1:reps){
#   x = rexp(16,1/2)
#   means_exp16[i] = mean(x)
# }
# 
# length(which(means_exp16<=2.5))/reps
# 
# hist(means_exp16, breaks = 50)
# abline(v = 2.5, col = "red")
# 
# mean(means_exp16)

### NOTE: the stuff commented out was wrong, shouldve done the following:
n = 16
pnorm((2.5-2)/(2/sqrt(n)))



#   (b) n = 36

# means_exp36 = NULL
# reps = 100000
# for(i in 1:reps){
#   x = rexp(36,1/2)
#   means_exp36[i] = mean(x)
# }
# 
# length(which(means_exp36<=2.5))/reps
# 
# hist(means_exp36, breaks = 50)
# abline(v = 2.5, col = "red")  
# 
# mean(means_exp36)

### NOTE: the stuff commented out was wrong, shouldve done the following:
n = 36
pnorm((2.5-2)/(2/sqrt(n)))



#   (c) n = 100
# 
# means_exp100 = NULL
# reps = 100000
# for(i in 1:reps){
#   x = rexp(100,1/2)
#   means_exp100[i] = mean(x)
# }
# 
# length(which(means_exp100<=2.5))/reps
# 
# hist(means_exp100, breaks = 50)
# abline(v = 2.5, col = "red")
# 
# mean(means_exp100)

### NOTE: the stuff commented out was wrong, shouldve done the following:

n = 100
pnorm((2.5-2)/(2/sqrt(n)))


#   (d)
pgamma(2.5, shape = 16, rate = 8)

#   (e)
pgamma(2.5, shape = 36, rate = 18)

#   (f)

pgamma(2.5, shape = 100, rate = 50)


# 4.) Chapter 4 Exercise 4.4.16
# CHP 4 Exercise 4.4.16:
#   Generate N samples X1, X2,..., X30 ??? Uniform[???20, 10] for N large (N =
#   10^4, if possible). Use these samples to estimate the probability P(M30 ??? ???5). How
#   does your answer compare to what the central limit theorem gives as an approximation?

reps = 10000
means_unif30 = NULL
for(i in 1:reps){
  means_unif30[i] = mean(runif(30, min = -20, max = 10))
}

length(which(means_unif30<=-5))/reps




# 10.) Use R to simulate N=10^4 random samples (Z1,...,Zn) from the normal(0,1) distribution 
#      and compute T= max(Z1,...,Zn) for each sample. Use n= 20 (but you can try larger n for 
#      comparison, if you have time and would like to.) max(x) gets the maximum value in a 
#      vector x. Use hist and boxplot to obtain a histogram and box-plot of your N values 
#      of T. Comment on the histogram shape and symmetry.

reps = 10^4
t = NULL
for(i in 1:reps){
  x = rnorm(20,mean = 0,sd = 1)
  t[i] = max(x)
}

hist(t, breaks = 50)
boxplot(t)


# n = 50
reps = 10^4
t = NULL
for(i in 1:reps){
  x = rnorm(1500000,mean = 0,sd = 1)
  t[i] = max(x)
}

hist(t, breaks = 50)

