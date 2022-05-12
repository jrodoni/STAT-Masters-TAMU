### STAT 630: Homework 1 Problem 5

# 5(a)
# Use R to estimate the probability that at least two students in a class of 100 share the
# same 4 digits. Do this by simulating 100,000 samples of size 100, with replacement, from
# the population {0, 1, 2 . . . , 9999} and then determining the proportion of times that a
# sample has at least one duplicate. (Note: if x is the sample then (length(unique(x))!=100)
# returns value TRUE, which computes to 1, when there is at least one duplicate. This is
# a bit simpler than the method shown in the birthday problem example.)

ss_nums = seq(from = 0, to = 9999, by = 1)
nsample = 10000
npeople = 100

dup.true = rep(0,nsample)
for (i in 1:nsample){
  ss_sample = sample(ss_nums, size = npeople, replace = TRUE)
  dup.true[i] = (length(unique(ss_sample))!=npeople)
  if(i == nsample){
    prob = sum(dup.true)/nsample
  }
}
prob


npeople = seq(100,120,by = 1)
nchoices = 10000
probs = 1-exp(lfactorial(10000)-lfactorial(10000-npeople)-npeople*log(10000))  # compute probability
cbind(npeople,probs)  # print result (cbind puts the two vectors into columns)



