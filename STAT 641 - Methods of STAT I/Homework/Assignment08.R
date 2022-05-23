
# 2.) An experiment is run to study the effects of PCB, an industrial contaminant, on the reproductive
#     ability of owls. The shell thickness (mm) of eggs produced by 10 owls exposed to PCB are compared to the
#     shell thickness of eggs produced by 10 owls which did not have PCB exposures

PCB_Exposed = c(3.6,3.2,3.8,3.6,4.1,3.8,4.2,3.4,3.7,3.8)
UnExposed = c(4.3,4.4,3.6,3.5,4.4,3.5,3.4,3.6,4.1,4.3)

# (a) Is there significant(?? = .05) evidence that the PCB exposed owls have thinner egg shells than those of
#    the unexposed owls? Use a t-test in reaching your conclusion and report the p-value.

# one of the conditions for using pooled t-test is that the populations are normal.
# not sure if we actually need to check here because we're told to do a t-test, but
# checking anyways
shapiro.test(sort(PCB_Exposed))
shapiro.test(sort(UnExposed))

## starting calcualtions for t-test
x_bar = mean(PCB_Exposed)
y_bar = mean(UnExposed)
theta_hat = x_bar - y_bar 


sigma1 = sd(PCB_Exposed)
sigma2 = sd(UnExposed)

n = m = length(PCB_Exposed)

# sigma_hat = sqrt(((n-1)*(sigma1^2)+(m-1)*(sigma2^2))/(n+m-2))
# note, b/c n=m we can use the following for sigma hat to simplify the calculation
sigma_hat = sqrt(0.5*(sigma1^2 + sigma2^2))

# calculate test statistic t
theta_not = 0

test_statistic = ((x_bar-y_bar)-theta_not)/(sigma_hat*sqrt((1/n)+(1/m))) 
test_statistic

# reject ho at alpha critical level if T < t_alpha with n+m - 2 df
alpha = 0.05
t = qt(alpha, df = n+m-2)

# calculate p-value 
pt(test_statistic,n+m-2)


# (b) Compute the chance that your test committed a Type II error for the following values of ?? = µexposed???
#     µunexposed = 0, ???.5??, ?????, ???1.5??, ???2??. Base this calculation on the pooled t-test.

theta_not = c(0,-0.5*sigma_hat, -sigma_hat,-1.5*sigma_hat,-2*sigma_hat) 
delta = (theta_not)/(sigma_hat*sqrt((1/n)+(1/m)))

theta_not

delta
-qt(1 - alpha, n + m - 2)


power = pt(-qt(1 - alpha, n + m - 2), n + m - 2, delta)

TypeIIErrorProb = 1 - power
TypeIIErrorProb




# (C) In designing a new study, the researchers want to determine the necessary sample sizes for exposed and
#     unexposed owls such that an ?? = .05 test will have power of at least 80% to detect a shell thickness
#     difference of more than 0.3 mm. The researchers want to examine three times as many exposed owls
#     as unexposed owls, that is, m=3n

z_alpha = qnorm(.05)
z_beta = qnorm(0.20)

delta = 0.3
k = 3

n = ceiling(((k+1)/k)*(sigma_hat*(z_alpha+z_beta)/delta)^2)
m = 3*n
c(n,m)



# (d) Is there significant(?? = .05) evidence that the PCB exposed owls have thinner egg shells than those of
#     the unexposed owls? Use a Wilcoxon test in reaching your conclusion and report the p-value.

wilcox.test(PCB_Exposed,UnExposed,alternative = "l", paired = F)

# manual calculation(better in this case b/c n* != n i,e some of the xi = 0)
n = length(PCB_Exposed)
m = length(UnExposed)
 
Groups = c(rep(0, times = length(PCB_Exposed)),rep(1, times = length(UnExposed)))
Shells = c(PCB_Exposed, UnExposed)
  
data = data.frame(Shells = Shells, Groups = Groups)
data = data[order(data$Shells),]
rownames(data) = seq(1:length(data[,1]))
data$Rank = seq(1:20)
 
 
 for(i in data$Shells){
   rank_temp = data$Rank[which(data$Shells == i)]
   if(length(rank_temp) > 1){
     data$Rank[which(data$Shells == i)] = sum(rank_temp)/length(rank_temp)
   }
 }
 
 
w1 = sum(data$Rank[which(data$Groups == 0)])
w2 = sum(data$Rank[which(data$Groups == 1)])
 
w1
w2
 
qwilcox(1-.05, n,m) + (n*(n+1))/2
 
1-pwilcox(w2-55-1,10,10)



# (e) Is there significant(?? = .05) evidence that the PCB exposed owls have greater variability in egg shell
#     thickness than those of the unexposed owls? Report the p-value of your test.

## use Brown-Forsythe-Levene (BFL) test

### See pg 71, H.O. 13
#install.packages("lawstat")
library(lawstat)

data = data.frame(Y = c(PCB_Exposed,UnExposed), Group = factor(c(rep(0,times = length(PCB_Exposed)), 
                                                                 rep(1,times = length(UnExposed)))))
levene.test(data$Y, data$Group)

### See pg 80, h.o 13
#install.packages("car")
library(car)
leveneTest(data$Y,data$Group)

# note: both functions give the same p-value


# 3.) In a study of the effect of vitamin B on learning, 12 pairs of children were matched on IQ, age,
#     size, and general health. Within each pair, one child was randomly selected to receive a vitamin B table
#     every day and the other child received a placebo tablet. The following table shows the change in IQ score
#     over the six months of the study

# Is there substantial evidence that a six months treatment with vitamin B increased IQ score? Use ?? = .05
# in applying both the t-test and the Wilcoxon signed rank test for these hypotheses. Which test produces
# the most reliable conclusion?


vitaminB = c(14, 26, 2, 4, -5, 14, 3, -1, 1, 6, 3, 4)
Placebo  = c(8, 18, -7, -1, 2, 9, 0,-4, 13, 3, 3, 3)
Diffs = vitaminB-Placebo

### Test normality
shapiro.test(sort(vitaminB))
shapiro.test(sort(Placebo))
shapiro.test(sort(Diffs))
# the placebo data seems to be normally distributed but the VitaminB data does not appear to be so


#### T-test
n = length(Diffs)
D_bar = mean(Diffs)
sigma_hat=sd(Diffs)


# calculate test statistic t
theta_not = 0

test_statistic = sqrt(n)*(D_bar-theta_not)/sigma_hat
test_statistic

# reject ho at alpha critical level if T < t_alpha with n+m - 2 df
alpha = 0.05
t = qt(1-alpha, df = n-1)
t
# calculate p-value 
1-pt(test_statistic,n-1)
## or
t.test(vitaminB,Placebo, alternative = "g", paired = T)

##### Wilcoxon signed rank test

wilcox.test(vitaminB,Placebo,alternative = "g", paired = T)

# 4.) A study evaluated the urinary-thromboglobulin excretion in 12 normal and 12 diabetic patients.
#     The excretions were summarized with a value of 20 or less labeled as "Low" and values above 20 as "High".

data = data.frame(Diabetic =c(4,8), Normal = c(10,2))
rownames(data) = c("low", "High")
data
p_hat1 = data$Diabetic[2]/sum(data$Diabetic)
p_hat2 = data$Normal[2]/sum(data$Normal)


## checking if any of the Eij <1 or if 20% of the Eij<5
p_hat1*12
p_hat2*12
### NOte that p_hat2*12 = Expected counts = 2 < 5 => need fisher exact test

data = matrix(c(10,4,2,8), nrow = 2, byrow = F)
fisher.test(data, alternative = "two.sided")



# 5.) A study was conducted to compare two topical anesthetic drugs for use in dentistry. The two
#     drugs were applied on the oral mucous membrane of the two sides of each patient's mouth and after a fixed
#     period of time it was recorded whether or not the membrane remained anesthetized. Data from the 45
#     patients is recorded below

data = data.frame(Anesthetized = c(15,3,18),NAnesthetized = c(13,14,27), Total = c(28,17,45))
rownames(data) = c("Anesthetized", "NAnesthetized", "Total")
data
p1. = data[1,3]/data[3,3]
p.1 = data[3,1]/data[3,3]

m = sum(data[1,2],data[2,1])
m

# At the ?? = .05 level what can you conclude? Report a p-value for your test.
pvalue = 2*min(1-pbinom(data[1,2],size=m,prob=.5), pbinom(data[1,2],size=m, prob = .5))
pvalue


# 6.) A genetics experiment on the characteristics of tomato plants provided the following data on the
#     number of offspring expression four phenotypes

data = data.frame(TallCut = 926, DwarfCut = 293, TallPotato = 288, DwarfPotato = 104, Total = 1611)
rownames(data)="Frequency"
data

pis = c(9/16,3/16,3/16,1/16,1)
E_counts = pis*data$Total
teststat_i = ((data[1,]-E_counts)^2)/E_counts
teststat = sum(teststat_i)
teststat

t = qchisq(1-.01,4-1)
t

pvalue = 1-pchisq(teststat,4-1) 
pvalue


# 7.) A company is attempting to automate the determination of the amount of the active ingredient,
#     chlorphaniramine maleate, in the tablets it produces. Two labs were asked to make 20 determinations on
#     a composite sample which had a nominal dosage level of 4 milligrams. The purpose of the experiment was
#     to study the consistency between labs and the variability of the determination procedure within labs. The
#     data is given in the following table

data = data.frame(Day = 1:20,
                  Lab1 = c(4.13, 4.07, 4.04, 4.07, 4.05, 4.04, 4.02, 4.06, 4.10, 3.86,
                           3.85, 4.08, 4.11, 4.08, 4.01, 4.02, 4.04, 3.97, 4.00, 4.04),
                  Lab2 = c(3.88, 4.02, 4.01, 4.01, 4.04, 3.99, 4.03, 3.97, 3.98, 4.02,
                           3.88, 3.91, 3.95, 3.92, 3.97, 3.92, 3.90, 3.89, 3.97, 3.95)
                  )
Diffs = data$Lab1 - data$Lab2

# (a) Do the readings from the labs appear to have a normal distribution? Justify your answer.
shapiro.test(sort(data$Lab1))
shapiro.test(sort(data$Lab2))
shapiro.test(sort(Diffs))

# (b) Do the readings from the two labs appear to have the same level of variability? Justify your answer.
library(car)
group = factor(c(rep(0, times = length(data$Lab1)),rep(1,times = length(data$Lab2))))
leveneTest(c(data$Lab1,data$Lab2), group)

# (c) Do the daily determinations within each lab appear to be correlated? Justify your answer
#### USE runs procedure

### for Lab1
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

#### For lab 2
n=length(data$Lab2)
means = mean(data$Lab2)
diff = data$Lab2-means
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


# (d)  Do the readings from the two labs appear to have different average determinations? Justify your
#      answer.

#### the data are paired and the paried data is normal (see ho13 pg 21)
#### NOTE: need to make sure there isnt any autocorrelation in the differences
#### NOTE: not sure why but we just do a wilcox test again 
# 
# ymean = mean(Diffs)
# yt = Diffs[2:length(Diffs)]
# ytlag1 = Diffs[1:length(Diffs)-1]
# 
# 
# 
# dif1 = (yt-ytlag1)^2
# num1 = sum(dif1)
# y2 = (y-ymean)^2
# den1 = sum(y2)
# Q = (num1/(n-1))/(den1/n)
# prd1 = (yt-ymean)*(ytlag1-ymean)
# prdsum1 = sum(prd1)
# rho1 = prdsum1/den1
# rho1
# Q
# 
# qpa = ((2*length(Diffs))/(length(Diffs)-1)) - qnorm(.95)*(2/sqrt(length(Diffs)))
# qna = ((2*length(Diffs))/(length(Diffs)-1)) + qnorm(.95)*(2/sqrt(length(Diffs)))
# c(qpa,qna)

#### Not sure about this test... just move on, ask about this later.


wilcox.test(data$Lab1, data$Lab2, alternative="two.sided", paired = F)

wilcoxon.rank.sum.test(data$Lab1, data$Lab2, alpha = 0.05,alternative = "ne")

# (e) Provide 95% confidence intervals on the average determinations for both labs.
# NOTE: see assignment 6 # 2(i) for code for bootstrap

n = length(data$Lab1)
m = length(data$Lab2)
muLab1 = mean(data$Lab1)
muLab2 = mean(data$Lab2)
v1 = var(data$Lab1)/n
v2 = var(data$Lab2)/m

muLab1_est = NULL
muLab2_est = NULL

N = 9999
for(i in 1:N){
  muLab1_est[i] = mean(sample(data$Lab1,replace = T))
  muLab2_est[i] = mean(sample(data$Lab2,replace = T))
}

z1 = sqrt(n)*(muLab1_est-muLab1)/muLab1_est
z2 = sqrt(m)*(muLab2_est-muLab2)/muLab2_est

z1 = sort(z1)
z2 = sort(z2)

Lz1 = z1[250]
Lz2 = z2[250]
Uz1 = z1[9750]
Uz2 = z2[9750]

mu1L = muLab1 - Uz1*sqrt(v1)
mu1U = muLab1 - Lz1*sqrt(v1)

mu2L = muLab2 - Uz2*sqrt(v2)
mu2U = muLab2 - Lz2*sqrt(v2)

CI95L1 = c(mu1L,mu1U)
CI95L2 = c(mu2L,mu2U)
CI95L1
CI95L2

### forget about doing studentized bootstrapping.. just use distribution free estimates

# 8.) A study was conducted to investigate whether there is a relationship between tonsil size and
#     carriers of a particular bacterium, Streptococcus pyrogenes. The following table contains the results from
#     1398 children


data = matrix(c(19,497,29,560,24,269),byrow = T, nrow = 3)
data

p_hat1. = sum(data[1,])/sum(data)
p_hat2. = sum(data[2,])/sum(data)
p_hat3. = sum(data[3,])/sum(data)

E_11 = (sum(data[1,])*sum(data[,1]))/sum(data)
E_21 = (sum(data[2,])*sum(data[,1]))/sum(data)
E_31 = (sum(data[3,])*sum(data[,1]))/sum(data)
E_12 = (sum(data[1,])*sum(data[,2]))/sum(data)
E_22 = (sum(data[2,])*sum(data[,2]))/sum(data)
E_32 = (sum(data[3,])*sum(data[,2]))/sum(data)

data = c(19,497,29,560,24,269)
Es = c(E_11,E_12,E_21,E_22,E_31,E_32)

Ts = sum(((data - Es)^2)/Es)
Ts

qchisq(.95,(3-1)*(2-1))




# 9.) The following table gives the racial characteristics of 326 individuals convicted of homicide and
#     whether or not they received the death penalty. Social scientists were interested in the relationship between
#     Defendant's Race and the probability that the defendant would receive the Death Penalty. A possible
#     confounding variable is the Race of the homicide victim



# (a) Test whether there is significant evidence that frequency of receiving the Death Penalty is related to
#     Defendant's Race ignoring the Victim's Race. Use ?? = .05.

M = matrix(c(19,141,17,149), nrow = 2, byrow = T)
fisher.test(M, alternative = "two.sided")

# (b) For each category of Victim's Race, test whether there is significant evidence that frequency of receiving
#     the Death Penalty is related to Defendant's Race. Use ?? = .05.

#### for white victims 
M = matrix(c(19,132,11,52), nrow = 2, byrow = T)
fisher.test(M,alternative = "two.sided")

#### for black victims
M = matrix(c(0,9,6,97), nrow = 2, byrow = T)
fisher.test(M,alternative = "two.sided")



############ MULTIPLE CHOICE ####################

# (7)
1-(1-pchisq(((2.5^2)/(2.775^2))*qchisq(1-.01,40-1),40-1))
power.t.test(n=40, delta = 2.775-2.5, sig.level = .01, sd = 2.775, type = c("one.sample"),alternative = c("one.sided"))


# (8)

n = 2*(0.4*(qnorm(.05)+qnorm(.20))/( 0.2))^2
n


