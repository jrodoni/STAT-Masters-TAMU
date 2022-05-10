############################ STAT 642 HW 05 (3/10/22) ############################
library(lsmeans)
# library(dplyr)
library(MASS)
library(gmodels)

stdOptions = options()
options(stdOptions)

# 1.) An accelerated (temperature stressed) life test was performed on HD televisions to assist the
# manufacturer in determining an appropriate warranty. Six TV's were tested at each of four temperatures:
# 40???C, 45???C, 55???C, 70???C. The time to failure in hours are given in the following table for the 24 television
# sets.


deg40 = c(1953, 2135, 2471, 4727, 6134, 6314)
deg45 = c(1190, 1286, 1550, 2125, 2557, 2845)
deg55 = c(651, 817, 848, 1038, 1361, 1543)
deg70 = c(511, 651, 651, 652, 688, 729)
temp_data = data.frame(deg40,deg45,deg55,deg70)


#### (a) Do the conditions of normality, equal variance, and independence of the data values hold for the data
#        from the four temperatures? Justify your answers with plots, tests, and logical reasoning.

##################### calculating the LSE means multiple ways ##################### 
mu40 = mean(deg40)
mu45 = mean(deg45)
mu55 = mean(deg55)
mu70 = mean(deg70)
# using apply we can get the same thing
means = apply(temp_data,2,mean)

# alternatively we could do
temps = as.factor(rep(c(40,45,55,70),each=6))
temp_data_vect = unlist(temp_data,use.names = FALSE) # creates vector out of the temp data
tempMod = lm(temp_data_vect~temps)
lsmeans(tempMod, ~temps)
summary(aov(tempMod))

##################################################################################

###### (i) Testing Normality: **H.O. 5 pg 6**
resids = c(deg40-mu40, deg45-mu45, deg55-mu55, deg70-mu70)
# or
resids1 = tempMod$residuals

shapiro.test(resids)
qqnorm(resids)
qqline(resids)


###### (ii) Testing equal variance: **H.O. 5 pg 10-18**

# brown-forsythe-levene Test (ni >= 3)
medians = apply(temp_data,2,median)
zs = abs(c(temp_data$deg40-medians[1],
           temp_data$deg45-medians[2],
           temp_data$deg55-medians[3], 
           temp_data$deg70-medians[4]))

bfl1 = aov(zs~temps)
summary(bfl1)

boxplot(temp_data)
par(mfrow = c(2,2))
plot(tempMod)
par(mfrow = c(1,1))

###### (iii) Testing for Correlation
resids
residsL0 = resids[2:length(resids)]
residsL1 = resids[1:length(resids)-1]
residTime1 = ts(resids,start = 1,frequency = 1)

## Graphical Tests
plot(residTime1,type = "b",ylab = "res_raw",main = "Resid_Raw vs Order")
abline(h=0,lty = 2)
plot(residsL1,residsL0)


## Calculation of Durbin-Watson Statistic
e2 = residsL0^2
e2L1 = residsL1^2
prod1 = residsL0*residsL1

DW = (sum(e2)+sum(e2L1)-2*sum(prod1))/sum(resids^2)
DW

# using tables we get 
dl.05 = 1.01
du.05 = 1.78

dl.01 = 0.80
du.01 = 1.53

##### much easier and we get the same test statistic #####
library(car)
durbinWatsonTest(tempMod)
##########################################################



#### (b)  Determine a reasonable transformation of the data using the slope of the regression line based on
####      log(Si) vs log(¯yi.).

sigmas = c(sd(deg40),sd(deg45),sd(deg55),sd(deg70))

lmtrans = lm(log(sigmas) ~ log(means))
summary(lmtrans)
plot(log(means),log(sigmas))
abline(lmtrans)

confint(lmtrans)
1.7295-qt(0.975,2)*0.2768




#### (c) Use the Box-Cox Technique for selecting a transformation of the data. Is the transformation from
####     Box-Cox procedure consistent with your transformation from part b.?

# should be -0.635

liklihood = boxcox(temp_data_vect+1~temps, lambda = seq(-3,3,0.01))
liklihood_max = max(liklihood$y)
imax = which(liklihood$y == liklihood_max)
thmax = liklihood$x[imax]
thmax


#### (d) Using the transformation from part c., is the transformed data appropriate for conducting an AOV?

temp_data_new = temp_data_vect^thmax
tempModNew = lm(temp_data_new~temps)

residsNew = tempModNew$residuals
###### Normality:
shapiro.test(residsNew)

###### Equal Variance:
Temp_Data_New = matrix(temp_data_new, ncol = 4)

medians = apply(Temp_Data_New,2,median)
zs = abs(c(Temp_Data_New[,1]-medians[1],
           Temp_Data_New[,2]-medians[2],
           Temp_Data_New[,3]-medians[3], 
           Temp_Data_New[,4]-medians[4]))

bfl2 = aov(zs~temps)
summary(bfl2)

###### Independence:
residsNewL0 = residsNew[2:length(residsNew)]
residsNewL1 = residsNew[1:length(residsNew)-1]
residTime1New = ts(residsNew,start = 1,frequency = 1)

eNew2 = residsNewL0^2
e2NewL1 = residsNewL1^2
prod1New = residsNewL0*residsNewL1

DW = (sum(eNew2)+sum(e2NewL1)-2*sum(prod1New))/sum(residsNew^2)
DW



#### (e) Perform an AOV on both the original data and the transformed data. Compare the results from the
####     two analyses.

modelo.g. = aov(temp_data_vect~temps)
summary(modelo.g.)

modelNew = aov(temp_data_new~temps)
summary(modelNew)


#### (f)  Use Tukey's HSD to group the four temperatures relative to the mean time to failure.


Compare = TukeyHSD(modelo.g.,"temps",ordered=F,.95)
plot(Compare, las = 1)


CompareNew = TukeyHSD(modelNew,"temps",ordered=F,.95)
plot(CompareNew, las = 1)
CompareNew


#### (g) Test for a trend in the time to failures as a function of Temperature? Because the Temperatures were
####     unequally spaced, the following contrast coefficients were obtained from R. The coefficients for the
####     three contrasts, Linear, Quadratic, and Cubic are given below:
  
contrasts = contr.poly(4,scores = c(40,45,55,70))
con_std = matrix(0,4,3)
m = c(rep(0,3))
for (i in 1:3){
  m[i] = min(abs(contrasts[,i]))
  con_std[,i] = contrasts[,i]/m[i]
  }
con_std
con_std[,2] = 3*con_std[,2]
con_std


fit.contrast(tempModNew, "temps", t(con_std))
fit.contrast(tempMod,"temps",t(con_std))
################################################################################################################
################################################################################################################


# 2.) For the data time to failure data in Problem 1.,

#### (a)  Use a rank based test to compare the average time to failure for the four temperatures

KW = kruskal.test(temp_data_vect,temps,temp_data_vect~temps)
KW

#### (b) Use a rank based multiple comparison procedure to group the four temperatures relative to the average
####     time to failure.

temp.label = rep(c(40,45,55,70),each = 6)
data.ranks = data.frame(Response = temp_data_vect, Treatment = temp.label)


data.ranks = data.ranks[order(data.ranks$Response),]  
data.ranks$Rank = 1:length(data.ranks$Response)


###### the follwoing loop is to deal with ties ######
for(i in 1:length(data.ranks$Rank)){
  
  same.ranks = which(data.ranks$Response == data.ranks$Response[i])
  
  if(length(unique(data.ranks$Rank[same.ranks])) > 1){
    
    data.ranks$Rank[same.ranks] = sum(data.ranks$Rank[same.ranks])/length(same.ranks)
  
  }
}


###### the following loop is to calculate the R_bars ######
t = 4
R_bar = matrix(0,nrow = 4,ncol = 2)

for(i in 1:t){
  data_temp = data.ranks[which(data.ranks$Treatment == unique(data.ranks$Treatment)[i]),]
  R_bar[i,] = c(unique(data.ranks$Treatment)[i],mean(data_temp$Rank))
}
R_bar = R_bar[order(R_bar[,1]),]
R_bar

R_bars = c(R_bar[1,2]-R_bar[c(2:4),2],
           R_bar[2,2]-R_bar[c(3,4),2],
           R_bar[3,2]-R_bar[4,2])
names(R_bars) = c("R1-R2","R1-R3","R1-R4","R2-R3","R2-R4","R3-R4")

###### Hollander Wolfe Procedure #####
critvalHW = sqrt(7.453*(2*4*25/12))
critvalHW

HollanderWolfeProc = data.frame(R_bars,critvalHW,`R_bars > critval` = R_bars>critvalHW, check.names = FALSE)
HollanderWolfeProc
######################################

###### Miller Procedure ######
critvalMiller = qtukey(0.95,4,1000000)*sqrt(4*(25)/12)
data.frame(R_bars,critvalMiller,`R_bars > critval` = R_bars>critvalMiller, check.names = FALSE)

#####################################################################################################################
#####################################################################################################################

# 3.) An entomologist counted the number of eggs laid by female moths on successive days in
#     three strains of tobacco budworm (USDA, Field, Resistant) from each of 15 matings. The entomologist was
#     interested in evaluating whether the average number of eggs was different for the three strains. The number
#     of eggs laid on the third day after the mating for each female is given in the following table:


USDA = c(448, 906, 28, 277, 634, 48, 369, 137, 29, 522, 319, 242, 261, 566, 734)
FIELD = c(211, 276, 415, 787, 18, 118, 1, 151, 0, 253, 61, 0, 275, 0, 153)
RESIST = c(0, 9, 143, 1, 26, 127, 161, 294, 0, 348, 0, 14, 21, 0, 218)

mothdata = data.frame(USDA,FIELD,RESIST)
mothdata_vect = unlist(mothdata, use.names = FALSE)
strain = as.factor(rep(c("USDA","FIELD","RESIST"),each = nrow(mothdata)))



means_vars = rbind(apply(mothdata, 2, mean),(apply(mothdata,2,sd))^2)
means_vars


#### (b)

mod = glm(mothdata_vect~strain, family = poisson)
summary(mod)




mod_overdisp = glm(mothdata_vect~strain, family = quasipoisson)
summary(mod_overdisp)

scaled.deviance = summary(mod_overdisp)$deviance/summary(mod_overdisp)$dispersion
scaled.deviance/summary(mod_overdisp)$df.residual

a= aov(mod_overdisp)

TukeyHSD(a,"strain")

lsmStrain = lsmeans(a,~strain)
summary(contrast(lsmStrain),method = "tukey", adjust = "mvt")


a2 = aov(mod)
TukeyHSD(a2,"strain")


#### (c) 
mothdata_vect
mothdataM

Centered = matrix(0,3,15)
for(i in 1:3){
  Centered[i,] = mothdataM[i,] - mean(mothdataM[i,])
}

n.neg = NULL
n.pos = NULL

for(i in 1:3){
  n.neg[i] = sum(Centered[i,]<0)
  n.pos[i] = sum(Centered[i,]>0)
}

numb.runs = rep(1,3)
for(i in 1:3){
  for(j in 2:15){
    if( sign(Centered[i,j]) != sign(Centered[i,j-1]) ){
      numb.runs[i] = numb.runs[i]+1
    }
  }
}

runs.results = data.frame(numb.runs,n.pos,n.neg)
names(Sruns.result) = c("No. runs", "N+", "N-")
