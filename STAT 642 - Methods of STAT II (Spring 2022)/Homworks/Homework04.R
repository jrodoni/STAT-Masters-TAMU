#################### STAT 642 HW04 ####################
rm(list = ls())
#install.packages("lsmeans")
#install.packages("ggplot2")
#install.packages("gmodels")
library(lsmeans)
library("gmodels")
library(MASS) # MASS = Modern Applied Statistics with S

# The following study will be used in answering questions 1 and 2:
  # An experiment was conducted to investigate heat loss for a coating placed on commercial thermal panes.
  # There were five thicknesses of the coating applied to the panes: 0 (Control), 20, 40, 60, and 80 units.
  # Ten panes were randomly assigned to each of the thicknesses. The interior temperature during the test
  # period was set at 70???F and the average heat loss through the pane was recorded over a range of exterior
  # temperatures. The heat loss data is given in the following table:


#### **For setup of data, see Hw and see HO4 pg 14**

HLdata = c(10.2, 10.8, 10.1, 10.9, 11.1, 11.8, 11.3, 11.9, 9.3, 9.9,
           9.2, 9.8, 9.1, 9.9, 10.1, 10.8, 10.3, 10.9, 9.3, 9.8,
           9.0, 9.9, 9.2, 9.8, 10.0, 10.8, 10.2, 10.7, 9.9, 9.0,
           8.1, 8.1, 8.0, 8.9, 8.2, 8.9, 8.1, 8.8, 9.2, 9.9,
           7.2, 7.8, 7.1, 7.9, 8.1, 8.8, 8.3, 8.9, 9.3, 9.8)

Thickness = as.factor(rep(c(0,20,40,60,80),each=10))
t = length(levels(Thickness))

# 1.) Use the data from the heat loss study to answer the following questions. Contrasts of
#     interest to the researchers were
#       C1: Control vs Average of the means of the four Thickness levels
#       C2: Linear trend across the 5 Thickness Levels
#       C3: Quadratic trend across the 5 Thickness Levels
#       C4: Cubic trend across the 5 Thickness Levels







# (b) Provide an estimate of each contrast along with the standard error of the estimator.
# ***** Not using dunnetts procedure for C1... dunnetts procedure is to compare EACH treatment mean to control*****
# ***** See H.O 4, pg 37 for info on dunnetts procedure *****
# ***** for fitting contrasts, see H.O 4, pg 14 *****

ThickModel = lm(HLdata ~ Thickness)
summary(ThickModel)
AOV = aov(ThickModel)
summary(AOV)
lsmeans(ThickModel,~Thickness) # note, ~Thickness is the same as "Thickness"


#### NOTE: The contrast matrices must be of full row rank
#### => make two different contrast matricies (C2,C2,C4) are independent, C1 is dependent
ThickContr1 = matrix(c(-2,-1,0,1,2,
                     2,-1,-2,-1,2,
                     -1,2,0,-2,1),3,t, byrow = TRUE)
rownames(ThickContr1) = c("Linear", "Quadratic", "Cubic")

ThickContr2 = matrix(c(4,-1,-1,-1,-1),1,t,byrow = TRUE)
rownames(ThickContr2) = "Control vs Average of Others"
### ask about the order of the rows in the matricies
### ask about doing 2 matricies... why doesnt this change our estimates?


C2_4 = fit.contrast(ThickModel, "Thickness", ThickContr1)
C1   = fit.contrast(ThickModel, "Thickness", ThickContr2)

rbind(C1,C2_4)

# (c) Use the Scheffe test at the ?? = .05 level of significance to test the significance of the four contrasts.
# ****** See H.O 4, pg 24******

ThickContr = rbind(ThickContr2,ThickContr1)
ni = 10; t = 5; n = t*ni

sigma_eps = sqrt(summary(AOV)[[1]][["Mean Sq"]][2])

Dh = data.frame(Dh = sqrt(colSums((t(ThickContr)^2)/ni)))
Dh

Sh = Dh*sigma_eps*sqrt((t-1)*qf(0.95,t-1,n-t))
# **** ask why we do qf(0.95,df1,df2) instead of qf(0.05,df1,df2)****
# **** see h.o 4, pg 24 ****


C_hats = c(C1[,1],C2_4[,1])
names(C_hats) <- NULL
SCIS = cbind(C_hats-Sh,C_hats+Sh)
SCIS
scheffe = data.frame(abs(C_hats), Sh, SCI_LB = SCIS[,1], SCI_UB = SCIS[,2], Significant = Sh<abs(C_hats))
names(scheffe)[c(1,2,5)] = c("|C_hat|","Sh","Significant")
scheffe


# We can see in the above output that C1 and C2 are significant contrasts


####### This plot is just for fun ############
Thickness2 =  rep(c(0,20,40,60,80),each=10)
ThickModel2 = lm(HLdata ~ Thickness2)
plot(Thickness2, HLdata)
abline(ThickModel2)
##############################################



# (d) Use a Bonferroni test with alphaE = .05 level of significance to test the significance of the four contrasts.
# ****** See H.O 4 pg 23 ******

M = 4
pvals = c(C1[,4],C2_4[,4])
names(pvals) = c("Control Vs Avg. Others", "Linear", "Quadratic", "Cubic")
af = .05/M
data.frame(pvals,af, `pvals<af` = pvals<af, check.names = F)
# We can conduct the Bonferroni comparisons y comparing the p-values from the four t-tests listed in the 
# R output with the familywise error rate af = 0.05/4 = 0.0125


# (e) Test the three trend contrasts simultaneously using the matrix approach from HO 4.
# ****** See H.O 4 pg 58 ******

x = as.numeric(Thickness)
polymodel = lm(HLdata~x+I(x^2)+I(x^3))
anova(polymodel)
summary(polymodel)


H = matrix(c(-2,-1,0,1,2,
              2,-1,-2,-1,2,
              -1,2,0,-2,1),3,5, byrow = TRUE)
muHat = matrix(tapply(HLdata,Thickness,mean),nrow = t) 
# **** tapply() here give us the same as the lsmeans(Thickmodel,"Thickness") used in (b) just without the extra stuff
# **** i.e. just returns the lsmeans
h = matrix(rep(0,nrow(H)),nrow = nrow(H)) # vector of zeros in this case... in general equal to the hypothesized 
                                          # value of H*muHat
x = table(Thickness) # the diagonals of the below matrix
D = diag(x,t,t) #(X^trans * X)

Dinv = solve(D)
A = H%*%muHat - h
Cinv = solve(H%*%Dinv%*%t(H))
SSH = t(A)%*%Cinv%*% A
SSH

Fstat = (SSH/3)/anova(polymodel)$`Mean Sq`[length(anova(polymodel)$`Mean Sq`)] # the last part is just MSE
pf(Fstat,3,46,lower.tail = FALSE)
1-pf(Fstat,3,46)


# ***** MSE used was wrong ***** 
# ***** Should've used the following *****
MSE = anova(ThickModel)$`Mean Sq`[length(anova(ThickModel)$`Mean Sq`)]
# ***** I.e. MSE is the MSE of the model with all our factors (Model from 1b)
# ***** Not the MSE of the model of orthogonal polynomials where the orthogonal polynomials are the 
# ***** contrasts we are testing to see if they are different *****
# ***** Fstat Should be the following 

Fstat = SSH/(3*MSE)
pf(Fstat,3,45,lower.tail = FALSE)
1-pf(Fstat,3,45)
# ***** also, the df should be 3,45 not 3,46



# (f) Is there a trend in the mean heat losses as a function of the coasting thickness? Justify your answer
# ****** See H.O 4 pg 59-62 ******


x = as.numeric(Thickness)
x2 = rep(c(0,20,40,60,80),each = 10)

x=Thickness

x
cubicmodel = lm(HLdata~x+I(x^2)+I(x^3))
anova(cubicmodel)
summary(cubicmodel)



quadmodel = lm(HLdata~x+I(x^2))
anova(quadmodel)
summary(quadmodel)

linmodel = lm(HLdata~x)
anova(linmodel)


# Yes, there seems to be a linear trend in the mean heat losses as a function of coating thickness.




# 2.) Use the data from the heat loss study to answer the following questions

# (a) Which thickness(es) have the smallest mean heat loss with a probability of correct selection of 0.95.
# ******See Hsu's procedure H.O 4 pg 39******
# ******See Brand-Hsu pdf for the code for function used below*****

mcb  <-  function(model,  best  =  "smallest",  alpha  =  .05){
    library(mvtnorm)
  if ("aov" %in% class(model) | "lm" %in% class(model)) {
    y <- model$model[,1]
    trt <- model$model[,2]
    dfMSE <- df.residual(model)
    MSE <- deviance(model)/dfMSE
  }
  data <- subset(data.frame(y, trt), is.na(y) == FALSE)
  means <- tapply(data[, 1], data[, 2], mean)
  ni <- tapply(data[, 1], data[, 2], length)
  N <- sum(ni)
  ntr <- length(ni)
  dcv <- qmvt(p=1-alpha, tail = "lower.tail", df = N-ntr, corr =
              matrix(rep(.5,(ntr-1)^2),ntr-1) + diag(ntr-1) * .5)$quantile
  if (best == "smallest") {
    min <- min(means)
    nmin <- min(means[means!=min(means)])
    m <- replace(rep(min, ntr), which(means==min, arr.ind=TRUE), nmin)
    k <- m + dcv*sqrt(MSE)*sqrt(2/ni[1])
    b <- (means < k)
    output <- data.frame(round(means, 3), round(m, 3), round(k, 3), b)
    names(output) <- c("means", "m", "k", "Best")
  }
  if (best == "largest") {
    max <- max(means)
    nmax <- max(means[means!=max(means)])
    m <- replace(rep(max, ntr), which(means==max, arr.ind=TRUE), nmax)
    k <- m - dcv*sqrt(MSE)*sqrt(2/ni[1])
    b <- (means > k)
    output <- data.frame(round(means, 3), round(m, 3), round(k, 3), b)
    names(output) <- c("means", "M", "k", "Best")
  }
  cat("Results for Hsu's procedure when 'best' is", best, "\n\n")
  print(output)
}
mcb(ThickModel)

# The two largest thicknesses (60,80) have the smallest mean heat loss

# (b) Do any of the coatings have a mean heat loss less than the mean heat loss for the panes with no
#     coating? Use ?? = .05 in your answer
# ***** See Dunnett's procedure H.O 4, pg 37 *****

ThickModel = lm(HLdata ~ Thickness)
summary(ThickModel)
AOV = aov(ThickModel)
summary(AOV)
lsmeans(ThickModel,~Thickness) # note: lsmeans(ThickModel,~Thickness) = lsmeans(AOV, ~Thickness)

lsmThick = lsmeans(AOV, ~Thickness)
summary(contrast(lsmThick, method="trt.vs.ctrl", adjust = "mvt", ref=1),
        infer=c(T,T), level = 0.95, side = "<")
# **** NOTE: we dont need to divide the p-values by M... in this case, we do have 
# **** All the thicknesses have signicicant evidence that their mean heat loss is less than the control
# ***** Question: why do we not use a familywise error rate?????
# ***** Answer: See H.0.4 pg 38 (5)

# c. Which pairs of the 5 treatment means are different using the Tukey procedure at the .05 significance
#    level.

Compare = TukeyHSD(AOV,"Thickness",ordered=FALSE,.95)
plot(Compare, las = 1)


# 5.)

# (c) Using the FDR method to adjust for multiple testing which SNPs have significant effects? Use a FDR
#     of 0.05.
# ***** See FDR.R file on canvas******
# ***** See H.O 4, pg 47 ******


data = data.frame(SNP = seq(1,10), PValues = c(0.0001,0.0058,0.0132,0.0289,0.0498,
                                               0.0911,0.2012,0.5718,0.8912,0.9011))
data = data[order(data$PValues),]
data$Rank = seq(1:nrow(data))

qi = nrow(data)*data$PValues/data$Rank
FDR = NULL
for(i in 1:nrow(data)){
  FDR[i] = min(qi[i:nrow(data)])
}
FDR

FDRL = FDR[max(which(FDR < 0.05))]
FDRL


