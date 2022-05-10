##### Testing how to do fit overdistpersed model in R #######################
##### Assignment 05, # 3(b) #################################################
##### Testing on Hermit crab data from H.O 5, see pgs 41-49) ################

library(readxl)
CrabData <- read_excel("C:/Users/jackr/OneDrive/Desktop/Graduate School Courses/STAT 642 - Methods of STAT II/Course Notes/Week 6 (0220-0226)/CrabData.XLS")
CrabData
y = CrabData$Count
site = as.factor(CrabData$Site)


CrabData = data.frame(y,site)

mod_crab = glm(y~site, family = poisson)
summary(mod_crab)
summary(mod_crab)$deviance


mod_crab_overdisp = glm(y~site, family = quasipoisson)
summary(mod_crab_overdisp)

scaled.deviance = summary(mod_crab_overdisp)$deviance/summary(mod_crab_overdisp)$dispersion
scaled.deviance/summary(mod_crab_overdisp)$df.residual



#### Now lets try fitting the contrasts ####
CrabContr = matrix(c(-1,1,0,0,0,0,
                      0,1,-1,0,0,0,
                      0,1,0,-1,0,0,
                      0,1,0,0,-1,0,
                      0,1,0,0,0,-1),
                      nrow = 5,ncol = 6, byrow = TRUE)
rownames(CrabContr) = c("S2 v S1", "S2 v S3", "S2 v S4","S2 v S5","S2 v S6")

fit.contrast(mod_crab, "site",CrabContr)
##### This doesnt give the same results she got...####


model.matrix.default( ~ site, CrabData, 
                      contrasts.arg =list(site = contr.treatment(n = 6, base = 2)))



contr.treatment(n = 6, base = 2)

#### NOT what I want but interestng
summary(glm(y~site, family = quasipoisson, contrasts =  list(site = t(CrabContr))))
#### contrasts is the way to set up what parameters are estimated


# gonna calculate SSC by hand.  SSC/sigma^2 ~ X^2 with 1 df
CrabContr = matrix(c(-1,1,0,0,0,0,
                     0,1,-1,0,0,0,
                     0,1,0,-1,0,0,
                     0,1,0,0,-1,0,
                     0,1,0,0,0,-1),
                     nrow = 5,ncol = 6, byrow = TRUE)
rownames(CrabContr) = c("S2 v S1", "S2 v S3", "S2 v S4","S2 v S5","S2 v S6")

muHat = matrix(tapply(y, site, mean),nrow = 6)
x = table(site) # the diagonals of the below matrix
D = diag(x,length(x),length(x)) 
Dinv = solve(D)

SSH = NULL
Chisqstat = NULL

for(i in 1:nrow(CrabContr)){
  H = matrix(CrabContr[i,],ncol = 6)
  h = 0 
  A = H%*%muHat - h
  Cinv = solve(H%*%Dinv%*%t(H))
  SSH[i] = t(A)%*%Cinv%*% A
  Chisqstat[i] = (SSH[i]/(mod_crab_overdisp$deviance)
  
  
}
Chisqstat
##### also doesnt work.... 

dchisq(2.33,df=1)





