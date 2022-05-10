library(readxl)
CrabData <- read_excel("C:/Users/jackr/OneDrive/Desktop/Graduate School Courses/STAT 642 - Methods of STAT II/Course Notes/Week 6 (0220-0226)/CrabData.XLS")
CrabData
y = CrabData$Count
site = CrabData$Site
anal1 = aov(y~site)
rs1 = resid(anal1,type = "response")
rstime1 = ts(rs1,start = 1,frequency = 1)
rsraw = rs1[2:149]
rsrawl1 = rs1[1:148]

plot(rstime1,type = "b",ylab = "res_raw",main = "Resid_Raw vs Order")
abline(h=0,lty=2)
# If residuals are uncorrelated we should see random variation around y=0
plot(rsraw,rsrawl1,main = "Resid_Raw Lag Plot")
# not sure about this plot, I'm pretty sure if uncorrelated we would see random scatter 
x = rnorm(150)
rsl1 = x[2:150]
rs = x[1:149]
plot(rs,rsl1)
# yes, so for uncorrelated residuals we should see random scatter

acf(rstime1)
# we have an ar(1) structure in the residuals


#### Calculation of Durbin-Watson Statistic
dif1 = (rsraw-rsrawl1)^2
num1 = sum(dif1)
rs12 = rs1^2
den1 = sum(rs12)
DW1 = num1/den1
prd1 = rsraw*rsrawl1
prdsum1 = sum(prd1)
rho1 = prdsum1/den1

rho1
DW1

durbinWatsonTest(anal1)
# note that the professors notes are wrong
