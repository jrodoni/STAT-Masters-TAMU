##################################### STAT 608 HW05 #####################################

# 3.) (e)

cars <- read.csv("C:/Users/jackr/OneDrive/Desktop/Graduate School Courses/STAT 608 - Regression Analysis/Data/cars04.csv")
attach(cars)

n = nrow(cars)

tSuggestedRetail = log(SuggestedRetailPrice)
tEngineSize = EngineSize^0.25
tCylinders = log(Cylinders)
tHorsepower = log(Horsepower)
tHighwayMPG = 1/HighwayMPG
tWheelBase = log(WheelBase)

mod.trans = lm(tSuggestedRetail ~ tEngineSize + tCylinders + tHorsepower + tHighwayMPG + Weight + tWheelBase + Hybrid)
summary(mod.trans)

RSSmod.trans = sum(resid(mod.trans)^2)

mod.red = lm(tSuggestedRetail ~ tEngineSize + tCylinders + tHorsepower + Weight + Hybrid)
summary(mod.red)

RSSmod.red = sum(resid(mod.red)^2)

PartialFstat = ((RSSmod.red-RSSmod.trans)/2)/(RSSmod.trans/(n-8))
PartialFstat

RSSmod.trans
RSSmod.red

pf(PartialFstat,2,n-8,lower.tail = F)
anova(mod.red,mod.trans)



#### just noticed that for the Full model I get a slightly different output than what is given in the text.
#### my residual standard error = .1789 and the books is .1724.  I get the same for the reduced model though

RSSFull = (0.1724^2)*226
RSSred = (0.1781^2)*228

PartialFstat = ((RSSred-RSSFull)/2)/(RSSFull/(n-8))
PartialFstat

pf(PartialFstat,2,n-8,lower.tail = F)



# 6.)
#   (a)

library(car)
data = data.frame(Y = seq(1:6), doseage = rep(seq(1:3),times = 2), Supplement = as.factor(rep(c("A","B"),each = 3)))
attach(data)

cor(doseage,as.numeric(Supplement))

rab.mod = lm(Y~doseage+Supplement)
vif(rab.mod)

detach(data)

#   (b)

data = data.frame(Y = (1:6), doseage = c(1:3,2:4), Supplement = as.factor(rep(c("A","B"),each = 3)))
attach(data)

rab.mod = lm(Y~doseage+Supplement)
vif(rab.mod)
cor(doseage,as.numeric(Supplement))
