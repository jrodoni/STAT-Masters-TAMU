install.packages("lme4")
library(lme4)
mach = as.factor(c(rep("M1",5),rep("M2",5),rep("M3",5),rep("M4",5)))
strngt =
  c(128, 127, 129, 126, 128,
    121, 120, 123, 122, 125,
    126, 125, 127, 125, 124,
    125, 126, 129, 128, 127)
data <- data.frame(strngt,mach)
#treat Machine as a fixed effect to obtain Sum of Squares
fixmach = lm(strngt ~ mach)
anova(fixmach)

#treat Machine as a random effect to obtain estimates of variances
ranmach = lmer(strngt ~ 1+(1|mach),data)
summary(ranmach)

