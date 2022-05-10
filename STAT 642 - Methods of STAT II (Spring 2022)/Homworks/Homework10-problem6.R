WS = c(37.5, 40.5, 49.0, 51, 61.5, 63,
       57.5, 69.5, 87, 92, 107, 119.5,
       38, 44.5, 53.0, 55.0, 58.5, 60)

Diameter = c(12.5,14.0,16.0,15.0,18,19.5,
             16.5,17.5,19,19.5,24,22.5,
             15.5,16,19,18,19,20.5)



data = data.frame(Alloy = factor(rep(c(1,2,3),each = 6)), Diameter, WS)


data$X_bar_i = rep(tapply(data$Diameter, data$Alloy, mean), each = 6)

X_bar = mean(Diameter)

X_bar_i


sum((data$Diameter-data$X_bar_i)^2)

X_bar1difXbar2sq = (X_bar_i[1]-X_bar_i[2])^2
X_bar1difXbar3sq = (X_bar_i[1]-X_bar_i[3])^2
X_bar2difXbar3sq = (X_bar_i[2]-X_bar_i[3])^2


MSE = 51.154571

SE12 = sqrt(MSE*((2/6)+(X_bar1difXbar2sq)/sum(((data$Diameter-data$X_bar_i)^2))))
SE13 = sqrt(MSE*((2/6)+(X_bar1difXbar3sq)/sum(((data$Diameter-data$X_bar_i)^2))))
SE23 = sqrt(MSE*((2/6)+(X_bar2difXbar3sq)/sum(((data$Diameter-data$X_bar_i)^2))))

SE12
SE13

subset(data, data$Alloy == 1)

lm1 = lm(WS~Diameter, data = subset(data, data$Alloy == 1))
lm2 = lm(WS~Diameter, data = subset(data, data$Alloy == 2))
lm3 = lm(WS~Diameter, data = subset(data, data$Alloy == 3))


plot(data$Diameter, data$WS)
abline(lm1)
abline(lm2)
abline(lm3)






