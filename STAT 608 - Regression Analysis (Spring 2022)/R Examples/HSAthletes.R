

athletes <- read.csv("/Users/home/Documents/TAMU/stat608/Data/high_school_female_athletes.csv")

BP_60 <- athletes$BRTF..60.
MaxBench <- athletes$X1RM.BENCH..lbs.

par(mar=c(5,5,1,1))
plot(BP_60, MaxBench, pch=19)


athletes.lm <- lm(MaxBench ~ BP_60 )
summary(athletes.lm)


#Prediction for existing data set:
predict(mathletes.lm, interval="confidence")
predict(athletes.lm, interval="prediction")

new<-data.frame(BP_60=c(25, 40))
#Moral: the names of the predictor variables in the new data set must match the names of the variables in the original data set.

#Then I'll make the intervals for only those two points:
predict(athletes.lm, new, interval="prediction")
predict(athletes.lm, new, interval="confidence")



