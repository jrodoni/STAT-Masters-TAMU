load("C:/Users/jackr/OneDrive/Desktop/Graduate School Courses/STAT 639 - Data Mining and Analysis/Data/class_data.RData")
y.fac = as.factor(y)

require(e1071)

x.red = x[,c(2,11,50,77,164,218,222,228,230,341,350,409)]

best.model = svm(x.red,y.fac, 
                 kernel = "radial",
                 cost = 8,
                 gamma = 0.08114262)


ynew = predict(best.model,xnew[,c(2,11,50,77,164,218,222,228,230,341,350,409)])
test_error = 0.097561
save(ynew, test_error, file = "2.RData")

