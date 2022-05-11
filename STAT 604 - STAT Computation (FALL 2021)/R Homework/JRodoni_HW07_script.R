#File name:  JRodoni_HW07_script.R
#Path:  "C:/Users/jackr/OneDrive/Desktop/Graduate School Courses/STAT 604 - STAT Computation/Homeworks/JRodoni_HW07_script"
#Created by Jack Rodoni
#Creation Date:  10/10/2021
#Purpose: creating a function
#Last executed:  
Sys.time()

#housekeeping functions 
ls()
library()
search()

#1 load workspace 

load("C:/Users/jackr/OneDrive/Desktop/Graduate School Courses/STAT 604 - STAT Computation/RData/HW05.RData")
ls()
str(CovidTexas)

#2(a) creating a new data frame from Texas data
Bexar_data <- CovidTexas[which(CovidTexas$COUNTY_NAME == "Bexar"),c(2,3)]
str(Bexar_data)

#2(b) ordering data frame by date, finding first case
Bexar_data <- Bexar_data[order(Bexar_data$REPORT_DATE, decreasing = FALSE),] 
first_case <- match(TRUE, Bexar_data$NEW_CASES > 0)
first_case

#2(c) assigning a value for alpha component 
a <- 2/31

#2(d) creating a vector of zeros
ema <- rep.int(0,length(Bexar_data$NEW_CASES))

#2(e) averaging first 30 days
first_30 <- Bexar_data[1:30,]
ema[30] <- mean(first_30$NEW_CASES)
ema[30]

#2(f) using a loop to run ema formula 
count <- 0
for (i in Bexar_data$NEW_CASES[31:length(Bexar_data$NEW_CASES)]) {
  ema[31+count] <- i*a+ema[31+count-1]*(1-a)
  count <- count + 1
}

#2(g) creating a plot for new cases 
par(bg="grey90")
Bexar_data$REPORT_DATE<- as.Date(Bexar_data$REPORT_DATE)
plot(Bexar_data$REPORT_DATE[first_case:601],Bexar_data$NEW_CASES[first_case:601],
     type = "l",xlab = "Date",ylab = "New Cases",col = "blue",
     main = "Bexar County 30 Day EMA and Daily Cases")

#2(h) adding a red line for ema values 
lines(Bexar_data$REPORT_DATE,ema,col = "red")

#2(i) adding ema formula to graph
text(Bexar_data[23,], .95*max(Bexar_data$NEW_CASES), 
     expression(EMA['i'] == (P['i']%*%alpha)+(EMA['i-1']%*%(1-alpha))~"where"~
                alpha == frac(2,1+30)),
     adj = 0,cex = 0.8)

#3 removing all objects except the two data frames 
ls()
rm(a,Bexar_data,count,ema,first_30,first_case,i)
ls()

#4 using the function for texas covid data 
func <- function(county_name,n=30,df=CovidTexas){
  county_data <- CovidTexas[which(CovidTexas$COUNTY_NAME == county_name),c(2,3)]
  county_data <- county_data[order(county_data$REPORT_DATE, decreasing = FALSE),] 
  first_case <- match(TRUE, county_data$NEW_CASES > 0)
  a <- 2 / (1+n)
  l <- length(county_data$NEW_CASES)
  ema <- rep(0,times=l)
  ema[1:n] <- sum(county_data$NEW_CASES[c(1:n)])/n
  count <- 0
  for (i in county_data$NEW_CASES[n+1:l]) {
    ema[n+1+count] <- i*a+ema[n+count]*(1-a)
    count <- count + 1
  }
  
  par(bg="grey90")
  county_data$REPORT_DATE<- as.Date(county_data$REPORT_DATE)
  plot(county_data$REPORT_DATE[first_case:l],
       county_data$NEW_CASES[first_case:l],
       type = "l",xlab = "Date",ylab = "New Cases",col = "blue",
       main = paste(county_name,n, "Day EMA and Daily Cases")) 
  lines(county_data$REPORT_DATE[first_case:l],ema[first_case:l],col = "red")
  text(county_data[first_case,], .95*max(county_data$NEW_CASES), 
       expression(EMA['i'] == (P['i']%*%alpha)+(EMA['i-1']%*%(1-alpha))~"where"~
                    alpha == frac(2,1+30)),
       adj = 0,cex = 0.8)
  
}

#5 sending graphics to pdf file
pdf("C:/Users/jackr/OneDrive/Desktop/Graduate School Courses/STAT 604 - STAT Computation/Homeworks/JRodoni_HW07_graphics.pdf",11,8.5)

#6 setting up 2 rows for graphics 
par(mfrow=c(2,1))
par(mar=c(4,4,4,0))
par(omi=c(0,1,.5,0))

#7 calling the function twice
func("Bexar")
func("Bexar",7)
ls()

#8 writing system time in plot 
mtext(Sys.time(),1,adj = 0)

#9 creating a vector of random samples 
set.seed(20210911)
samp_data <- sample(Sep12[,1],2)

#10 creating a loop to call function 
for (i in samp_data) {
  func(i)
}

#12a 601

#12b as N increases, the peaks on red line become smoother

#12c we created another object in our workspace 

#12d Bexar had the most covid cases, followed by Brazos then Eastland

graphics.off()
