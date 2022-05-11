
#load file of real estate data
bcs<-read.csv("C:\\Users\\kinchelf\\Documents\\TAIR Workshops\\bcs.csv", na.strings='*')
#Make columns available in search path
attach(bcs)

#Recreate scatter plot of home prices by square ft.
#Create a vector of plot characters
(pchv <- (Location=="College Station, TX")+1)

#Create an empty vector in which to put color names
(colv <- rep(NA,length(Sqft)))

#Put red in College Station positions
(colv[Location=="College Station, TX"] <- "red")

#Put blue in Bryan positions
colv[Location=="Bryan, TX"] <- "blue"; colv 
plot(Sqft,Price,pch=pchv, col=colv)

#draw a fit line 
abline(lm(Price~Sqft))

tm<-lm(Price~Sqft)

summary(tm)

##### Go to slide 

#put a legend on the graph
legend(2000,2000,c("Bryan","College Station"),pch=c(1,2),col=c("blue","red"))
legend(locator(1),c("Bryan","College Station"),pch=c(1,2),col=c("blue","red"))

##### Go to slide 

# Put text inside the plot
text(1550,2400,'This is the upper left corner', adj=0)

###### Go to slide  - Discuss Plotmath


# assign a mean value to mu
MU <- 8.25

# plot a couple of points to give us a working space
plot(c(1,10),c(10,1),
	xlab = expression(paste("Phase Angle ", phi)), col.lab = "blue",
	ylab = expression(mu== MU)) ## Why don't we get a number here?

# add text in top margin evaluating mu
mtext(bquote(mu==.(MU)))

# put a formula inside our plot
text(6, 6, expression(bar(x) == sum(frac(x[i], n), i==1, n)),col="#1C3C34" )

##### Go to Slide 

#Put blue in Bryan positions, red in other positions
rm(colv)
colv <- ifelse(Location=="Bryan, TX", "blue", "red") ; colv

col3 <- ifelse(Location=="Bryan, TX", "blue", ifelse(Location=="Waco, TX", "green", "red")) ; col3

##### Go to Slide
# Assign a small odd number
   V <- 6

	if(V%%2==1 & V<= 5) {
		cat(V, "is a small, odd number\n")
	}else{cat(V, "is either large and/or even\n")}

#for loop example
y <- 1:10
for(i in y){
    if(i > 6){
       cat("i=",i,"and is bigger than 6.\n")
    }else{
       cat("i=",i,"and is 6 or less.\n")
    }
}
## Does i live outside the loop?
i

##### Go to Slide 

#while loop examples
i <- 11 #Change this to 1 and compare results
while(i <=10) {
    if(i > 6){
       cat("i=",i,"and is bigger than 6.\n")
    }else{
       cat("i=",i,"and is 6 or less.\n")
    }
   i<-i+1
   cat("At the bottom of the loop i is now =",i,"\n")
}
i

### Go back and correct initial i value

# Why is final i value different in for vs. while?

###Using loop and IF to solve graphics question
	# x points where repeating values are not repeated
	x<-c(1,2,3,NA,NA,4,NA,NA,5,NA);x
	#random values for y
	y<-rnorm(length(x))
	#plot
	boxplot(y~x)
	#fix missing values and replot
	for(i in 1:length(x)){
		 if(is.na(x[i])) x[i] <- x[i-1]
		}
	x
	boxplot(y~x)

##### Go to Slide 
(Bryan <- which( Location=="Bryan, TX"))

Give me the rows that have the number of Baths equal max in Bryan
which(Baths==max(bcs[Location=="Bryan, TX",3]))
	
