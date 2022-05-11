
#load file of real estate data
#bcs<-read.csv("C:\\Users\\kinchelf\\Documents\\TAIR Workshops\\bcs.csv", na.strings='*')
bcs <- read.csv("C:/Users/jackr/OneDrive/Desktop/Graduate School Courses/STAT 604 - STAT Computation/RData/bcs.csv")

# Use R to view contents of data frame
View(bcs)

#Make columns available in search path
attach(bcs)

#### Go to slide 

#produce a bar plot of the number of baths
barplot(sort(table(Baths),decreasing=TRUE))
barplot(sort(table(Baths),decreasing=TRUE),horiz=TRUE)


#Enhance barplot to show both locations
(barmat<-table(Baths,Location))
class(barmat)
barplot(barmat,beside=TRUE)

#### Go to slide 
#Create crosstab using tapply
tapply(bcs$Price, bcs[ ,3:4], mean)

#### Go to slide 
#show the same data as first bar in a pie chart
pie(sort(table(Baths),decreasing=TRUE))
pie(sort(table(Baths),decreasing=TRUE),clockwise=TRUE)

#### Go to slide 

#Create a histogram of home square footage
# plot histogram
hist(Sqft,freq=FALSE) #default breaks  

# establish a vector of break values
	brv<-seq(1000,5000,400)

hist(Sqft,freq=FALSE, breaks=brv) #fixed breaks
hist(Sqft,freq=FALSE, breaks=16) #single number of breaks
hist(Sqft,freq=FALSE, breaks=5) 	#different number of breaks
hist(Sqft,freq=FALSE, breaks="FD")  # break algorithm

#### Go to slide 
#Create a boxplot of home prices by location
boxplot(Price ~ Location)

# Extend max to include outliers
boxplot(Price ~ Location, range=0)

# Add notches
boxplot(Price ~ Location, range=0, notch=TRUE)

#### Go to slide 

# Rerun graphics functions to be all in one image
# 2X2 grid populated by Rows
par(mfrow=c(2,2))
barplot(sort(table(Baths),decreasing=T))
pie(sort(table(Baths),decreasing=T))
hist(Sqft,freq=F) #default breaks
boxplot(Price ~ Location)
mtext("This is the first group")

#Do it again by columns (down then across)
par(mfcol=c(2,2))
barplot(sort(table(Baths),decreasing=T))
pie(sort(table(Baths),decreasing=T))
hist(Sqft,freq=F) #default breaks
boxplot(Price ~ Location)
mtext("This is the second group")

##### Go to slide 

# Rerun graphics functions to be all in one image


par(mfrow=c(2,2))

barplot(sort(table(Baths),decreasing=TRUE), main="#1 - Bar")
pie(sort(table(Baths),decreasing=TRUE), main="#2 - Pie")
hist(Sqft,freq=FALSE, main="#3 - Histogram") 
boxplot(Price ~ Location, main="#4 - Boxplot")



#Do it again by columns (down then across)
par(mfcol=c(2,2))
barplot(sort(table(Baths),decreasing=TRUE), main="#1 - Bar", ylim=c(10,0))

par(adj=0)
pie(sort(table(Baths),decreasing=TRUE), main="#2 - Pie")

par(adj=1)
hist(Sqft,freq=FALSE, main="#3 - Histogram", xlab='Square Footage')
 
boxplot(Price ~ Location, main="#4 - Boxplot", names=c("Bryan","Coll. Sta."))

# Plot the histogram again
hist(Sqft,freq=FALSE) #default breaks

# Add the normal distribution
  #create a vector of "X" values
	Xd<-seq(1000,5000,1)
  #create vector of densities for our data
	Yd<-dnorm(Xd,mean=mean(Sqft),sd=sd(Sqft)) 
  #plot the distribution
	lines(Xd,Yd)


##### Go to slide

  # replace the distribution with a polygon
	polygon(Xd, Yd)
	polygon(Xd, Yd, col='blue')

	
