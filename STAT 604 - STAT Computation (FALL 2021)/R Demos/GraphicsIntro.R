## Example of na.strings to Read Corruption data
(cpina<-read.csv("C:\\Users\\kinchelf\\Documents\\STAT604-FA20\\Lecture Slides\\R\\CorruptionPerceptionIndexNA.csv",
	na.strings=c('?','*','n/a')))

## show a basic plot of y2002 by y2011
plot(cpina$y2002,cpina$y2011)

#Where do the other parameters (defaults) come from?
par()

### Go to slide 

# Demo of graphics parameters
plot(cpina$y2002,cpina$y2011,type='l', col=6)
plot(2002:2011,cpina[2,11:2],type='l')

# Show colors
colors()
palette()

#plot palette
plot(1:8,1:8,pch=19,col=palette(), cex=4)

# change palette
palette(c('red','orange','yellow','green','blue','#4b0082', 'violet'))

# Change the plot character and colors
plot(2002:2011,cpina[2,11:2],type='b',pch=7, col=4)

#load file of real estate data
(bcs<-read.csv("C:\\Users\\kinchelf\\Documents\\TAIR Workshops\\bcs.csv", na.strings='*'))

plot(bcs$Sqft,bcs$Price)

#Make columns available in search path
attach(bcs)

#Create a vector of plot characters
(pchv <- (Location=="College Station, TX")+1)

#Create an empty vector in which to put color names
(colv <- rep(NA,length(Sqft)))

#Put red in College Station positions
(colv[Location=="College Station, TX"] <- "red")

#Put blue in Bryan positions
colv[Location=="Bryan, TX"] <- "blue"; colv 

#plot home prices by square ft.
plot(Sqft,Price,pch=pchv, col=colv)


