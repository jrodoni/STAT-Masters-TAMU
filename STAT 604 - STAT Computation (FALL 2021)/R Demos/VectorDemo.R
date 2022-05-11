#create a vector of country names
countries<- c('New Zealand', 'Denmark',
'Finland',
'Sweden',
'Singapore',
'Norway',
'Netherlands',
'Australia',
'Switzerland',
'Canada',
'Luxembourg',
'Hong Kong',
'Iceland',
'Germany')

#display our new vector
countries

#display the 7th member
countries[7]

#display all but the 7th member
countries[-7]

#display the first 3 members
countries[1:3]

#display information about the vector
summary(countries)

#add Japan as the 15th member and display
countries[15]<-'japan'
countries

#correct Japan and display
(countries[15]<-'Japan')

#append Austria to the end of the vector and display
(countries<-c(countries,'Austria'))

#add Ireland as the 20th member and display
countries[20]<-'Ireland'
countries

#Stop and look at NA slide

#Remove the last 4 entries
countries[-(17:20)]

#Get rid of them for real this time
(countries<-countries[-(17:20)])
summary(countries)

#Stop and look at next PPT slide

#Create a new vector with our Corruption Perception Index values
CPI<-c(9.5, 9.4, 9.4, 9.3, 9.2, 9, 8.9, 
8.8, 8.8, 8.7, 8.5, 8.4, 8.3, 8, 8, 7.8)

#get information about CPI vector
summary(CPI)
length(CPI)
mode(CPI)
class(CPI)

#show CPI values greater than 8 and less than 9
CPI>8 & CPI<9

#Use this expression as mask of the index vector to actually show the CPI values
CPI[CPI>8 & CPI<9]

#  Since we know that our two vectors line up with each other 
#  we can use the same index expression to show the countries
#  that meet this criteria
countries[CPI>8 & CPI<9]

#  How many countries meet this criteria?
sum(CPI>8 & CPI<9) #Why does this work?

##### Stop and view slide 15

#combine the two vectors into one object
(cpicb<-cbind(countries,CPI)) 
str(cpicb) 
summary(cpicb)
class(cpicb)
mode(cpicb)

# compare with rbind
(cpirb<-rbind(countries,CPI)) 
str(cpirb) 

##### Stop and view slide 16

#Try using data.frame to combine the vectors 
(cpidf<-data.frame(Country=countries,CPI))
str(cpidf) 
summary(cpidf)
class(cpidf)
mode(cpidf)

# Create a short vector of CPI values
(CPIshrt <- CPI[1:8])

# Combine short vector with countries
(cpishrt<-data.frame(Country=countries,CPIshrt))
(CBshrt<-cbind(countries,CPIshrt))

# Create a short vector of CPI values
(CPIshrtr <- CPI[1:7])

# Combine short vector with countries
(cpishrtr<-data.frame(Country=countries,CPIshrtr))
(CBshrtr<-cbind(countries,CPIshrtr))


#Stop and view slide 17

## Create a matrix from a series of values
(mat1 <- matrix(1:12, nrow=4, byrow=TRUE))

## What if we get the rows and columns wrong?
(mat1 <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,12),nrow=5, ncol=3, byrow=TRUE))

(mat1 <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,12),nrow=5,  byrow=TRUE))

