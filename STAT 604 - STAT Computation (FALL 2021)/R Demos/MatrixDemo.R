## Create a matrix from a series of values
(mat1 <- matrix(1:12, nrow=4, byrow=TRUE))

## What if we get the rows and columns wrong?
(mat1 <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,12),nrow=5, ncol=3, byrow=TRUE))

(mat1 <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,12),nrow=5,  byrow=TRUE))


## Access various elements of the matrix
mat1
mat1[2,3] 
mat1[1:2,2:3] 
mat1[2,] 
mat1[,2] 
mat1[,-2] 

#Compare this method to access members of data frame
#create a vector of country names
countries<- {c('New Zealand', 'Denmark',
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
'Germany',
'Japan',
'Austria')}

#Create a new vector with our Corruption Perception Index values
CPI<-c(9.5, 9.4, 9.4, 9.3, 9.2, 9, 8.9, 
8.8, 8.8, 8.7, 8.5, 8.4, 8.3, 8, 8, 7.8)

Try using data.frame to combine the vectors 
(cpidf<-data.frame(Country=countries,CPI))

cpidf[3,] #display 3rd row
cpidf[4,1] #display row 4, column 1
cpidf[5,2] #display row 5, column 2
cpidf[1:6,] #display all columns in first 6 rows

#How do we access odd numbered rows?
	#step1 formula for odd X%%2==1
	#step2 substitute row values in formula (1:16)%%2==1
	#step3 use formula as row subscript
	  cpidf[(1:16)%%2==1,]

	#step4 enhance expression to allow more rows (replace 16)
	# Define "hard coding"
	#	  16 is the length of one of the column vectors length(cpidf[,1])
	  cpidf[(1:length(cpidf[,1]))%%2==1,]

#Stop and view slide 9

###Using names
#Is CPI a vector in Workspace or column in data frame?
#Shorten CPI for demo purposes
CPI<-CPI[-length(CPI)]
CPI

mean(CPI)
ls()
search()
attach(cpidf)
search()
rm(CPI) #Remove the vector and see what happens
mean(CPI)
mean(cpidf[,2])
mean(cpidf$CPI)
with(cpidf,mean(CPI))

#Remove cpidf from search path
detach(cpidf)
	mean(CPI)

#access vector of column names
names(cpidf)

#What about rows?
row.names(cpidf)

#Stop and view slide 10

## functions for matrices
dim(mat1)
rowSums(mat1)
colMeans(mat1)


#load workspace containing full cpi data
load("C:\\Users\\kinchelf\\Documents\\STAT604-FA20\\Lecture Slides\\R\\cpi.RData")
ls()

cpi #display results
class(cpi)
mode(cpi)
summary(cpi)
str(cpi)


# Mean CPI per year for all countries
apply(cpi[,2:11],2,mean,na.rm=TRUE)

# Change previous statement to:
# Create a new column in the data frame with average CPI for each country
cpi$AvgCPI<-apply(cpi[,2:11],1,mean,na.rm=TRUE)

###Stop and view slide 12

# Mean CPI per year for all countries for years after 2005
apply(cpi[,names(cpi)>'y2005'],2,mean,na.rm=TRUE)

#### Slide 15

#  combine data frames
(mcaps<-merge(cpis, capitals))
(allcaps<-merge(cpis, capitals, all=TRUE))


