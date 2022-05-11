#### Look at data in editor before importing
## Example of na.strings to Read Corruption data
(cpina<-read.csv("C:\\Users\\kinchelf\\Documents\\STAT604-FA20\\Lecture Slides\\R\\CorruptionPerceptionIndexNA.csv"))
str(cpina)
(cpina<-read.csv("C:\\Users\\kinchelf\\Documents\\STAT604-FA20\\Lecture Slides\\R\\CorruptionPerceptionIndexNA.csv",
	na.strings=c('?','*','n/a')))
str(cpina)

#### Go to slide 5-5


# create a character vector
s <- c('apple','bee','cars','danish','egg')

# get the number of characters in each string
nchar(s)

# Convert all letters to upper case
toupper(s)

#### Go to slide 5-7

# Extract characters from each string
substr(s,2,3)
onetwo <- substr(s,1,2)

# Replace the first and second characters of each string
substr(s,1,2) <- 'BU'
s
substr(s,1,2) <- onetwo 
 
#### Go to slide 5-9

# Replace first e with _
sub('e','_',s) 

# Replace every e with _
gsub('e','_',s) 

#### Go to slide 5-10


# Search s for instances of 'e'
grep('e', s) 

grep('e', s, value=TRUE) 

# Return list of TRUE/FALSE instead of indices
grepl('e', s)


## Examine Corruption data
str(cpina)

# display country names containing Z
grep("Z", cpina$Country)
grep("Z", cpina$Country, value=TRUE)

grep("A", cpina$Country, value=TRUE, ignore.case=TRUE)
grep("^A", cpina$Country, value=TRUE, ignore.case=TRUE)
grep("A", cpina$Country)

## create new column of Average CPI for use in later examples
cpina$AvgCPI<-rowMeans(cpina[ ,grepl('^y',names(cpina))],na.rm=TRUE)
cpina

cpina$AvgCPI[grep("A", cpina$Country)]
cpina[grep("A", cpina$Country),c(1,12)]

#### See slide on ordering values (5-14)
# Show country values
sort(cpina$Country)
sort(cpina$Country, decreasing=TRUE)

# Return ordered index values
order(cpina$Country, decreasing=TRUE)

# Use ordered index values to show other data
cpina[order(cpina$AvgCPI),c(1,12)]

# Base the order of one vector on the order of another
cpina$Country[order(cpina$AvgCPI)]

# Use indices to select only the 10 lowest CPI
cpina$Country[order(cpina$AvgCPI)][1:10]


#### Go to slide 5-17 for Date Values
## Demo of Date functions
#Create two date values
 x <- '2021-09-11'
 y <- '2021-09-10'
# Are they dates?
 class(x)
  mode(x)
as.numeric(as.Date(x))

#Find the time interval between the two
 z <- x-y

#Change the object type
 z <- as.Date(x)-as.Date(y)
 z

# What is z?
class(z)
mode(z)

#Coerce to a number
 as.numeric(z)

##Demo Date times
thisdate <- as.Date('14Sep2021 05:30:00 PM',
                format='%d%b%Y %I:%M:%S %p')
thisdate
class(thisdate)
mode(thisdate)


strp <- strptime('14Sep2021 05:30:00 PM',
                format='%d%b%Y %I:%M:%S %p')
strp
class(strp)
mode(strp)

Plt <- as.POSIXlt('14Sep2021 05:30:00 PM',
                format='%d%b%Y %I:%M:%S %p')
Plt
class(Plt)
mode(Plt)

Pct <- as.POSIXct('14Sep2021 05:30:00 PM',
                format='%d%b%Y %I:%M:%S %p')

Pct
class(Pct)
mode(Pct)
as.numeric(Pct)

##Demo of Lecture 6 character output functions
#tab and new lines
cat("5\t9\n\n")

#Load workspace from previous lecture
load("C:\\Users\\kinchelf\\Documents\\STAT604-FA20\\Lecture Slides\\R\\cpi.RData")

# Create two vectors for demo
(Capital<-capitals$Capital)
(Country<-capitals$Country)


(Capital<-as.character(capitals$Capital))
(Country<-as.character(capitals$Country))


cat(Country, Capital, sep=',')
cat(Country, Capital, sep=',\n')

paste(Country, Capital, sep=',')
paste(Country, Capital, sep=',', collapse='\n')

## try using cat function for csv values
cat(cpi$Country,cpi$AvgCPI,sep=',')

## What is Country?
class(cpi$Country)

## Get the names back
cat(as.character(cpi$Country),cpi$y2011,sep=',')

## Compare the same code using the paste function
paste(cpi$Country,cpi$y2011,sep=',')

## assign it to an object 
(outlist<-paste(cpi$Country,cpi$y2011,sep=',',collapse="\n"))

## What kind of object?
class(outlist)
length(outlist)
nchar(outlist)

## Use cat function to send to file (Note these could have been nested)
cat(outlist,file='avgcpi.csv')
getwd()
#### Go to slide 




