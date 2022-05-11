

## Observe the generation of random numbers
(rnorm(3))  # execute this twice

## set a seed so we will get same random numbers every time
set.seed(9182012);(rnorm(3))  # exectute this twice, too and compare with above

####### Additional demo for students to try at home
#set graph area for side by side comparison
par(mfrow=c(1,3))
## plot rnorm
r<-rnorm(200)
hist(r)

#Increase the sample size
r<-rnorm(2000)
hist(r)

for (i in 1:2000){
	r[i]<-rnorm(1)
}
hist(r)
####### End of Additional demo

##### Go to slide 

#set graph area for two rows of graphs
par(mfrow=c(2,1))
	
## Create a vector of X values.  Add an extra for n+1
X<-rep(0,1001)

## Use a loop to generate AR process values


for (i in 1:1000){
	X[i+1]<- (0.9*X[i]) + rnorm(1)
}
X

## setup to observe random number
set.seed(90210)
for (i in 1:1000){
	e <- rnorm(1)
      cat(e)
	X[i+1]<- (0.9*X[i]) + e
}
X
## Alternate method that creates a vector of random numbers first.

set.seed(90210)
(r <- rnorm(1000))

for (i in 1:1000){
	X[i+1]<- (0.9*X[i]) + r[i]
}
X

#plot all values except first 0
plot(1:1000,X[2:1001], type='l')

#plot only the last 500
plot(1:500,X[502:1001], type='l')

##### Go to Slide 

## Create and test a simple function
mysum<-function(a,b,c=0){
	if(is.numeric(a) && is.numeric(b) && is.numeric(c)) {
	a+b+c
	}else {
		cat("All arguments to function must be numeric\n")
	     }
	}

ls()
summary(mysum)
class(mysum)
mode(mysum)

## What do we get when we call the function?
mysum(3,5, "C")
ls()

## Put an assignment inside the function
mysum<-function(a,b,c=0){
	a+b+c->d  ## Note: this assignment is turned around just cuz we can
  }

## call again & observe difference
mysum(3,5)
ls()

## Add return code to the function

mysum<-function(a,b,c=0){
	a+b+c->d
	return(d)
  }

## call again & observe difference
mysum(3,5)
ls()

## Use the function for assignment
d<-mysum(3,5,c=5)
ls()

##  What if we supply character inputs?
mysum(A,B)

mysum("A","B")

mysum("A","B","C")

## What if a and b are vectors?

A<-1:6
B<-7:12
mysum(A,B)

## What is the class & mode of the output now?
d<-mysum(A,B,3)
class(d)
mode(d)

##### Go to slide 

# Create and examine a uniform distribution
par(mfrow=c(1,1))

set.seed(100)
ubt<-runif(50,-4, 4)
hist(ubt)
mean(ubt)
sd(ubt)

##### Go to slide 

## Plot various gamma distributions for illustration

x<-seq(0,20,.1) #vector of quantiles
g1<-dgamma(x,1,scale=2)
plot(x,g1,type='l', col='red')

g2<-dgamma(x,2,scale=2)
lines(x,g2,col='green')

g3<-dgamma(x,3,scale=2)
lines(x,g3,col='dark blue')

g4<-dgamma(x,5,scale=1)
lines(x,g4,col='turquoise')

g5<-dgamma(x,9,scale=.5)
lines(x,g5,col='gold')

## Create 50 random values with gamma distibution

set.seed(90210)
(rbt<-rgamma(50,.9))
summary(rbt)
hist(rbt, col='gray', breaks=12)

## Double the number of values and compare
set.seed(90210)
rbt<-rgamma(100,.9)
summary(rbt)
hist(rbt, col='gray', breaks=12)

##### Go to slide 

###########################
## Demonstrate sampling principles

# permutation (reordering) of 1:10
sample(10)

#bootstrap sample (resampling) of 1:10
sample(10, replace=TRUE)

# subset of 5 items from vector rbt
sample(rbt, 5)

## Can we sample a character vector?
cpi<-read.csv("C:\\Users\\kinchelf\\Documents\\STAT604-FA18\\Lecture Slides\\R\\CorruptionPerceptionIndex.csv")
dim(cpi)
names(cpi)

with(cpi,sample(Country,10))

## Will I get the same results if I sample again?
with(cpi,sample(Country,10))

## How can we get a repeatable sample?



set.seed(924)
with(cpi,sample(Country,10))

with(cpi,sample(Country,10))

set.seed(924)
with(cpi,sample(Country,10))

## How does this work?
set.seed(924)
# Random sample of numbers between 1 and length of cpi(184)
samp.cntry<-sample(length(cpi$Country),10)
samp.cntry

# Use those numbers as row indices
cpi[samp.cntry,1]

#######################
##  Bootstrap experiment
	#1 create a matrix to hold samples
	#2 populate each row with a bootstrap sample
	#3 create a histogram of the sample means (rowmeans)
	#4 Turn this into a function so we can run it again with a diff n

## we are going to use rbt as our population
summary(rbt)
length(rbt)

B<-5 #number of samples
n<-length(rbt) #number of values in our population
smat<-matrix(0,nrow=B,ncol=n) # empty matrix of proper size
for(i in 1:B) smat[i,1:n] <- sample(rbt, replace=TRUE) #Bootstrap samples
hist(rowMeans(smat), col='gray', xlab=paste(B,"Samples"))

Bfx<- function(V, B=5){
# Copy code from above except line that assigns B & replace rbt with V


}

#set up 4 plots in graphics window
par(mfrow=c(2,2))

#plot histogram of rbt
hist(rbt, col='light blue')

#Call function 3 times with different sample sizes
Bfx(rbt) #default of 5
Bfx(rbt,10)
Bfx(rbt,20)


