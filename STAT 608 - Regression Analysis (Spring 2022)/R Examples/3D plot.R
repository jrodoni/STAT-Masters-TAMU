library(rgl)
library(car)

x1 <- c(2,1,1,3)
x2 <- c(1,2,3,1)
y1  <- c(1,3,3,1)

scatter3d(x=x1, y=y1, z=x2, point.col="blue", surface=FALSE)

n<-10
x1 <- c(rep(c(1:n),n),4,5,6,11,11,11,2,3,4,11,11,12,0,0,5,6)
x2 <- c(rep(1,n), rep(2,n), rep(3,n), rep(4, n), rep(5, n), rep(6, n), rep(7,n), rep(8,n), rep(9,n), rep(10,n),11,11,11,4,5,6,11,11,12,2,3,4,5,6,0,0)
e2 <- rep(0, length(x1))
e2[10] <- -1
e2[9] <- -0.5
e2[20] <- -0.5
e2[19] <- -0.2
e2[30] <- -0.3
e2[40] <- -0.2
e2[71] <- 0.5
e2[81] <- 0.5
e2[5] <- 0.3
e2[6] <- 0.4
e2[7] <- 0.5
e2[8] <-0.7
e2[37] <- 1
y2 <- (18 - 2*x1 -2*x2)/5 + rnorm(length(x1),sd=0.2) + e2

mydata <- data.frame(cbind(x1,x2,y2))
blobdata <- mydata[-c(1,2,3,11,12,100,99,98,90,10,91,92),]

scatter3d(x=blobdata$x1, y=blobdata$y2, z=blobdata$x2, point.col="blue", surface=FALSE)


plot(blobdata$x1,blobdata$y2)
plot(blobdata$x2,blobdata$y2)

plot(blobdata$x1,blobdata$x2)

scatter3d(x=blobdata$x1, y=blobdata$y2, z=blobdata$x2, point.col="blue")

