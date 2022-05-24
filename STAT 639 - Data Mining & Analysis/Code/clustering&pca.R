# Principal Components Analysis
# We will use the `USArrests` data (which is in R)
# Look at the dimension of the data set
?USArrests
dim(USArrests)
# Each observation is a state of US
row.names(USArrests)
# look at variables in this data set
names(USArrests)
# calculate column mean
apply(USArrests, 2, mean) # you can also do colMeans(USArrests)
# calculate column variance
apply(USArrests, 2, var)
# We see that `Assault` has a much larger variance than the other variables. 
# It would dominate the principal components, so we choose to standardize the variables when we perform PCA
# prcomp function is in the standard libary
pca.out=prcomp(USArrests, scale=TRUE)
# Print out the output of pca, which include all the principal component loadings
# There are total of 4 pricipal components in this case
pca.out
# Let's look at each output of prcomp function
names(pca.out)
# sdev is the sd of each pricipal component scores
pca.out$sdev
# center is the mean of each variable before applying pca
pca.out$center
# scale is the sd of each variable before applying pca
pca.out$scale
# rotation is the matrix that contains all the pricipal component loadings
pca.out$rotation
# x is the pricinpal component score for each data point
pca.out$x
# the dimension of x should be n by min(n-1, p)
dim(pca.out$x)
# this should be the same as pca.out$sdev
apply(pca.out$x, 2, sd)
# let's make the very confusing biplot
biplot(pca.out, scale=0)
# this is mirror image of what we see in class, because the loading vector are unit vectors so the sign doesn't matter
# if we flip the sign of the loading vectors, which also flip the sign of all principal component scores, we will get the same plot
pca.out$rotation=-pca.out$rotation
pca.out$x=-pca.out$x
# there you have it, flipped biplot
biplot(pca.out, scale=0)
# let's calculate the proportion of variance explained
pca.out$sdev
pr.var=pca.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
# cumsum is a function to calculate cumulative sum of each element in a vector
# see example below
a=c(1,2,8,-3)
cumsum(a)


####################################################################################################################
# K-Means Clustering
# K-means works in any dimension, but is most fun to demonstrate in two, because we can plot pictures.
# Lets make some data with clusters. We do this by shifting the means of the points around.
set.seed(2020)
# Simulate data: generate random sample from standard normal and make a 100 by 2 matrix X
x=matrix(rnorm(100*2),100,2)
plot(x)
# I want 4 clusters so I generate 4 cluster centers
xmean=matrix(rnorm(8,sd=5),4,2)
plot(xmean)
# Now randomly assign cluster ID to my 100 data points
which=sample(1:4,100,replace=TRUE)
# shift each data point by the cluster center for the particular cluster that data point belongs to
x=x+xmean[which,]
# plot the 4 clusters using different colors
plot(x,col=which,pch=19)
# We know the "true" cluster IDs, but we wont tell that to the `kmeans` algorithm.
# apply k-means with k=4 and using 15 different random start
km.out=kmeans(x,centers=4,nstart=15)
# print output: there are cluster means, cluster assignment, within cluster ss
km.out
# let's plot the cluster results by kmeans
plot(x,col=km.out$cluster,cex=2,pch=1,lwd=2)
# let's also add the original data with true cluster id, using different point type
# because we want to see which points are misclustered by kmeans
points(x,col=which,pch=19)
# but this plot looks confusing because there are label swithing in clustering:
# meaning the true cluster named "1" is not the cluster named "1" by kmeans
# we can fix that by reassigning colors since we know the true clusters
# now we can see which points are misclustered
points(x,col=c(4,1,2,3)[which],pch=19)
# play with kmeans with different choice of k
#k=3
set.seed(2020)
km.out3=kmeans(x,centers=3,nstart=15)
plot(x,col=km.out3$cluster,cex=2,pch=1,lwd=2)
points(x,col=c(2,3,1,4)[which],pch=19)
#k=5
set.seed(2020)
km.out5=kmeans(x,centers=5,nstart=15)
plot(x,col=km.out5$cluster,cex=2,pch=1,lwd=2)
points(x,col=c(2,4,3,1)[which],pch=19)



#dbscan
library(dbscan)
db = dbscan(x, eps = 1, minPts = 4)#You can play with the two tuning parameters. You will see it quite sensitve. 
plot(x,col=db$cluster+1,cex=2,pch=1,lwd=2)

db = dbscan(x, eps = 1.1, minPts = 4) 
plot(x,col=db$cluster+1,cex=2,pch=1,lwd=2)

db = dbscan(x, eps = .9, minPts = 4) 
plot(x,col=db$cluster+1,cex=2,pch=1,lwd=2)

db = dbscan(x, eps = 1, minPts = 3) 
plot(x,col=db$cluster+1,cex=2,pch=1,lwd=2)

db = dbscan(x, eps = 1, minPts = 5) 
plot(x,col=db$cluster+1,cex=2,pch=1,lwd=2)





# Hierarchical Clustering
# We will use these same data and use hierarchical clustering
?hclust
?dist
hc.complete=hclust(dist(x),method="complete")
plot(hc.complete)
hc.single=hclust(dist(x),method="single")
plot(hc.single)
hc.average=hclust(dist(x),method="average")
plot(hc.average)
# Lets compare this with the actualy clusters in the data. 
# We will use the function `cutree` to cut the tree at level 4.
# This will produce a vector of numbers from 1 to 4, saying which branch each observation is on. 
# You will sometimes see pretty plots where the leaves of the dendrogram are colored. 
# We can use `table` to see how well they match:
# we use cutree function to cut at level 4 as our final cluster assignment
hc.cut=cutree(hc.complete,4)
# compare with the true cluster
# here it is really confusing because of label switching again
# you need to look for large numbers for correct cluster and small numbers for miscluster
# because it's no longer the same as the confusion matrix as in the classification problem where the correct ones are on the diagonal
table(hc.cut,which)
table(hc.cut,c(4,1,3,2)[which])
# we can also compare hierarchical clustering with kmeans
# again do not look for diagonal...
table(hc.cut,km.out$cluster)
table(hc.cut,c(1,3,2,4)[km.out$cluster])
# or we can use our group membership as labels for the leaves of the dendrogram:
plot(hc.complete,labels=which)



#Gaussian mixture model (GMM)
library(mclust)
gmm = Mclust(x, G = 4)
plot(x,col=gmm$classification,cex=2,pch=1,lwd=2)
points(x,col=c(4,1,3,2)[which],pch=19)
#soft assignment
gmm$z[1:10,]

#let's look at a harder scenario
set.seed(2020)
x=matrix(rnorm(100*2),100,2)
xmean=matrix(rnorm(8,sd=1),4,2)#the standard deviation is reduced
which=sample(1:4,100,replace=TRUE)
x=x+xmean[which,]
plot(x,col=which,pch=19)

gmm = Mclust(x, G = 4)
plot(x,col=gmm$classification,cex=2,pch=1,lwd=2)
points(x,col=c(4,1,2,3)[which],pch=19)
gmm$z[1:10,]



# Explore the following code with the NCI60 data by yourself!
library(ISLR)
?NCI60
nci.labs=NCI60$labs
nci.data=NCI60$data
dim(nci.data)
nci.labs[1:4]
table(nci.labs)

# PCA on the NCI60 Data
pr.out=prcomp(nci.data, scale=TRUE)
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19,xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19,xlab="Z1",ylab="Z3")
summary(pr.out)
plot(pr.out)
pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve,  type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component", col="brown3")

# Clustering the Observations of the NCI60 Data
sd.data=scale(nci.data)
par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist), labels=nci.labs, main="Complete Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs, main="Average Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="single"), labels=nci.labs,  main="Single Linkage", xlab="", sub="",ylab="")
hc.out=hclust(dist(sd.data))
hc.clusters=cutree(hc.out,4)
table(hc.clusters,nci.labs)
par(mfrow=c(1,1))
plot(hc.out, labels=nci.labs)
abline(h=139, col="red")
set.seed(2)
km.out=kmeans(sd.data, 4, nstart=20)
km.clusters=km.out$cluster
table(km.clusters,hc.clusters)
hc.out=hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels=nci.labs, main="Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out,4), nci.labs)
# 
