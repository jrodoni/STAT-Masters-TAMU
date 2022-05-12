####
#### Illustrate PCA with USArrests dataset.
####

library(ISLR)

##
## Example: USArrests data set from ISLR textbook.
##

data(USArrests)

## First standardize the variables.
X <- USArrests[, c(1, 2, 4)]
X <- scale(X, center = TRUE, scale = TRUE)

## Use 'princomp' function.
pca_out <- princomp(X)
summary(pca_out)
print(ll <<- pca_out$loadings)

## Biplot showing first two PCs.
PC_1 <- X %*% ll[, 1]
PC_2 <- X %*% ll[, 2]
PCs <- cbind(PC_1, PC_2)

plot(PC_1, PC_2, xlab = "PC 1 (79%)", ylab = "PC 2 (15%)")

## Label the extreme values.
pc1_min <- which.min(PC_1)
pc1_max <- which.max(PC_1)
pc2_min <- which.min(PC_2)
pc2_max <- which.max(PC_2)

USArrests[pc1_min, ]
USArrests[pc1_max, ]
USArrests[pc2_min, ]
USArrests[pc2_max, ]

PCs[pc1_min, ]
PCs[pc1_max, ]
PCs[pc2_min, ]
PCs[pc2_max, ]

text(-2.5, 0.25, labels = "N. Dakota")
text(2.5, 0.6, labels = "Florida")
text(1.5, -1.5, labels = "Alaska")
text(1, 1.5, labels = "Mississippi")

## Manually match 'princomp'.
Sigma <- var(X)
eigen_out <- eigen(Sigma)
ee <- eigen_out$vectors
lambda <- eigen_out$values

ll[, 1]
ee[, 1]
lambda[1] / sum(lambda)

## The variances of the PCs equal the corresponding eigenvalues.
var(PC_1)
var(PC_2)
lambda[1:2]

## The PCs are uncorrelated with each other.
t(PC_1) %*% PC_2
