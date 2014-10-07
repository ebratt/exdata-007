## week 3 - hierarchical clustering
# clustering organizes things that are "close" into groups
# an agglomerative approach:
#   find closest two things
#   put them together
#   find next closest
# requires
#   a defined distance
#   a merging approach
# produces
#   a tree showing how close things are together
# how do we define close?
#   most important step (gigo)
#   distance or similarity
#     continuous - euclidean distance
#     continuous - correlation similarity
#     binary - manhattan distance (imagine the city street grid)
#   pick a distance that makes sense
set.seed(1234)
par(mar=c(0,0,0,0))
x<-rnorm(12,mean=rep(1:3,each=4),sd=0.2)
y<-rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05, y+0.05, labels=as.character(1:12))
dataFrame <- data.frame(x=x,y=y)
dist(dataFrame) # takes a matrix or data frame and measures the distance between them; defaults to euclidean distance
max(dist(dataFrame))
which(dist(dataFrame)==max(dist(dataFrame)))
# merge 5 and 6 into one point
# then merge 10 and 11 into one point
# keep doing this 
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
par(mar=c(6,5,2,2))
plot(hClustering) # creates a dendrogram
# need to "cut" the tree to see how many clusters there are; ie, draw a horizontal line across the graph at a y value
# prettier dendrograms
myplclust <- function(hclust, lab=hclus$labels, lab.col = rep(1, length(hclust$labels)),
    hang = 0.1, ...) {
  ## modification of plclust for plotting hclust objects *in colour*! Copyright
  ## Eva KF Chan 2009 Arguments: hclust: hclust object lab: a character vector
  ## of labels of the leaves of the tree lab.col: colour for the labels;
  ## NA=default device foreground colour hang: as in hclust & plclust Side
  ## effect: A display of hierarchical cluster with coloured leaf labels.
  y <- rep(hclust$height, 2)
  x <- as.numeric(hclust$merge)
  y <- y[which(x < 0)]
  x <- x[which(x < 0)]
  x <- abs(x)
  y <- y[order(x)]
  x <- x[order(x)]
  plot(hclust, labels=F, hang=hang, ...)
  text(x=x, y=y[hclust$order] - (max(hclust$height) * hang), labels=lab[hclust$order],
       col=lab.col[hclust$order], srt=90, adj = c(1, 0.5), xpd=NA, ...)
}
myplclust(hClustering, lab=rep(1:3, each=4), lab.col=rep(1:3, each=4))
# check out http://gallery.r-enthusiasts.com/RGraphGallery.php?graph=79
# merging points - complete
# heatmap function is great for large matrix of data
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
heatmap(dataMatrix)
str(dataMatrix)
# HCA is done to give an idea of the relationships between variables/observations
d <- dist(as.matrix(mtcars))   # find distance matrix 
hc <- hclust(d)                # apply hirarchical clustering 
plot(hc)                       # plot the dendrogram

# K-Means Clustering with 5 clusters
fit <- kmeans(mtcars, 5)
# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
library(cluster) 
clusplot(mtcars, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
# Centroid Plot against 1st 2 discriminant functions
install.packages("fpc")
library(fpc)
plotcluster(mtcars, fit$cluster)

## K-Means clustering
# a partitioning approach
#   fix a number of clusters
#   get "centroids" of each cluster
#   assign things to closest centroid
#   recalculate centroids
# requires
#   a defined distance metric
#   a number of clusters
#   an initial guess at centroids
# produces
#   final estimate of cluster centroids
#   assignment of each point to the clusters
set.seed(1234)
x<-rnorm(12,mean=rep(1:3,each=4),sd=0.2)
y<-rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05, y+0.05, labels=as.character(1:12))
dataFrame <- data.frame(x,y)
kmeansObj <- kmeans(dataFrame, centers=3)
names(kmeansObj)
kmeansObj$cluster
kmeansObj$centers
kmeansObj$size
# one way to visualize clustering is to plot the k-means clusters and their centroids
plot(x,y,col=kmeansObj$cluster,pch=19,cex=2)
points(kmeansObj$centers, col=1:3, pch=3, cex=3, lwd=3)
# another way to visualize clustering is to use a heat map
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
kmeansObj2 <- kmeans(dataMatrix, centers=3)
par(mfrow=c(1,2),mar=c(2,4,0.1,0.1))
image(t(dataMatrix)[, nrow(dataMatrix):1], yaxt="n")
image(t(dataMatrix)[, order(kmeansObj2$cluster)], yaxt="n")
# k-means requires a number of clusters
#   pick by eye/intuition
#   pick by cross validation/information theory, etc.
# K-means is not deterministic
#   different # of clusters
#   different # of iterations

## Principal Component Analysis (PCA) and Singular Value Decomposition
rm(list=ls())
set.seed(12345)
par(mar=rep(0.2, 4))
dataMatrix <- matrix(rnorm(400), nrow=40)
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])
heatmap(dataMatrix)
# nothing interesting b/c there are no relationships in the data, so let's try something interesting
set.seed(678910)
rm(list=ls())
dataMatrix <- matrix(rnorm(400), nrow=40)
for (i in 1:40) {
  # flip a coin
  coinFlip <- rbinom(1,size=1,prob=0.5)
  # if coin is heads add a common pattern to that row
  if (coinFlip) {
    dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0,3), each=5)
  }
}
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])
heatmap(dataMatrix)
# patterns in rows and columns
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1, , xlab="Row Mean", ylab="Row", pch=19)
plot(colMeans(dataMatrixOrdered), xlab="Column", ylab="Column Mean", pch=19)
# related problems
#   find a new set of multivariate variables that are uncorrelated and explain as much variance as possible
#   if you put all the variables together in one matrix, find the best matrix created with fewer variables (lower rank) that explains the original data
# The first goal is statistical and the second goal is data compression

# Singular Value Decomposition (SVD):
# If X is a matrix with each variable in a column and each observation in a row then the SVD is a "matrix decomposition"
#   X = UDV
# where the columns of U are orthogonal (left singular vectors), the columns of V are orthogonal (right singular vectors) and D is a diagonal matrix (singular values)

# PCA
# The principal components are equal to the right singular values if you first scale (subtract the mean, divide by the standard deviation) the variables.
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd1$u[, 1], 40:1, , xlab="Row", ylab="First left singular vector", pch=19)
plot(svd1$v[, 1], xlab="Column", ylab="First right singular vector", pch=19)
# svd picked-up the pattern right away without having to be told
# variance explained
par(mfrow=c(1,2))
plot(svd1$d, xlab="Column", ylab="Singular value", pch=19)
plot(svd1$d^2/sum(svd1$d^2), xlab="Column", ylab="Prop. of variance explained", pch=19)
par(mfrow=c(1,1))
par(mar=c(5,4,1,1))
pca1 <- prcomp(dataMatrixOrdered, scale=T)
plot(pca1$rotation[, 1], svd1$v[, 1], pch=19, xlab="Principal Component 1", ylab="Right Singular Vector 1")
abline(c(0,1))
# more variance explained
constantMatrix <- dataMatrixOrdered*0
for(i in 1:dim(dataMatrixOrdered)[1]){constantMatrix[i,] <- rep(c(0,1),each=5)}
svd1 <- svd(constantMatrix)
par(mfrow=c(1,3))
image(t(constantMatrix)[, nrow(constantMatrix):1])
plot(svd1$d, xlab="Column", ylab="Singular Value", pch=19)
plot(svd1$d^2/sum(svd1$d^2), xlab="Column", ylab="Prop. of variance explained", pch=19)
# this means that 100% of the variation is explained by just one dimension
# lets add a second pattern
set.seed(678910)
for (i in 1:40) {
  # flip a coin
  coinFlip1 <- rbinom(1, size=1, prob=0.5)
  coinFlip2 <- rbinom(1, size=1, prob=0.5)
  # if coin is heads add a common pattern to that row
  if (coinFlip1) { dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0,5), each=5) }
  if (coinFlip2) { dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0,5), 5) }
}
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]
svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rep(c(0,1), each=5), pch=19, xlab="Column", ylab="Pattern 1")
plot(rep(c(0,1), 5), pch=19, xlab="Column", ylab="Pattern 2")
# is there a way to pick up on the fact that there are two patterns with SVD?
# v and patterns of variance in rows
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd2$v[, 1], pch=19, xlab="Column", ylab="First right singular vector")
plot(svd2$v[, 2], pch=19, xlab="Column", ylab="Second right singular vector")
# d and variance explained
par(mfrow=c(1,2))
svd1 <- svd(scale(dataMatrixOrdered))
plot(svd1$d, xlab="Column", ylab="Singular Value", pch=19)
plot(svd1$d^2/sum(svd1$d^2), xlab="Column", ylab="Percent of variance explained", pch=19)
# the shift pattern comprises almost 60% of the variation; the alternating pattern only makes up about 18%

# missing values
dataMatrix2 <- dataMatrixOrdered
# randomly insert some missing data
dataMatrix2[sample(1:100, size=40, replace=FALSE)] <- NA
svd1 <- svd(scale(dataMatrix2))
# doesn't work, so what do we do with the missing values? 
# one option is to use the impute package to fill in the missing data points
source("http://bioconductor.org/biocLite.R")
biocLite("impute")
library(impute)
dataMatrix2 <- impute.knn(dataMatrix2)$data # use k-nearest neighbors to complete the data
svd1 <- svd(scale(dataMatrixOrdered))
svd2 <- svd(scale(dataMatrix2))
par(mfrow=c(1,2))
plot(svd1$v[,1],pch=19)
plot(svd2$v[,1],pch=19)
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
image(t(dataMatrix2)[, nrow(dataMatrix2):1])
# idea is to filter-out all the "noise" -- compression helps

## plotting and working with color in R
# default color schemes for most plots in R are really bad
# there are handy functions that overcome this
# grDevices has two functions:
#   colorRamp: takes a palette of colors and returns a function that takes values between 0 and 1
#   colorRampPalette: takes a palette of colors and returns a function that takes values between 0 and 1 and returns a character vector of colors interpolating the palette
# they help interpolate between the colors
# colors() also gives you all the named colors
rm(list=ls())
pal <- colorRamp(c("red", "blue"))
pal(0)
pal(1)
pal(0.5)
pal(0.25)
pal(0.75)
pal <- colorRamp(c("red","green","blue"))
pal(0)
pal(1)
pal(0.5)
pal(0.25)
pal(0.75)
pal(seq(0,1,len=10))
pal <- colorRampPalette(c("red","yellow"))
pal(2)
pal(10)
# where do i come up with interesting colors?
# RColorBrewer package handles this
# three types of palettes:
#   sequential: ordered numeric data
#   diverging: diverging numeric data (they move away from each other)
#   qualitative: data that are not ordered (factors)
# can be used in conjunction with colorRamp() and colorRampPalette()
library(RColorBrewer)
cols <- brewer.pal(3, "BuGn")
cols
pal <- colorRampPalette(cols)
image(volcano, col=pal(10))
pal(10)
x <- rnorm(1000)
y <- rnorm(1000)
smoothScatter(x,y)
# the rgb function can be used to produce any color via red, green, and blue proportions
# color transparency can be added via the alpha parameter to rgb (0 to 1)
# the colorspace package can be used for different control over colors
plot(x,y,pch=19, col=rgb(1,0,0,0.2))
# use transparency when trying to clarify with many points