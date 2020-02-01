# cluster analysis 

# model based clustering selects number of clusters
# kmeans() does basic k-means clustering
# hclust() does hierarchical aggomerative clustering
# Use plot() to plot the results.  
# See help(hclust) for more.
# library(cluster) provides lots of agglomerative and divisive
# clustering methods 
# Agglomerative hierarchical methods (hclust, agnes)
#   - show how to merge smaller clusters into larger ones
#   - main differences are in how to calculate between-cluster
#     distances ("single", "complete" or "average" linkage)
# * Optimal partitioning methods (kmeans)
#   - need to know how many clusters K to produce
#   - need an initial clustering
# * Divisive hierarchical methods (diana, mona)
#   - show how to break larger clusters into smaller ones
#   - attractive when a few large clusters are desired
#   - most methods make splits "one variable at a time"
#   - computationally nearly impossible to find optimal solutions

data(iris)
names(iris)
y <- as.matrix(iris[,-5])[6*(1:25),]   # subsample to make the graphs

rownames(y) <- iris$species[6*(1:25)]  # pretty
# "default" plot orientation
# combine multiple plots into one overall graph, using either the
# par( ) or layout( ) function
# mfrow=c(nrows, ncols) to create a matrix of nrows x ncols plots that are filled in by row.
# mfcol=c(nrows, ncols) fills in the matrix by columns.
par(mfrow=c(2,3))
plot(hclust(dist(y),method="single"))
plot(hclust(dist(y),method="complete"))
plot(hclust(dist(y),method="average"))

plot(hclust(dist(y)^2,method="single"))
plot(hclust(dist(y)^2,method="complete"))
plot(hclust(dist(y)^2,method="average"))

# plotting horizontally instead

par(mfrow=c(2,3))

plot(as.dendrogram(hclust(dist(y),method="single")),horiz=T,
     main="dist(y)\nSingle")
plot(as.dendrogram(hclust(dist(y),method="complete")),horiz=T,
     main="dist(y)\nComplete")
plot(as.dendrogram(hclust(dist(y),method="average")),horiz=T,
     main="dist(y)\nAverage")

plot(as.dendrogram(hclust(dist(y)^2,method="single")),horiz=T,
     main="dist(y)^2\nSingle")
plot(as.dendrogram(hclust(dist(y)^2,method="complete")),horiz=T,
     main="dist(y)^2\nComplete")
plot(as.dendrogram(hclust(dist(y)^2,method="average")),horiz=T,
     main="dist(y)^2\nAverage")
# try 'agnes' from library(cluster) -- should be like 'hclust'
library(cluster)
par(mfrow=c(2,3))
plot(hclust(dist(y),method="single"))
plot(hclust(dist(y),method="complete"))
plot(hclust(dist(y),method="average"))
plot(agnes(dist(y),method="single"),which.plots=2)
plot(agnes(dist(y),method="complete"),which.plots=2)
plot(agnes(dist(y),method="average"),which.plots=2)

y <- as.matrix(iris[,1:4])           
rownames(y) <- iris$Species

hclust.avg <- hclust(dist(y),method="average")

par(mfrow=c(1,1))
plot(hclust.avg)
