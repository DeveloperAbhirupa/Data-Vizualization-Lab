
names(car)
y <- as.matrix(car[,-5])[6*(1:25),]   # subsample to make the graphs

rownames(y) <- car$model[6*(1:25)]  
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

y <- as.matrix(car[,1:4])           
rownames(y) <- car$model

hclust.avg <- hclust(dist(y),method="average")

par(mfrow=c(1,1))
plot(hclust.avg)
