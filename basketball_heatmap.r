version
library("RColorBrewer")
library("pheatmap")
library("RColorBrewer")
library("gplots")
library("d3heatmap")


data<- read.csv("Heatmap.csv" )
data

row.names(data) <- data$Name
data<-data[,-1]
data
    
ncol(data)
nrow(data)
matrix<-data.matrix(data)
  
nba_heatmap <- heatmap(matrix, Rowv=NA, Colv=NA, col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256), scale="column", margins=c(5,10))

col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
heatmap(matrix, scale = "none", col =  col, RowSideColors = rep(c("orange", "yellow"), each = 25),ColSideColors = c(rep("red",10), rep("orange", 10)))

heatmap.2(matrix, scale = "none", col = bluered(20),    trace = "none", density.info = "none")

pheatmap(matrix, cutree_rows = 5)


d3heatmap(matrix, colors = "RdYlBu", k_row = 4,k_col = 2 )
