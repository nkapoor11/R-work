library(gridExtra)
library(grid)
library(gtable)
library(scales)
library(pheatmap)


# load data
# import treatments.
t <- read.delim("Treat - Sheet1.tsv", header = T, sep = "\t", stringsAsFactors = F)
tm <- as.matrix(t)
# import genes and numbers.
d <- read.delim("CellNumberLNMean - Sheet1.tsv", header = T, sep = "\t", stringsAsFactors = F)
dm <- as.matrix(d)
# add treatment names. 
rownames(dm) <- paste0(tm, 1:116)
# transpose matrix so genes are on vertical axis. create heatmap using pheatmap.
pheatmap(t(dm))