# Stripchart with scatter
library(easyGgplot2)
library(ggplot2)
df = read.csv("ScatterPlot-PharII.txt", header = T, sep = "\t", stringsAsFactors = F)

treatments <- c("T6: PTX/MEKi/Carbo KT", "Phenformin", "Trametinib 6-week", "Trametinib", "Palbociclib")
# treatments <- c("Paclitaxel", "Trametinib", "Paclitaxel + Trametinib", "Phenformin",
#                 "Paclitaxel + Carbo", "Paclitaxel + Trametinib + Carbo", "Everolimus", "Palbociclib")

means <- c()
for(i in 1:length(treatments)) {
  imean <- mean(df$LungWeight[df$Treatment == treatments[i]])
  means <- c(means, imean)
}
means <- sort(means, decreasing = TRUE)

Torder <- c()
LWorder <- c()
for(i in 1:length(means)) {
  for (j in 1:length(treatments)) {
    ilw <- df$LungWeight[df$Treatment == treatments[j]]
    print(ilw)
    if(means[i] == mean(ilw)) {
      # LWorder <- c(LWorder, ilw)
      Torder <- c(Torder, treatments[j]) # rep(treatments[j], length(ilw))
    }
  }
}

Torder <- c("w18 untreated", "w18 untreated KT", Torder)

# plot data. 

# Change main title and axis titles
p1 <- ggplot2.stripchart(data=df, xName='Treatment', yName='LungWeight',
                         mainTitle="Phar II: Plot of LungWeight by Treatment",
                         xtitle="Treatment", ytitle="LungWeight", addMean=TRUE, meanPointShape=21, meanPointSize=4,
                         meanPointColor="black", meanPointFill="RED")

# order the treatments as desired. 
p1 <- ggplot2.customize(p1, legendItemOrder = Torder)
