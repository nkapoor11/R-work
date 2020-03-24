rm(list = ls())
Data <- read.csv("KT_Hongchen_Percentiles copy 2.txt", header = T, sep = "\t", stringsAsFactors = F)
#summary(Data)
#head(Data, n=10)
#Genes <- unique(Data$target)

# Prepare data for analysis
Inert <- c("Neo1Ori", "Neo2Ori", "Neo3Ori", "NT1Ori", "Setd2Ori", "Setd2V1", "Stk11Ori", "Stk11V1")
Percentiles <- sort(unique(Data$Percentile))

DataPlot <- c()
for (iPerc in Percentiles) { 
  iPercRow <- Data[Data$Percentile == iPerc,]
  InertTrue <- iPercRow[iPercRow$target %in% Inert,"True"]
  # Correct values
  iPercRow$low <- iPercRow$True - iPercRow$CI # low value
  iPercRow$lowCorr = iPercRow$low / InertTrue # corrected value
  
  iPercRow$high <- iPercRow$True + iPercRow$CI # high value
  iPercRow$highCorr = iPercRow$high / InertTrue # corrected value
  
  iPercRow$trueCorr = iPercRow$True / InertTrue
  DataPlot <- rbind(DataPlot, iPercRow)
}
# move Inert genes to beginning of sgIDs
sgIDs <- c()
sgIDs <- sort(unique(Data$target))
sgIDs <- sgIDs[! sgIDs %in% Inert ] 
sgIDs <- c(Inert, sgIDs) # order to be graphed on x axis.

# Plot a figure
# define attributes of my plot
myXVal <- seq(from = 2/10, to = 8/10, by = 1/10)
myCol1 <- rgb(0,0,0, alpha = myXVal) # black
myCol2 <- rgb(128/255,0,1, alpha = myXVal) # purple
myCol3 <- rgb(1,0,0, alpha = myXVal) # red
myCol4 <- rgb(0,1,0, alpha = myXVal) # green
myCol5 <- rgb(0, 161/255, 1, alpha = myXVal) # blue
myColAll <- c(rep(myCol1, 4), rep(c(myCol2, myCol3, myCol4, myCol5), length(sgIDs)-1), myCol2)

myXValAll <- c()
myYValAll <- c()
YLineLow <- c()
YLineHigh <- c()

for(i in 1:length(sgIDs)){
  myXValAll <- c(myXValAll, myXVal + (i - 1))
  myYValAll <- c(myYValAll, DataPlot$trueCorr[DataPlot$target == sgIDs[i]]) # Why not the Corr values? 
  YLineLow <- c(YLineLow, DataPlot$lowCorr[DataPlot$target == sgIDs[i]]) 
  YLineHigh <- c(YLineHigh, DataPlot$highCorr[DataPlot$target == sgIDs[i]])
}

pdf("Neil KT modified.pdf", width = 100, height = 6)
# draw plot. 
plot(xaxt = "n", x = myXValAll + 0.5, y = myYValAll, ylim = c(min(YLineLow) * .95, max(YLineHigh) * 1.05), pch = 20, cex = 2, 
     col = myColAll, ylab = "Relative growth rate", xlab = "sgRNA")

# for(i in 1:dim(DataPlot)[1]){
for(i in 1:length(sgIDs)){
  lines(x = c(myXValAll[i] +0.5, myXValAll[i]+0.5), y = c(YLineLow[i], YLineHigh[i])) # add lines
}
# axis
lines(c(-1, 101), y=c(1,1), lwd=2, lty=3)
mtext(sgIDs, side = 1, at = 1:101, line=0)
mtext()
# # y axis
# yaxis <- seq(from=0, to = 1.6, by = 0.2)
# vect <- c(1:10)


mtext(text = yaxis, side = 2, at = 1:9, line = 1)
#dyAxis("y", valueRange = c(0, 1.6))
#axis(2, at=c(1, 5, 10), labels=c("First", "Second", "Third"))
#axis(side = 2, at = c(1, 2, 3, 4, 5), labels = c("0.6", "0.8", "1.0", "1.2", "1.4"))
#plot(x = NULL, y = NULL, xlim = NULL, ylim = c(0,10))

dev.off()
# Configure axis settings
#text()
#legend()