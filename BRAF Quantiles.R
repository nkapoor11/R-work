rm(list = ls())

QuantileTumor <- function(Data, FileName="Test.pdf", Inert = c("Neo1","Neo2","Neo3","NT1")) {
  Percentiles <- sort(unique(Data$Percentile))
  DataPlot <- c()
  for (iPerc in Percentiles) { 
    iPercRow <- Data[Data$Percentile == iPerc,]
    # calculate IntertTrue value (mean of Neo1, Neo2, Neo3, NT)
    InertTrue <- iPercRow[iPercRow$target %in% Inert,"FinalGR"]
    meanInertTrue <- mean(InertTrue)  # mean is 1 for all three files. 
    # Correct values
    iPercRow$low <- iPercRow$FinalGR - iPercRow$GRCI # low value
    iPercRow$lowCorr = iPercRow$low / meanInertTrue # corrected value
    iPercRow$high <- iPercRow$FinalGR + iPercRow$GRCI # high value
    iPercRow$highCorr = iPercRow$high / meanInertTrue # corrected value
    iPercRow$trueCorr = iPercRow$FinalGR / meanInertTrue
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
  
  pdf(FileName, width = 15, height = 6)

  # draw plot. 
  plot(xaxt = "n", x = myXValAll + .5, y = myYValAll, ylim = c(min(YLineLow) * .95, max(YLineHigh) * 1.05), pch = 20, cex = 2, 
       col = myColAll, ylab = "Relative growth rate", xlab = "sgRNA")
  
  for(i in 1:dim(DataPlot)[1]){
    lines(x = c(myXValAll[i] +0.5, myXValAll[i]+0.5), y = c(YLineLow[i], YLineHigh[i])) # add lines
  }
  # x axis
  lines(c(-1, 110), y=c(1,1), lwd=2, lty=3)
  mtext(sgIDs, side = 1, at = 1:101, line=0)

  # y axis
  yaxis <- seq(from=0, to = 1.6, by = 0.2)
  mtext(text = yaxis, side = 2, at = 1:9, line = 1)
  dev.off()
}

Data <- read.csv("BRAF_W8BrafCas9_Percentiles.txt", header = T, sep = "\t", stringsAsFactors = F)
QuantileTumor(Data, FileName = "BRAF_W8BrafCas9_Percentiles.pdf")

Data <- read.csv("BRAF_W16Braf_Percentiles.txt", header = T, sep = "\t", stringsAsFactors = F)
QuantileTumor(Data, FileName = "BRAF_W16Braf_Percentiles.pdf")

Data <- read.csv("BRAF_W16BrafCas_Percentiles.txt", header = T, sep = "\t", stringsAsFactors = F)
QuantileTumor(Data, FileName = "BRAF_W16BrafCas_Percentiles.pdf")
