# This is the final subfunction for plotting jitter plot
# Input: A dataframe with sgID, BC, CellNum, CellNumNorm (Normalized to Inert)
# Setting: 
#   CellCutoff: only show cell larger than certain cutoff
#   col: allow color scheme to be set, though with a default
#   scale: how big dot should be for 500000 cells, adjust to make each dot bigger or smaller
#   Inert: What's the name of the Inert
# rm(list=ls())
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(RColorBrewer)
JitterTumor <- function(Data, FileName="Test.pdf", CellCutoff = 200, color="Default", scale = 3, Inert = c("Neo1","Neo2","Neo3","NT1")){
  Data <- Data[Data$CellNum > CellCutoff, ]
  sgIDAll <- sort(unique(Data$sgID))
  sgIDAll <- c(Inert, sgIDAll[!sgIDAll %in% Inert])
  
  # Define color space
  if (color == "Default"){
    color1 <- c(rep("gray", 4))
    color2 <- c(brewer.pal(9,"Set1"), brewer.pal(8,"Dark2"))
    color2[6] <- "steelblue"
    color = c(color1, rep(color2, length(sgIDAll)/10))
  }
  
  X <- c()
  Y <- c()
  LNMean <- c()
  col <- c()
  
  for(i in 1:length(sgIDAll)){
    Data1 <- Data$CellNum[Data$sgID == sgIDAll[i]]
    LNMean <- c(LNMean, log10(LN_mean(Data1)))
    X <- c(X, rep(i, length(Data1)))
    Y <- c(Y, log10(Data1))
    col <- c(col, rep(color[i], length(Data1)))
  }
  
  # can plot the biggest tumor to have the same size
  # scale = 3
  # cex <- sqrt(10^Y)*scale/max(sqrt(10^Y))
  
  # can plot a certain size tumor with the same size
  YBar = 500000
  cex <- (10^Y)^(1/3)/(YBar)^(1/3)*scale
  
  ymax = max(Y)*1.05
  ymin = min(Y)*0.96
  pdf(FileName, width = length(sgIDAll), height = 5)
  # change the color
  plot(jitter(x = X, amount = 0.35), Y, pch=20, cex=cex, col=unlist(col), xaxt="n", yaxt="n", ylim=c(ymin, ymax),
       xlab="Targeted tumor suppressor", ylab = "Tumor size (cancer cell no.)")
  for(i in 1:length(sgIDAll)){
    lines(c(i-0.4,i+0.4), rep(LNMean[i], 2), lwd=2, col=rgb(0,0,0,0.5))
  }
  
  # plot x and y axes labels
  text(1:length(sgIDAll), par("usr")[3] - 0.22, labels = sgIDAll, srt = 35, pos = 1, xpd = TRUE)
  # axis(side=1, at = 1:length(sgIDAll), tick = F, labels = sgIDAll, srt=45)
  yaxis2 <- c(expression(paste("10"^1)), expression(paste("10"^2)), expression(paste("10"^3)), expression(paste("10"^4)), 
              expression(paste("10"^5)), expression(paste("10"^6)), expression(paste("10"^7)))
  axis(side=2, at = 1:7, tick = T, labels = yaxis2, las=1)
  
  # legend
  sizeBar = 10000^(1/3)/(YBar)^(1/3)*scale
  legend(x = "topleft", y = 1, legend=c(" ~ 10,000 cells"), col="black", pch = 20, pt.cex=sizeBar, bty='n')
  
  # annotate top of graph for size.
  size <- c()
  for (sgID in sgIDAll){
    size <- c(size, sum(Data$sgID == sgID))
  }
  # sample size for each sgID
  
  mtext(bquote(paste(italic("N"), " = ")), side = 3, at = 1:length(size), line = 1.5, cex = 0.75) # annotate "N = " in italics
  mtext(size, side = 3, at = 1:length(size), line = .5, cex = 0.75) # annotate size
  
  
  dev.off()
}

# Subfunction to calculate log normal mean
LN_mean <- function(InertCellNum) {
  # MLE of mean of data, presuming a LogNormal Distribution.
  LN_x = log(InertCellNum)
  X = mean(LN_x)
  X2 = var(LN_x)
  return (exp(X + 0.5*X2))
}

# # A test case using Hongchen's Data
# Data <- read.csv("TestJitter.txt", header = T, sep="\t", stringsAsFactors = F)
# Inert = c("Neo1Ori", "Neo2Ori", "Neo3Ori", "NT1Ori")
# JitterTumor(Data, Inert=Inert)


# # Now plot using S23_BarcodeCleanCellNum.txt file.
# files <- c("BRAF_1_BarCodeFinal.txt", "BRAF_2_BarCodeFinal.txt")
D2 <- read.csv("BRAF_2_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
D3 <- read.csv("BRAF_3_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
D13 <- read.csv("BRAF_13_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
D14 <- read.csv("BRAF_14_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
D15 <- read.csv("BRAF_15_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
D16 <- read.csv("BRAF_16_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
group1 <- rbind(D2, D3, D13, D14, D15, D16)
pdfName1 <- "Group 1.pdf"

D4 <- read.csv("BRAF_4_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
D5 <- read.csv("BRAF_5_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
D6 <- read.csv("BRAF_6_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
D7 <- read.csv("BRAF_7_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
D8 <- read.csv("BRAF_8_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
D9 <- read.csv("BRAF_9_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
D10 <- read.csv("BRAF_10_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
D11 <- read.csv("BRAF_11_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
D17 <- read.csv("BRAF_17_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
D18 <- read.csv("BRAF_18_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
D25 <- read.csv("BRAF_25_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
group2 <- rbind(D4, D5, D6, D7, D8, D9, D10, D11, D17, D18, D25)
pdfName2 <- "Group 2.pdf"

D12 <- read.csv("BRAF_12_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
D19 <- read.csv("BRAF_19_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
D20 <- read.csv("BRAF_20_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
D21 <- read.csv("BRAF_21_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
D22 <- read.csv("BRAF_22_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
D23 <- read.csv("BRAF_23_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
D24 <- read.csv("BRAF_24_BarCodeFinal.txt", header = T, sep="\t", stringsAsFactors = F)
group3 <- rbind(D12, D19, D20, D21, D22, D23, D24)
pdfName3 <- "Group 3.pdf"

JitterTumor(group1, FileName = pdfName1, CellCutoff = 0)
JitterTumor(group2, FileName = pdfName2, CellCutoff = 0)
JitterTumor(group3, FileName = pdfName3, CellCutoff = 0)

# # Now plot using S25_BarcodeCleanCellNum.txt file.
# Data <- read.csv("S25_BarcodeCleanCellNum.txt", header = T, sep="\t", stringsAsFactors = F)
# JitterTumor(Data, FileName = "Point size S25 subfunction.pdf", CellCutoff = 500)
