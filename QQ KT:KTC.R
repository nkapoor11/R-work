rm(list = ls())
library(RColorBrewer)
#all palette available from RColorBrewer
display.brewer.all()
#we will select the first 4 colors in the Set1 palette
cols<-brewer.pal(n=4,name="Set1")

# plot is for growth rate, not tumor size.
# True - CI is "low".
# True + CI is "high".
# The KT file is the control group while the KTC file is the treatment group.

KTCdata <- read.csv("KTC_Hongchen_Percentiles.txt", header = T, sep = "\t", stringsAsFactors = F)
KTdata <- read.csv("KT_Hongchen_Percentiles.txt", header = T, sep = "\t", stringsAsFactors = F)

KTCTrue <- KTCdata$True
KTTrue <- KTdata$True

KTCpvalues <- KTCdata$Sig
KTpvalues <- KTdata$Sig

KTC_CI <- KTCdata$CI
KT_CI <- KTdata$CI

x <- c()
y <- c()

# order of x/y values: Stk11Ori, Stk11V1, Setd2Ori, Setd2V1, Rb1Ori, AtmV1, Arid1aV1
x <- c(1.012589, 1.011359, 1.008196, 0.9846671, 0.9909408, 0.9830329, 0.9801573) # KT
# y <- c(1.238143, 1.282474, 1.204155, 1.122408, 1.056533, 1.007605, 1.022089) # KTC old
y <- c(1.290197, 1.336272, 1.254491, 1.168874, 1.099526, 1.048248, 1.063305)
# mean values from google doc: 1.290197 1.336272 1.254491 1.168874 1.099526 1.048248 1.063305

sgIDs <- c("Stk11Ori", "Stk11V1", "Setd2Ori", "Setd2V1", "Rb1Ori", "AtmV1", "Arid1aV1")
i <- 1
for (i in 1:length(sgIDs)) {
  CIy <- KTC_CI[KTCdata$target == sgIDs[i]] #sgIDs[i]]
  CIy <- CIy[4] # use median for confidence intervals
  lines(rep(x[i], 2), c(y[i] - CIy, y[i] + CIy), lwd=1, lty=1, col = "red")
  
  CIx <- KT_CI[KTdata$target == sgIDs[i]] #sgIDs[i]]
  CIx <- CIx[4]
  lines(c(x[i] - CIx, x[i] + CIx), rep(y[i], 2), lwd=1, lty=1, col = "red")
  
  # annotate sgID
  text(x[i] + 0.07, y[i], sgIDs[i])
}

plot(x,y, xlab="KT;Cas9 (relative to inert)", ylab="KTC;Cas9 (relative to inert)", main ="KT and KTC Mean Relative Growth Rate", ylim=c(.95,1.4), xlim=c(.95, 1.4), cex = 1.5, pch=16, col="blue")
abline(a=0, b=1, lty = 2)

# dev.off()