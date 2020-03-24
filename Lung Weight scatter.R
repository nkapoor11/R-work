# # Stripchart from a single numeric vector 
# ggplot2.stripchart(data=numVector)

# Basic stripchart from the vector "len"
# ggplot2.stripchart(data=df, xName='dose',yName='len')

#change dot size
# ggplot2.stripchart(data=df, xName='dose',yName='len',
#                    size=3)

# Change the orientation: Horizontal stripchart
# ggplot2.stripchart(data=df, xName='dose',yName='len',
#                    orientation="horizontal")

# Stripchart with scatter
library(easyGgplot2)
library(ggplot2)
df = read.csv("LungWeight.txt", header = T, sep = "\t", stringsAsFactors = F)

treatments <- c("Paclitaxel", "Trametinib", "Paclitaxel + Trametinib", "Phenformin",
                "Paclitaxel + Carbo", "Paclitaxel + Trametinib + Carbo", "Everolimus", "Palbociclib")

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

# w15LW <- df$LungWeight[df$Treatment == "w15 untreated"]
# w18LW <- df$LungWeight[df$Treatment == "w18 untreated"]
# Torder <- c(rep("w15 untreated", length(w15LW)), rep("w18 untreated", length(w18LW)), Torder)
Torder <- c("w15 untreated", "w18 untreated", Torder)
# LWorder <- c(w15LW, w18LW, LWorder)

# plot data. 

# Change main title and axis titles
p1 <- ggplot2.stripchart(data=df, xName='Treatment', yName='LungWeight',
                   mainTitle="Plot of Lungweight by Treatment",
                   xtitle="Treatment", ytitle="Lung weight", addMean=TRUE, meanPointShape=21, meanPointSize=4,
                   meanPointColor="black", meanPointFill="RED")
# p1 <- ggplot2.stripchart(data=df, xName='Treatment', yName='LungWeight',
#                          addBoxplot=TRUE, addMean = TRUE)
# order the treatments as desired. 
p1 <- ggplot2.customize(p1, legendItemOrder = Torder)
# data_summary <- function(x) {
#   m <- mean(x)
#   ymin <- m-sd(x)
#   ymax <- m+sd(x)
#   return(c(y=m,ymin=ymin,ymax=ymax))
# }

# p1 + stat_summary(df=mean_sdl, mult=1, 
#                  geom="pointrange", color="red")
# p1 + stat_summary(fun.data=data_summary, color="blue")

# Remove plot legend
# ggplot2.customize(p1, showLegend = FALSE)

# # Change the order of items in the legend
# # legendItemOrder : character vector indicating 
# # the order of items in the legends.
# ggplot2.customize(p1, legendItemOrder = c("w15 untreated", "w18 untreated", "0.5"))
# # Remove plot legend
# ggplot2.customize(p1, showLegend = FALSE)  
# 
# # stripchart with notched box plot
# ggplot2.stripchart(data=df, xName='dose',yName='len',
#                    addBoxplot=TRUE,notch=TRUE)a