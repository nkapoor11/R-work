range <- c(1:5)
for (i in range) {
  iString <- toString(i)
  for (j in 1:25) {
    # import data file
    jString <- toString(j)
    chuanCode <- paste(c("L", iString, "-", jString), collapse = '')
    fileName <- c(chuanCode, "_BarCodeFinal.txt")
    fileName <- paste(fileName, collapse = '')
  
    # create data frame
    Data <- read.csv(fileName, header = T, sep = "\t", stringsAsFactors = F)

    Data <- Data[Data$CellNum > 500, ]
    sgIDAll <- Data$sgID

    sampleSize <- Data$CellNum
    dframe <- as.data.frame(cbind(sgIDAll, sampleSize))
    
    # write to excel file.
    write.csv(dframe, "sgID + cell size.csv")
  }
}