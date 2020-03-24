range <- c(1:1)
dframe <- c()
sgIDAll <- c()
for (i in range) {
  iString <- toString(i)
  for (j in 1:2) {
    # import data file
    jString <- toString(j)
    chuanCode <- paste(c("L", iString, "-", jString), collapse = '')
    fileName <- c(chuanCode, "_BarCodeFinal.txt")
    fileName <- paste(fileName, collapse = '')
    
    # create data frame
    Data <- read.csv(fileName, header = T, sep = "\t", stringsAsFactors = F)
    
    Data <- Data[Data$CellNum > 1000, ]
    # create row of sgIDs for first file, i = 1 and j = 1. 
    if (i == 1 && j == 1) {
      sgIDAll <- sort(unique(Data$sgID))
    #   # dframe <- c(sgIDAll)
      print("this only occur once")
    }
    
    # count num times each sgID occurs
    rowVectCount <- c()
    for (i in 1:length(sgIDAll)) {
      length <- length(Data$sgID[Data$sgID == sgIDAll[i]])
      rowVectCount <- c(rowVectCount, length)
      
    }
    
    # add the sample size count for this file to the matrix.
    dframe <- rbind(dframe, rowVectCount)

    
  }
}
# write to excel file.
# write.csv(dframe, "Sample size 1000.csv")
row.names(dframe) <- c(1:nrow(dframe))
colnames(dframe) <- sgIDAll
write.table(dframe, file=paste0("Sample_", 1000,"_Cells.txt"), append = FALSE, quote = FALSE, sep = "\t", row.names = T,
            col.names = TRUE)
print(dframe)