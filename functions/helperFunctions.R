################################################################################
# readData 
# 
# It reads the CSV file (database). It also sets the "Date" as a R Date format
################################################################################
readData <- function() {
  allFeatures <- read.csv(paste0(getwd(), "/allFeatures.csv", sep = ""), row.names = 1, check.names = FALSE)
  allFeatures$Date <- as.POSIXct(as.character(allFeatures$Date))
  
  return(allFeatures)
}


################################################################################
# DEBUG - separateBatches 
# 
# This is only for debugging. This will not be necessary when user input is implemented
# This separate to 2 batches to simulate batch.2 as input batch
################################################################################
separateBatches <- function(allFeatures) {
  
  batch.1 <- allFeatures[1:220,]
  batch.2 <- allFeatures[221:259,]
  batch.2$Instrument <- "input"
  
  # Create dates
  batch.1$Date <- as.Date(batch.1$Date)
  batch.2$Date <- as.Date(batch.2$Date)
  
  batch.1 <- batch.1[order(batch.1$Date),]
  batch.2 <- batch.2[order(batch.2$Date),]
  
  
  return (list(batch.1 = batch.1, batch.2 = batch.2))
}
