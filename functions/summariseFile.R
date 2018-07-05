# author - Taiyun Kim

library(dplyr)


################################################################################
# removeFactorsInColumn 
# 
# A helper function to remove factors in a matrix column
################################################################################
removeFactorsInColumn <- function(mat) {
  columns <- colnames(mat)
  for (i in 2:length(columns)) {
    mat[,columns[i]] <- as.numeric(as.character(mat[,columns[i]]))
  }
  return(mat)
}


################################################################################
# checkMissingFile 
# 
# A helper function to find the missing file from summarising
################################################################################
checkMissingFile <- function(mat, mlist) {
  
}


################################################################################
# summariseFile 
# 
# A helper function to summarise each text file (mean)
#
# NOTE: If you need to change the summary from mean to other (e.g. median),
#       Then create a parameter in this function and pass it to each file's 
#       summary functions (e.g. getAllPeptidesSummary) parameter 'valType'.
################################################################################
summariseFile <- function(inputFiles, instrumentNames) {
  sumFileTable <- inputFiles[["sumFileTable"]]
  allPepFileTable <- inputFiles[["allPepFileTable"]]
  msScansFileTable <- inputFiles[["msScansFileTable"]]
  evidenceFileTable <- inputFiles[["evidenceFileTable"]]
  
  # sumFileTable <- removeFactorsInColumn(sumFileTable)
  # allPepFileTable <- removeFactorsInColumn(allPepFileTable)
  # msScansFileTable <- removeFactorsInColumn(msScansFileTable)
  # evidenceFileTable <- removeFactorsInColumn(evidenceFileTable)
  
  sumFile.raw <- getSumFileSummary(sumFileTable)
  # total.num.files <- nrow(sumFile.raw) # summary file cannot have missing file (since no filtering is applied)
  
  allPepFile.mean <- getAllPeptidesSummary(allPepFileTable)
  # if (nrow(allPepFile.mean) < total.num.files) {
  #   
  # }
  
  msScans.mean <- getMsScansSummary(msScansFileTable)
  evidence.mean <- getEvidenceSummary(evidenceFileTable)
  
  # Find the files that are removed from filtering NA values and replace them back with NA
  allFileNames <- sumFile.raw$`Raw file`
  summarisedFileTables <- list(
    allPepFile.mean = allPepFile.mean,
    msScans.mean = msScans.mean,
    evidence.mean = evidence.mean
  )
  for (i in 1:length(ls(summarisedFileTables))) {
    curFile <- ls(summarisedFileTables)[i]
    if (length(which(!(allFileNames %in% summarisedFileTables[[curFile]]$`Raw file`))) > 0) {
      lostFileIndex <- which(!(allFileNames %in% summarisedFileTables[[curFile]]$`Raw file`))
      for (j in 1:length(lostFileIndex)) {
        summarisedFileTables[[curFile]] <- rbind(summarisedFileTables[[curFile]], c(allFileNames[lostFileIndex[j]], rep(NA, (ncol(summarisedFileTables[[curFile]])-1) )))
      }
    } 
  }

  allPepFile.mean <- summarisedFileTables$allPepFile.mean
  msScans.mean <- summarisedFileTables$msScans.mean
  evidence.mean <- summarisedFileTables$evidence.mean
  
  
  rownames(sumFile.raw) <- as.character(sumFile.raw$`Raw file`)
  rownames(allPepFile.mean) <- as.character(allPepFile.mean$`Raw file`)
  rownames(msScans.mean) <- as.character(msScans.mean$`Raw file`)
  rownames(evidence.mean) <- as.character(evidence.mean$`Raw file`)
  
  
  
  
  
  # sort all tables by filename
  sumFile.raw <- sumFile.raw[order(rownames(sumFile.raw)),]
  allPepFile.mean <- allPepFile.mean[order(rownames(allPepFile.mean)),]
  msScans.mean <- msScans.mean[order(rownames(msScans.mean)),]
  evidence.mean <- evidence.mean[order(rownames(evidence.mean)),]
  
  
  
  # combine all Features
  combinedTable <- cbind(sumFile.raw[,-1], allPepFile.mean[,-1], msScans.mean[,-1], evidence.mean[,-1])
  
  # add date column
  combinedTable <- cbind(combinedTable, substr(as.character(rownames(combinedTable)), 1, 8))
  colnames(combinedTable) <- c(colnames(combinedTable)[-ncol(combinedTable)], "Date")
  combinedTable$Date <- gsub("^([0-9]{4})([0-9]{2})([0-9]{2})$", "\\1-\\2-\\3", combinedTable$Date)
  
  combinedTable$Date <- as.POSIXct(combinedTable$Date)
  
  # add instrument column
  # inst <- rep("input", nrow(combinedTable))

  inst <- rep("unknown", nrow(combinedTable))
  for (i in 1:nrow(combinedTable)) {
    for (j in 1:length(instrumentNames)) {
      curInstrumentName <- gsub(paste("(.*_)(", tolower(instrumentNames[j]) , ")(_.*)", sep = ""), "\\2", tolower(as.character(rownames(combinedTable)[i])))
      if ( !is.na( match(tolower(curInstrumentName), tolower(instrumentNames[j])) ) ) {
        inst[i] <- instrumentNames[j]
        break
      } 
      # # only for testing
      # curInstrumentName <- gsub(paste("(.*_)(", "fusion" , ")(_.*)", sep = ""), "\\2", tolower(as.character(rownames(combinedTable)[i])))
      # if (curInstrumentName == "fusion") {
      #   inst[i] <- "FusionIT"
      # }
    }
  }
  
  combinedTable <- cbind(combinedTable, as.character(inst))
  colnames(combinedTable) <- c(colnames(combinedTable)[-ncol(combinedTable)], "Instrument")
  
  combinedTable <- cbind(combinedTable, rep("input", nrow(combinedTable)))
  colnames(combinedTable) <- c(colnames(combinedTable)[-ncol(combinedTable)], "Type")
  
  unknown.inst <- rownames(combinedTable)[which(inst == "unknown")]
  
  summary.result <- list(
    sumTable = combinedTable,
    unknown.inst = unknown.inst
  )
  return (summary.result)
  
}



################################################################################
# getSumFileSummary 
# 
# A helper function to select features from summary.txt
################################################################################
getSumFileSummary <- function(sumFileTable) {
  # features <- c("Peptide Sequences Identified", "MS/MS Identified [%]", "Mass Standard Deviation [ppm]", "MS/MS Identified", "Peaks Repeatedly Sequenced [%]", "MS/MS")
  # sumFileFeatures.raw <- getFeatures(sumFileTable, features)
  
  # So far, 2nd column is the "Experiment" OR "Enzyme" which should select only the exact input files data
  sumFileTable <- sumFileTable[which(sumFileTable[,2] != ""),]
  sumFileTable <- sumFileTable[,-2]
  sumFile.raw <- removeFactorsInColumn(sumFileTable)
  # sumFileFeatures.raw.filtered <- na.omit(sumFile.raw)
  # return (sumFileFeatures.raw.filtered)
  return (sumFile.raw)
}



################################################################################
# getAllPeptidesSummary 
# 
# A helper function to select features and summarise allPeptides.txt
################################################################################
getAllPeptidesSummary <- function(allPepFileTable, valType = "mean") {
  # select features
  # features <- c("Raw file", "Retention length (FWHM)", "Sequence", "Score", "Intensity", "Mass precision [ppm]")
  allPepFileFeatures.raw <- getFeatures(allPepFileTable, name = "allPeptides")
  
  allPepFileFeatures.raw <- allPepFileFeatures.raw[,-4]
  # omit rows with NA
  allPepFileFeatures.raw <- na.omit(allPepFileFeatures.raw)
  
  
  # summarise
  allPepFileFeatures.raw.dat <- group_by(allPepFileFeatures.raw, `Raw file`)
  
  
  if (valType == "mean") {
    allPepFileAllFeatures.mean <- summarise_all(
      allPepFileFeatures.raw.dat,
      mean
    )
    return(allPepFileAllFeatures.mean) 
  } else if (valType == "median") {
    allPepFileAllFeatures.median <- summarise_all(
      allPepFileFeatures.raw.dat,
      median
    )
    return(allPepFileAllFeatures.median)
  }
}



################################################################################
# getMsScansSummary 
# 
# A helper function to select features and summarise msScans.txt
################################################################################
getMsScansSummary <- function(msScansFileTable, valType = "mean") {
  # features <- c("Raw file", "Cycle time")
  # msScansFeatures.raw <- getFeatures(msScansFileTable, features)
  msScansFileTable <- removeFactorsInColumn(msScansFileTable)
  msScansFeatures.raw <- msScansFileTable[!is.na(msScansFileTable$`Cycle time`),]
  
  # # remove the bad file 20170915_QECl_DL_Hela01
  # msScansFeatures.raw <- msScansFeatures.raw[-which(msScansFeatures.raw$`Raw file` == "20170915_QECl_DL_Hela01"),]
  
  msScansFeatures.raw.dat <- group_by(msScansFeatures.raw, `Raw file`)
  
  if (valType == "mean") {
    msScansFeatures.raw.mean <- summarise_all(
      msScansFeatures.raw.dat,
      mean
    )
    return (msScansFeatures.raw.mean)
  } else if (valType == "median") {
    msScansFeatures.raw.median <- summarise_all(
      msScansFeatures.raw.dat,
      median
    )
    return (msScansFeatures.raw.median)
  }
  

}



################################################################################
# getEvidenceSummary 
# 
# A helper function to select features and summarise evidence.txt
################################################################################
getEvidenceSummary <- function(evidenceFileTable, valType = "mean") {
  # features = c("Raw file", "Mass Error [ppm]")
  # evidenceFileFeature.raw <- getFeatures(evidenceFileTable, features)
  evidenceFileTable <- removeFactorsInColumn(evidenceFileTable)
  evidenceFileFeature.raw <- evidenceFileTable[!is.na(evidenceFileTable$`Mass Error [ppm]`),]
  
  # summarise
  evidenceFileFeature.raw.dat <- group_by(evidenceFileFeature.raw, `Raw file`)
  
  
  if (valType == "mean") {
    evidenceFileFeature.raw.mean <- summarise_all(
      evidenceFileFeature.raw.dat,
      mean
    )
    return (evidenceFileFeature.raw.mean)
  } else if (valType == "median") {
    evidenceFileFeature.raw.median <- summarise_all(
      evidenceFileFeature.raw.dat,
      median
    )
    return (evidenceFileFeature.raw.median)
  }
  

}


### select features from a table
################################################################################
# getFeatures 
# 
# A helper function to select features. This is currently (November 2017) only used
# for allPeptides.txt
################################################################################
getFeatures <- function(featTable, name = NULL) {
  # featTable <- targetTable[,features]
  if (name == "allPeptides") {
    featTable <- featTable[featTable$Sequence != " ", ]
    featTable <- removeFactorsInColumn(featTable)
    featTable <- featTable[!is.nan(featTable$`Retention length (FWHM)`),]
  }
  # colnames(featTable) <- features
  # rownames(featTable) <- as.character(featTable$`Raw file`)
  return (featTable)
}
