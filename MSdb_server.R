Sys.setlocale("LC_ALL","C")


# msDbServer <- function() {
source("./functions/summariseFile.R")
source("./functions/MS_db_update.R")

msDbServer <-  function(input,output,session) {
    
  # instrumentNames <- read.csv("allFeatures.csv", colClasses = c(rep("NULL", 14), "character"), check.names = F)[,1]
  # instrumentNames <- unique(instrumentNames)
  
  instrumentNames <- reactive({
    instNames <- strsplit(input$ms_dbInstName, split = ",")[[1]]
    instNames <- trimws(instNames)
  })
  
  fileInDB <- reactiveValues()
  
  
  
  dbInputValidate <- reactive({
    dbInputFiles <- input$ms_dbInput
    shiny::validate(
      shiny::need(
        (!is.null(dbInputFiles$name)),
        "Please upload zipped files of txt output folder."
      )
    )
  })
    
  output$ms_dbInputWarning <- renderText({
    dbInputValidate()
  })
    
    
    
  summariseInputDB <- reactive({
      
      sumFileHeaders <- read.delim2(fileInDB$path[which(fileInDB$name == "summary.txt")], sep = "\t", header = TRUE, check.names = FALSE, nrow = 1)
      sumFileHeadersIndex <- which(colnames(sumFileHeaders) %in% c("Raw file", "Enzyme", "MS/MS", "MS/MS Identified", "MS/MS Identified [%]", 
                                                                   "Mass Standard Deviation [ppm]", "Peaks Repeatedly Sequenced [%]", "Peptide Sequences Identified"))
      
      allPepHeaders <- read.delim2(fileInDB$path[which(fileInDB$name == "allPeptides.txt")], sep = "\t", header = TRUE, check.names = FALSE, nrow = 1)
      allPepHeadersIndex <- which(colnames(allPepHeaders) %in% c("Raw file", "Intensity", "Mass precision [ppm]", "Retention length (FWHM)", "Score", "Sequence"))
      
      
      msScansHeaders <- read.delim2(fileInDB$path[which(fileInDB$name == "msScans.txt")], sep = "\t", header = TRUE, check.names = FALSE, nrow = 1)
      msScansHeadersIndex <- which(colnames(msScansHeaders) %in% c("Raw file", "Cycle time"))
      
      
      evidenceHeaders <- read.delim2(fileInDB$path[which(fileInDB$name == "evidence.txt")], sep = "\t", header = TRUE, check.names = FALSE, nrow = 1)
      evidenceHeadersIndex <- which(colnames(evidenceHeaders) %in% c("Raw file", "Mass Error [ppm]"))
      
      
      # [ncol == 52] Raw file (1), Experiment(2), MS/MS(20), MS/MS Identified(26), MS/MS Identified [%](30), Peptide Sequences Identified(34), Peaks Repeatedly Sequenced [%](39), Mass Standard Deviation [ppm](49))
      sumFileTable <- read.delim2(fileInDB$path[which(fileInDB$name == "summary.txt")], sep = "\t", header = TRUE, check.names = FALSE,
                                  colClasses = unlist( lapply(1:ncol(sumFileHeaders), function(i) {if(i %in% sumFileHeadersIndex) {"character"} else {"NULL"} } ) ))
      
      # [ncol == 33] Raw file(1), Mass Precision [ppm](14), FWHM(18), Sequences(23), Score(28), Intensity(29) 
      allPepFileTable <- read.delim2(fileInDB$path[which(fileInDB$name == "allPeptides.txt")], sep = "\t", header = TRUE, check.names = FALSE,
                                     colClasses = unlist( lapply(1:ncol(allPepHeaders), function(i) {if(i %in% allPepHeadersIndex) {"character"} else {"NULL"} } ) ))
      
      # [ncol == 29] Raw file(1), Cycle time(5)
      msScansFileTable <- read.delim2(fileInDB$path[which(fileInDB$name == "msScans.txt")], sep = "\t", header = TRUE, check.names = FALSE,
                                      colClasses = unlist( lapply(1:ncol(msScansHeaders), function(i) {if(i %in% msScansHeadersIndex) {"character"} else {"NULL"} } ) ))
      # Raw file, Mass error [ppm]
      # evidenceFileTable <- read.delim2(fileInDB$path[which(fileInDB$name == "evidence.txt")], sep = "\t", header = TRUE, check.names = FALSE,
      #                                  colClasses = c(rep("NULL", 15), "character", rep("NULL", 9), "character", rep("NULL", 37)))
      evidenceFileTable <- read.delim2(fileInDB$path[which(fileInDB$name == "evidence.txt")], sep = "\t", header = TRUE, check.names = FALSE,
                                       colClasses = unlist( lapply(1:ncol(evidenceHeaders), function(i) {if(i %in% evidenceHeadersIndex) {"character"} else {"NULL"} } ) ))
      
      
      summary.result <- summariseFile(
        list(
          sumFileTable = sumFileTable,
          allPepFileTable = allPepFileTable,
          msScansFileTable = msScansFileTable,
          evidenceFileTable = evidenceFileTable
        ),
        instrumentNames()
      )
      combinedFile <- summary.result[["sumTable"]]
      if (length(summary.result$unknown.inst) > 0) {
        showModal(
          modalDialog(
            title = HTML('<span style="color:red; font-size: 20px; font-weight:bold; font-family:sans-serif ">Sorry, some of your file instruments are not in our database<span>
                         <button type = "button" class="close" data-dismiss="modal" ">
                         <span style="color:white; "><span>
                         </button> '),
            # "Update success!",
            h3("The following files are removed from the plots."),
            
            do.call(p, c(lapply(1:length(summary.result$unknown.inst), function(i) {
              p(summary.result$unknown.inst[i])
            }))),
            
            easyClose = TRUE,
            footer = NULL
            )
          )
      }
      
      
      
      return (combinedFile)
    })
    
    
    
    
  # does not check for files at this stage
  newDBInput <- reactive({
      dbInputFiles <- input$ms_dbInput
      if (!is.null(dbInputFiles$name)) {
        dbInputMat <- NULL
        for (i in 1:length(dbInputFiles$name)) {
          outputDir <- gsub("^(.*/)([0-9]+\\.zip$)", "\\1", dbInputFiles$datapath[i]) # Assume the filename is alwasy 0.zip
          
          # extraction to temporary directory does not work on windows machine.
          # This may be due to os directory structuture
          # (outputDir cannot be found when tried to access with exdir in windows)
          unzippedFolderDir <- unzip(dbInputFiles$datapath[i], exdir = outputDir)
          unzippedFolderName <- gsub("(.*/)(.*\\.txt)$", "\\2", unzippedFolderDir)
          
          fileInDB$path <- unzippedFolderDir
          fileInDB$name <- unzippedFolderName
          # validate correct file names inside zipped folder
          mat <- summariseInputDB()
          mat$Type <- "DB"
          if (is.null(dbInputMat)) {
            dbInputMat <- mat
          } else {
            dbInputMat <- rbind(dbInputMat, mat)
          }
        }
        dbInputMat
      }
    })
    
    # need to validate database name?
  output$ms_dbCreate <- downloadHandler(
    filename = function(){paste(input$ms_dbName, ".csv", sep = "")},
    content = function(fname) {
      tmp <- newDBInput()
      write.csv(tmp, fname)
    }
  )
}
# }
