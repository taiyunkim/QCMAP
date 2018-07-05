library(glmnet)
source("./functions/helperFunctions.R")

mstestServer <- function(input, output, session) {
  mstest_selected_inst <- reactiveValues()
  mstest_fileIn <- reactiveValues()
  # mstest_db <- reactiveValues(instNames = NULL, path = NULL, file = NULL)
  
  instrumentNames <- read.csv("allFeatures.csv", colClasses = c(rep("NULL", 14), "character"), check.names = F)[,1]
  instrumentNames <- unique(instrumentNames)
  
  
  mstest_fileInList <- reactive({
    fileObj <- input$mstest_maxQuantFiles
    
    shiny::validate(
      shiny::need(
        (!is.null(fileObj$name)),
        "Please upload your txt files(/zipped folder)."
      )
    )
    
    if (grepl("*\\.txt$", fileObj$name[1])) {
      mstest_fileIn$name[[1]] <- fileObj$name
      mstest_fileIn$path[[1]] <- fileObj$datapath
    } else if (grepl(".*\\.zip$",fileObj$name[1])) {
      # print("here name start")
      # print(fileObj$name)
      # print("here name end, datapath start")
      # print(fileObj$datapath)
      # print("here datapath end")
      for (i in 1:length(fileObj$name)) {
        # if (length(fileObj$name) == 1) { # zipped folder
          outputDir <- gsub("^(.*/[0-9]+)(\\.zip$)", "\\1", fileObj$datapath[i]) # Assume the filename is alwasy 0.zip
          
          # extraction to temporary directory does not work on windows machine. 
          # This may be due to os directory structuture 
          # (outputDir cannot be found when tried to access with exdir in windows)
          unzippedFolderDir <- unzip(fileObj$datapath[i], exdir = paste(outputDir, "/", sep = ""))
          unzippedFolderName <- gsub("(.*/)(.*\\.txt)$", "\\2", unzippedFolderDir)
          mstest_fileIn$path[[i]] <- unzippedFolderDir
          mstest_fileIn$name[[i]] <- unzippedFolderName
        # } 
      }
    } 
  })
  
  # summarise input file
  mstest_summariseInput <- reactive({
    # mstest_readData()
    mstest_fileInList()
    # print("start path")
    # print(mstest_fileIn$path)
    # print("end path, start name")
    # print(mstest_fileIn$name)
    # print("end name")
    for (i in 1:length(mstest_fileIn$name)) {
      mstest_sumFileHeaders <- read.delim2(mstest_fileIn$path[[i]][which(mstest_fileIn$name[[i]] == "summary.txt")], sep = "\t", header = TRUE, check.names = FALSE, nrow = 1)
      mstest_sumFileHeadersIndex <- which(colnames(mstest_sumFileHeaders) %in% c("Raw file", "Enzyme", "MS/MS", "MS/MS Identified", "MS/MS Identified [%]", 
                                                                                 "Mass Standard Deviation [ppm]", "Peaks Repeatedly Sequenced [%]", "Peptide Sequences Identified"))
      
      mstest_allPepHeaders <- read.delim2(mstest_fileIn$path[[i]][which(mstest_fileIn$name[[i]] == "allPeptides.txt")], sep = "\t", header = TRUE, check.names = FALSE, nrow = 1)
      mstest_allPepHeadersIndex <- which(colnames(mstest_allPepHeaders) %in% c("Raw file", "Intensity", "Mass precision [ppm]", "Retention length (FWHM)", "Score", "Sequence"))
      
      
      mstest_msScansHeaders <- read.delim2(mstest_fileIn$path[[i]][which(mstest_fileIn$name[[i]] == "msScans.txt")], sep = "\t", header = TRUE, check.names = FALSE, nrow = 1)
      mstest_msScansHeadersIndex <- which(colnames(mstest_msScansHeaders) %in% c("Raw file", "Cycle time"))
      
      
      mstest_evidenceHeaders <- read.delim2(mstest_fileIn$path[[i]][which(mstest_fileIn$name[[i]] == "evidence.txt")], sep = "\t", header = TRUE, check.names = FALSE, nrow = 1)
      mstest_evidenceHeadersIndex <- which(colnames(mstest_evidenceHeaders) %in% c("Raw file", "Mass Error [ppm]"))
      
      
      # [ncol == 52] Raw file (1), Experiment(2), MS/MS(20), MS/MS Identified(26), MS/MS Identified [%](30), Peptide Sequences Identified(34), Peaks Repeatedly Sequenced [%](39), Mass Standard Deviation [ppm](49))
      mstest_sumFileTable <- read.delim2(mstest_fileIn$path[[i]][which(mstest_fileIn$name[[i]] == "summary.txt")], sep = "\t", header = TRUE, check.names = FALSE,
                                         colClasses = unlist( lapply(1:ncol(mstest_sumFileHeaders), function(i) {if(i %in% mstest_sumFileHeadersIndex) {"character"} else {"NULL"} } ) ))
      
      # [ncol == 33] Raw file(1), Mass Precision [ppm](14), FWHM(18), Sequences(23), Score(28), Intensity(29) 
      mstest_allPepFileTable <- read.delim2(mstest_fileIn$path[[i]][which(mstest_fileIn$name[[i]] == "allPeptides.txt")], sep = "\t", header = TRUE, check.names = FALSE,
                                            colClasses = unlist( lapply(1:ncol(mstest_allPepHeaders), function(i) {if(i %in% mstest_allPepHeadersIndex) {"character"} else {"NULL"} } ) ))
      
      # [ncol == 29] Raw file(1), Cycle time(5)
      mstest_msScansFileTable <- read.delim2(mstest_fileIn$path[[i]][which(mstest_fileIn$name[[i]] == "msScans.txt")], sep = "\t", header = TRUE, check.names = FALSE,
                                             colClasses = unlist( lapply(1:ncol(mstest_msScansHeaders), function(i) {if(i %in% mstest_msScansHeadersIndex) {"character"} else {"NULL"} } ) ))
      # Raw file, Mass error [ppm]
      # evidenceFileTable <- read.delim2(fileIn$path[which(fileIn$name == "evidence.txt")], sep = "\t", header = TRUE, check.names = FALSE,
      #                                  colClasses = c(rep("NULL", 15), "character", rep("NULL", 9), "character", rep("NULL", 37)))
      mstest_evidenceFileTable <- read.delim2(mstest_fileIn$path[[i]][which(mstest_fileIn$name[[i]] == "evidence.txt")], sep = "\t", header = TRUE, check.names = FALSE,
                                              colClasses = unlist( lapply(1:ncol(mstest_evidenceHeaders), function(i) {if(i %in% mstest_evidenceHeadersIndex) {"character"} else {"NULL"} } ) ))
      if (i == 1) {
        mstest_sumFileTable_combined <- mstest_sumFileTable
        mstest_allPepFileTable_combined <- mstest_allPepFileTable
        mstest_msScansFileTable_combined <- mstest_msScansFileTable
        mstest_evidenceFileTable_combined <- mstest_evidenceFileTable
      } else {
        mstest_sumFileTable_combined <- rbind(mstest_sumFileTable_combined, mstest_sumFileTable)
        mstest_allPepFileTable_combined <- rbind(mstest_allPepFileTable_combined, mstest_allPepFileTable)
        mstest_msScansFileTable_combined <- rbind(mstest_msScansFileTable_combined, mstest_msScansFileTable)
        mstest_evidenceFileTable_combined <- rbind(mstest_evidenceFileTable_combined, mstest_evidenceFileTable)
      }
    }
    mstest_summary.result <- summariseFile(
      list(
        sumFileTable = mstest_sumFileTable_combined,
        allPepFileTable = mstest_allPepFileTable_combined,
        msScansFileTable = mstest_msScansFileTable_combined,
        evidenceFileTable = mstest_evidenceFileTable_combined
      ),
      instrumentNames
    )
    mstest_combinedFile <- mstest_summary.result[["sumTable"]]
    if (length(mstest_summary.result$unknown.inst) > 0) {
      showModal(
        modalDialog(
          title = HTML('<span style="color:red; font-size: 20px; font-weight:bold; font-family:sans-serif ">Sorry, some of your file instruments are not in our database<span>
                       <button type = "button" class="close" data-dismiss="modal" ">
                       <span style="color:white; "><span>
                       </button> '),
          # "Update success!",
          h3("The following files are removed from the plots."),
          
          do.call(p, c(lapply(1:length(mstest_summary.result$unknown.inst), function(i) {
            p(mstest_summary.result$unknown.inst[i])
          }))),
          
          easyClose = TRUE,
          footer = NULL
          )
        )
    }
    
    # mstest_combinedFile <- checkDuplicateDates(mstest_combinedFile)
    mstest_combinedFile
    # return (combinedFile)
  })
  
  # check if correct files are uploaded
  mstest_validateFileNames <- reactive({
    fileNameList <- c("summary.txt", "allPeptides.txt", "msScans.txt", "evidence.txt")
    shiny::validate(
      need(
        (all(fileNameList %in% mstest_fileIn$name) & (length(mstest_fileIn$name) >= length(fileNameList) ) ),
        "Please choose the correct files"
      )
    )
  })
  output$mstest_validationMessage <- renderText({
    mstest_fileInList()
    mstest_validateFileNames()
    
  })
  
  # [not done so far for input data] switch to tabs from input file instrument
  observe({
    updateTabsetPanel(
      session,
      "t2",
      selected = mstest_fileIn$batch.2$Instrument[1]
    )
  })
  
  
  observe({
    withProgress(message = "Reading your input database ...", value = 0, {
      # mstest_readData()
      incProgress(1/4, message = "Validating txt files ...")
      if (input$ms_pred_example) {
        fileObj <- list()
        fileObj$name <- "mouse_brain.csv"
        fileObj$datapath <- paste(getwd(), "/www/data/", fileObj$name, sep = "")
        
        mstest_fileIn$path <- fileObj$datapath
        mstest_fileIn$name <- fileObj$name
        mstest_combinedInputFile <- read.csv(mstest_fileIn$path, check.names = F, row.names = 1)
        mstest_combinedInputFile$Date <- as.POSIXct(as.character(mstest_combinedInputFile$Date))
      } else {
        mstest_fileInList()
        incProgress(1/4, message = "Reading and summarising txt files ...")
        mstest_combinedInputFile <- mstest_summariseInput()
      }
      mstest_batch.2 <- mstest_combinedInputFile
      mstest_batch.2 <- mstest_batch.2[order(mstest_batch.2$Date),]
      mstest_dat.batch2 <- data.frame(
        date = c(mstest_batch.2$Date),
        pep = c(mstest_batch.2$`Peptide Sequences Identified`),
        batch = c(rep("input batch", nrow(mstest_batch.2))),
        name = c(rownames(mstest_batch.2)),
        instrument = c(as.character(mstest_batch.2$Instrument)),
        type = c(as.character(mstest_batch.2$Type))
      )
      incProgress(1/4, message = "Plotting ...")
    })
    mstest_fileIn$batch.2 <- mstest_batch.2
    mstest_fileIn$dat.batch2 <- mstest_dat.batch2
  })
  
  
  # Create tabs dynamically
  output$mstest_outputUI <- renderUI({
    if(!input$ms_pred_example) {
      combinedFiles <- mstest_summariseInput()
    } else {
      combinedFiles <- mstest_fileIn$batch.2
    }
    # combinedFiles <- mstest_summariseInput()
    instrumentNames <- unique(combinedFiles$Instrument)
    
    do.call(
      tabsetPanel, c(
        id = "t2",
        lapply(1:length(instrumentNames), function(i) {
          tabPanel(
            title = instrumentNames[i],
            br(),
            plotlyOutput(paste0("mstest_scatter_", instrumentNames[i], sep = "")),
            # br(),
            # hr(),
            # br(),
            # plotlyOutput(paste0("mstest_bar_", instrumentNames[i], "_history", sep = "")),
            br(),
            # h3(textOutput(paste0("score_",instrumentNames[i],"_check",sep=""))),
            # br(),
            hr(),
            br(),
            #textOutput(paste0("score_",instrumentNames[i],"_check",sep="")),
            plotlyOutput(paste0("mstest_box_", instrumentNames[i], sep = ""), height = "500px")
          )
        })
        
        # list(
        #   tabPanel(
        #     title = "Comparisons",
        #     plotlyOutput("bar_plotHistory"),
        #     plotlyOutput("box_individualFeatures", height = "1000px")
        #   )
        # )
      )
    )
    
    
    
  })
  
  
  # scatter plot
  lapply(1:length(instrumentNames), function(i) {
    output[[paste0("mstest_scatter_", instrumentNames[i], sep = "")]] <- renderPlotly({
      
      this.instrument <- instrumentNames[i]
      
      # Select input data
      batch.2 <- mstest_fileIn$batch.2
      batch.2 <- batch.2[order(batch.2$Date),]
      standard_input_Instrument <- batch.2[which(batch.2$Instrument == this.instrument),1:12] 
      # View(standard_input_Instrument)
      
      
      
      # Read the database
      allFeatures <- readData()
      batch.1 <- allFeatures
      batch.1 <- batch.1[order(batch.1$Date),]
      instrument_DB <- batch.1[which(batch.1$Instrument == this.instrument),1:12]
      
      
      ################################################################################
      # IRENE's CODE
      ################################################################################
      
      X.raw_Instrument <- instrument_DB[,2:12]
      y_Instrument <- instrument_DB[,1]
      #boxplot(y_Instrument)
      #boxplot(X.raw_Instrument)
      
      ##########################################################
      #find and remove the high leverage point
      
      selecInstrument_leverage <- instrument_DB[,1:12]
      
      # colnames(X.raw_Instrument) <- c("MS.MS.Identifiedp", "MassStandardDevisationppm","MS.MS.Identified","PeaksRepeatedlySequencedp" ,"MS.MS" ,"Retention.length.FWHM","Score","Intensity","MassPrecision.ppm","CycleTime","MassError")
      results_Instrument <- lm(y_Instrument ~ `MS/MS Identified [%]` + `Mass Standard Deviation [ppm]` + `MS/MS Identified` + `Peaks Repeatedly Sequenced [%]` + `MS/MS` + `Retention length (FWHM)` + `Score` + `Intensity` + `Mass precision [ppm]` + `Cycle time` + `Mass Error [ppm]`, data=X.raw_Instrument)
      D<- cooks.distance(results_Instrument)
      
      #no of Feature(parametrs) / no of observers (p/n)
      
      #find the cut off value, identify the leverage datasets, then remove them
      HL<- as.matrix(which (D> qf(0.5,11,nrow(selecInstrument_leverage)-11)), rownames.force = NA)
      
      removeName <- as.list(row.names(HL)) 
      rownames(HL)<- row.names(HL)
      remove <- rownames(HL)
      
      #REMOVE the high leverage points from the datasets
      selectInstrument_remove2 <- selecInstrument_leverage[!rownames(instrument_DB)%in% remove,]
      
      
      # normalise the data to 1#
      X_Instrument_remove <- selectInstrument_remove2[,2:12]
      
      y_Instrument_remove <- selectInstrument_remove2[,1]
      ###########################################################
      #standardize the values of 11 features####################
      median_features <- apply(X.raw_Instrument, 2, FUN = median)
      
      median_features <- t(median_features)
      
      median_features <- as.data.frame(median_features)
      
      
      # assumes that  the column order of "median_features" and "X_Instrument_remove" are identical
      standard_x_Instrument <- X_Instrument_remove
      for (i in 1:ncol(X_Instrument_remove)) {
        standard_x_Instrument[,i] <- standard_x_Instrument[,i]/median_features[1,i]
      }
      scaled_y_Instrument_re<- (y_Instrument_remove - min(y_Instrument_remove))/(max(y_Instrument_remove)-min(y_Instrument_remove))
      
      
      
      set.seed(1)
      cv.out_Instrument_stand <- cv.glmnet(x=as.matrix(standard_x_Instrument), y= scaled_y_Instrument_re,alpha=0, nfolds=5)
      
      ridge.mod_Instrument_stand <- glmnet (x=as.matrix(standard_x_Instrument), y=scaled_y_Instrument_re,alpha = 0)
      
      # scale input X features and normalise input Y value
      # standard_input_Instrument_x <- standard_input_Instrument[,which(colnames(standard_input_Instrument) != "Peptide Sequences Identified")]
      standard_input_Instrument_x <- standard_input_Instrument[,colnames(standard_x_Instrument)]
      for (i in 1:ncol(standard_input_Instrument_x)) {
        standard_input_Instrument_x[,i] <- standard_input_Instrument_x[,i]/median_features[1,i]
      }
      # View(median_features)
      # View(standard_input_Instrument_x)
      # View(standard_x_Instrument)
      ridge.pred_Instrument_stand_Mouse <- predict(ridge.mod_Instrument_stand,s = cv.out_Instrument_stand$lambda.min, newx= as.matrix(standard_input_Instrument_x))
      # ridge.pred_Instrument_stand_Hela <- predict(ridge.mod_Instrument_stand,s = cv.out_Instrument_stand$lambda.min, newx= as.matrix(standard_x_Instrument))
      
      # ridge.coef_Instrument <- predict(ridge.mod_Instrument_stand, type="coefficients",s=cv.out_Instrument_stand$lambda.min)
      
      # comparey_stand <- cbind(ridge.pred_Isntrument_stand[,1],y_Isntrument_mouse)
      # 
      # #View(comparey_stand)
      # corY_stand <- cor(comparey_stand,method = "spearman")
      
      
      
      
      ################################################################################
      # END OF IRENE's CODE
      ################################################################################
      predict_result <- data.frame(
        prediction = ridge.pred_Instrument_stand_Mouse[,1],
        raw_peptide_num = standard_input_Instrument[,which(colnames(standard_input_Instrument) == "Peptide Sequences Identified")],
        names = rownames(standard_input_Instrument)
      )
      fit <- lm(round(prediction, digits = 3) ~ raw_peptide_num, data = predict_result)
      # View(predict_result)
      plot_ly(predict_result, x = ~raw_peptide_num, y = ~round(prediction, digits = 3), type = "scatter", text =~names, key = ~names, source = "mstest_pred_plot") %>%
        layout(
          # showlegend = T,
          title = "Score prediction",
          yaxis = list(title = "QC score predicted"),
          xaxis = list(title = "Peptide")
        ) %>%
        add_lines(x = ~raw_peptide_num, y = fitted(fit), hoverinfo = "none")
      
      
    })
  })
  
  # bar plot
  lapply(1:length(instrumentNames), function(i) {
    output[[paste0("mstest_bar_", instrumentNames[i], "_history", sep = "")]] <- renderPlotly({
      this.instrument <- instrumentNames[i]
      # msuser_readData()
      
      # dat.batch2 <- mstest_fileIn$dat.batch2
      batch.2 <- mstest_fileIn$batch.2
      batch.2 <- batch.2[batch.2$Instrument == this.instrument,]
      dat.batch2 <- data.frame(
        date = c(as.POSIXct(batch.2$Date)),
        pep = c(batch.2$`Peptide Sequences Identified`),
        batch = c(rep("Database", nrow(batch.2))),
        name = c(rownames(batch.2)),
        instrument = c(as.character(batch.2$Instrument)),
        type = c(as.character(batch.2$Type))
      )
      
      
      goodColors <- c("#8B4513", "#98FFCC", "#0000FF", "#808080", "#FF00FF", "#f88379", "#9400D3")
      p <- plot_ly(dat.batch2, x = ~date, source = "mstest_peptideNumPlot", y = ~pep, text = ~name, name = ~name, key = ~instrument, showlegend = T, color = ~instrument, colors = "#808080", type = "bar") %>%
        layout(
          barmode = 'group',
          title = paste0("[", this.instrument, "] Peptide Sequences Identified", sep = ""),
          showlegend = T,
          xaxis = list(
            title = "Date",
            rangeselector = list(
              buttons = list(
                list(
                  count = 7,
                  label = "1W",
                  step = "day",
                  stepmode = "backward"),
                list(
                  count = 14,
                  label = "2W",
                  step = "day",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "1M",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 3,
                  label = "3M",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 6,
                  label = "6M",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "1Y",
                  step = "year",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "YTD",
                  step = "year",
                  stepmode = "todate"),
                list(step = "all")
              )
            ),
            rangeslider = list(type = "date")
          ),
          
          
          yaxis = list(title = "Peptide Sequences Identified")
        ) %>%
        rangeslider(start = (as.POSIXct(tail(dat.batch2$date,1)) - 1296000), end = as.POSIXct(tail(dat.batch2$date,1)))
      
      # # if (ms_runReader$run == T) {
      # dat.batch2 <- msuser_fileIn$dat.batch2
      # if (any(dat.batch2$instrument == this.instrument)) {
      #   dat.batch2 <- dat.batch2[dat.batch2$instrument == this.instrument,]
      #   p <- p %>%
      #     add_bars(data = dat.batch2, x = ~date, y = ~pep, key = ~type,
      #              marker = list(color = "#e60000"), text = ~name,
      #              name = ~instrument, color = ~name, showlegend = T, inherit = T)
      #   
      #   p <- p %>%
      #     rangeslider(start = (as.POSIXct(tail(dat.batch2$date,1)) - 1296000), end = as.POSIXct(tail(dat.batch2$date,1)))
      #   # }
      # }
      
      # thresholdData <- paste(getwd(), "/thresholds.txt", sep = "")
      # if (!file.exists(thresholdData)) {
      #   thresholdData = NA
      # }
      # if (!is.na(thresholdData)) {
      #   inst_threshold$present = T
      #   tmp <- read.delim(thresholdData, check.names = F, header = T)
      #   for (i in 1:nrow(tmp)) {
      #     inst_threshold[[as.character(tmp[i,1])]] <- as.numeric(tmp[i,2])
      #   }
      # }
      # if (inst_threshold$present == T) {
      #   if (ms_runReader$run == T) {
      #     p <- p %>%
      #       add_lines(x = c(dat$date, dat.batch2$date, tail(dat.batch2$date,1)+86400), y = inst_threshold[[this.instrument]], name = paste(this.instrument, "<br />threshold", sep = ""),
      #                 key = "threshold", line = list(color = "#ff7f50"), hoverinfo = "none", showlegend = T, inherit = F)
      #   } else {
      #     p <- p %>%
      #       add_lines(y = inst_threshold[[this.instrument]], name = "threshold",
      #                 key = "threshold", line = list(color = "#ff7f50"), hoverinfo = "none", showlegend = T, inherit = T)
      #   }
      #
      # }
      # commentData <- paste(getwd(), "/allComments.csv", sep = "")
      # if (!file.exists(commentData)) {
      #   commentData <- NA
      # }
      # if (!is.na(commentData)) {
      #   comment_text$present = T
      #
      #   tmp <- comment_fileReader()
      #   # tmp <- read.csv(commentData, check.names = F, header = T)
      #   # remove numbering
      #   tmp <- tmp[,-1]
      #   tmp$datetime <- as.POSIXct(tmp$datetime)
      #   this.comment <- tmp[tmp$Instrument == this.instrument,]
      #   if (nrow(this.comment) > 0) {
      #     p <- p %>%
      #       add_markers(x = this.comment$datetime, y = 0, hoverinfo = "text", showlegend = T, inherit = F, name = paste(this.instrument, "<br />comments", sep = ""),
      #                   key = "comment", hovertext = ~this.comment$Comment, marker = list(color = ~this.comment$Colour, symbol = "x", size = 10))
      #   }
      # }
      # p %>%
      #   onRender(jsCode = JS("comment_javascript"))
      p %>%
        onRender(jsCode = JS("javascript"))
      
      # layout(barmode = "overlay")
    })
  })
  
  
  # box plot
  lapply(1:length(instrumentNames), function(i) {
    output[[paste0("mstest_box_", instrumentNames[i], sep = "")]] <- renderPlotly({
      this.Instrument = instrumentNames[i]
      s <- event_data("plotly_click", source = "mstest_pred_plot")
      
      ####################################################################
      allFeatures <- readData()

      batch.1 <- allFeatures
      batch.1 <- batch.1[order(batch.1$Date),]

      # Reshape - for raw
      ds1 <- reshape2::melt(batch.1, id = c("Date","Instrument"))
      ds1 <- filter(ds1, variable != "Type")
      ds1$value <- as.numeric(ds1$value)

      ds1$value <- round(ds1$value, digits = 3)
      ds1.i.row <- which(as.character(ds1$Instrument) %in% this.Instrument)
      ds1.i.inst.table <- ds1[ds1.i.row,]
      ####################################################################
      
      
      
      batch.2 <- mstest_fileIn$batch.2
      batch.2 <- batch.2[order(batch.2$Date),]
      # Reshape - for raw
      ds2 <- reshape2::melt(batch.2, id = c("Date", "Instrument"))
      ds2 <- filter(ds2, variable != "Type")
      ds2$value <- as.numeric(ds2$value)
      ds2$value <- round(ds2$value, digits = 3)
      
      goodColors <- c("#8B4513", "#98FFCC", "#0000FF", "#808080", "#FF00FF", "#f88379", "#9400D3")
      # i goes through each instrument
      # find rows in ds1 that is current instrument
      ds2.i.row <- which(as.character(ds2$Instrument) %in% this.Instrument)
      ds2.i.inst.table <- ds2[ds2.i.row,]
      # j goes through each features
      
      # if (ms_runReader$run) { # this means that input was made
      #   batch.2 <- ms_runReader$batch.2
      #   ds2 <- reshape2::melt(batch.2, id = c("Date", "Instrument"))
      #   ds2$value <- as.numeric(ds2$value)
      # }
      
      p <- list()
      for (j in 1:length(unique(ds2$variable))) {
        # find rows in ds2.i.inst.table that is current variable
        # ds2.j.row <- which(as.character(ds2.i.inst.table$variable) %in% as.character(unique(ds2.i.inst.table$variable)[j]))
        # if (j == 1) {
        #   plotLegend = T
        # } else {
        #   plotLegend = F
        # }
        ds1.j.row <- which(as.character(ds1.i.inst.table$variable) %in% as.character(unique(ds1.i.inst.table$variable)[j]))
        if (j == 1) {
          plotLegend = T
        } else {
          plotLegend = F
        }
        
        q <- plot_ly(ds1.i.inst.table[ds1.j.row,], x = ~variable, y = ~value, type = "box", color = ~Instrument, colors = "#0033cc", name = "HeLa database", legendgroup = this.Instrument, showlegend = plotLegend) %>%
          layout(boxmode = "group", paper_bgcolor = 'rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)', xaxis = list(showticklabels = T, tickangle = -45))
        
        
        if (length(s)) {
          print(s)
          if ((s[["curveNumber"]] == 0)) {
            selectedInstrument <- batch.2[rownames(batch.2) == s[["key"]], , drop = F]
            selectedFileName <- rownames(selectedInstrument)
            
            selected.ds2 <- ds2[ds2$Instrument == this.Instrument,]
            ds2Row.selected <- which(as.character(selected.ds2$variable) %in% as.character(unique(ds1.i.inst.table$variable)[j]))
            q <- q %>%
              add_markers(data = selected.ds2[ds2Row.selected[s[["pointNumber"]]+1],], x = ~variable, y = ~value, 
                          legendgroup = selectedFileName, showlegend = plotLegend, marker = list(size = 13, color = "#00cc00"),  # #ff9933
                          name = selectedFileName)
          }
          
        }
        
        
        # tmp.batch.2 <- batch.2[batch.2$Instrument == this.Instrument,]
        # tmp.batch.2 <- tmp.batch.2[order(tmp.batch.2$Date, decreasing = FALSE),]
        # tmp.batch.2 <- tmp.batch.2[,order(colnames(tmp.batch.2))]
        # lastRuns <- tail(tmp.batch.2,5)
        # # }
        # lastRuns <- lastRuns[,-(which(colnames(lastRuns) %in% c("Date", "Instrument")))]
        # lastRuns.dat <- data.frame(
        #   filename = rep(rownames(lastRuns), ncol(lastRuns)),
        #   variable = rep(colnames(lastRuns), each = nrow(lastRuns)), # nrow should be 5 (last 5 runs)
        #   value = as.vector(as.matrix(lastRuns)),
        #   Instrument = rep(this.Instrument, nrow(lastRuns)*ncol(lastRuns))
        # )
        # 
        # q <- q %>%
        #   add_markers(data = lastRuns.dat[lastRuns.dat$variable == unique(ds2$variable)[j],], x = ~variable, y = ~value,
        #               showlegend = plotLegend, marker = list(size = 10, color = c("#fee391", "#fec44f", "#fe9929", "#d95f0e", "#993404")),#"#00cc00"), # #00FF00", "#00cc00", "#009933", "#336600", "#003300")), 
        #               text = ~filename, name = ~filename)
        # 
        
        
        p <- append(p, list(q))
      }
      
      subplot(p, nrows = 1, titleX = F) %>%
        layout(title = paste0(this.Instrument, " Features", sep = ""), showlegend = T, margin = list(l = 100, r = 50, b = 200, t = 50, pad = 4))
    })
  })
  
  
  
  
  
  
}