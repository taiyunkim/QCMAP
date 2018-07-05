# author - Taiyun Kim
options(shiny.maxRequestSize=5*1024^3)
Sys.setlocale("LC_ALL","C")

library(progress)
library(htmlwidgets)
library(colourpicker)
library(shinyTime)
library(rhandsontable)
library(D3TableFilter)
library(DT)
library(glmnet)

# source the necessary functions
source("./functions/summariseFile.R")
source("./functions/MS_db_update.R")
source("./functions/helperFunctions.R")


# MS server function
msServer <- function(input,output,session) {
    
    instrumentNames <- read.csv("allFeatures.csv", colClasses = c(rep("NULL", 14), "character"), check.names = F)[,1]
    instrumentNames <- unique(instrumentNames)
    
    # declare fileIn to store input files as reactive values.
    fileIn <- reactiveValues()
    
    ms_runReader <- reactiveValues(run = F, example = F)
    selected_inst <- reactiveValues()
    inst_threshold <- reactiveValues(present = F)
    comment_text <- reactiveValues(present = F)
    comment_reactive_table <- reactiveValues(colorsHex = c("#27D3E3", "#99FF33", "#3B5E2B", "#FFFF00", "#8A2BE2"),
                                             colorsName = c("blue", "lime", "green", "yellow", "violet"))
    
    
    
    observe({
      
      withProgress(message = "Validating txt files ...", value = 0, {
        if(input$ms_example & (input$ms_try_example != "")) {
          fileObj <- list()
          fileObj$name <- input$ms_try_example
          fileObj$datapath <- paste(getwd(), "/www/data/", fileObj$name, ".csv", sep = "")
          # outputDir <- gsub(paste("^(.*/)(", fileObj$name, ")$", sep = ""), "\\1", fileObj$datapath)
          # 
          # # extraction to temporary directory does not work on windows machine. 
          # # This may be due to os directory structuture 
          # # (outputDir cannot be found when tried to access with exdir in windows)
          # outputDir <- gsub("^(.*)(/)$", "\\1", outputDir)
          # unzippedFolderDir <- unzip(fileObj$datapath, exdir = outputDir)
          # unzippedFolderName <- gsub("(.*/)(.*\\.txt)$", "\\2", unzippedFolderDir)
          
          fileIn$path <- fileObj$datapath
          fileIn$name <- fileObj$name
          ms_runReader$example = T
          
          combinedInputFile <- read.csv(fileIn$path, check.names = F, row.names = 1)
          combinedInputFile$Date <- as.POSIXct(as.character(combinedInputFile$Date))
      
        } else {
          fileInList()
          ms_runReader$example = F
          incProgress(1/4, message = "Validating txt files ...")
        
          ms_validateFileNames()
          
          
          incProgress(1/4, message = "Reading and summarising txt files ...")
          combinedInputFile <- summariseInput()
        
        } 
        ms_runReader$run <- T
          
        batch.2 <- combinedInputFile
        batch.2 <- batch.2[order(batch.2$Date),]
        dat.batch2 <- data.frame(
          date = c(batch.2$Date),
          pep = c(batch.2$`Peptide Sequences Identified`),
          batch = c(rep("input batch", nrow(batch.2))),
          name = c(rownames(batch.2)),
          instrument = c(as.character(batch.2$Instrument)),
          type = c(as.character(batch.2$Type))
        )
        incProgress(1/4, message = "Plotting ...")
        
      })
      ms_runReader$batch.2 <- batch.2
      ms_runReader$dat.batch2 <- dat.batch2
    })
    
    
    # Switch tab to first instrument in the input batch
    observe({
      if (ms_runReader$run) {
        updateTabsetPanel(
          session,
          "t",
          selected = ms_runReader$batch.2$Instrument[1]
        )
      }
    })
    
    
    # modal to add comment
    lapply(1:length(instrumentNames), function(i) {
      observeEvent(input[[paste("ms_comment_", instrumentNames[i], sep = "")]], {
        showModal(modalDialog(
          title = "Comment form",
          renderUI({
            list(
              selectInput("comment_instrument", "Instrument", selected = instrumentNames[i], choices = instrumentNames)
              
            )
          }),
          renderUI({
            list(
              dateInput("comment_date", "Date")
              # textInput("comment_date", "Date", placeholder = "Date", value = "")
              # renderText({update_request_firstName_validation()})
            )
          }),
          br(),
          renderUI({
            tagList(
              timeInput("comment_time", "Time")
              
              # textInput("comment_time", "Time", placeholder = "Date")
              # renderText({update_request_lastName_validation()})
            )
          }),
          br(),
          renderUI({
            list(
              colourpicker::colourInput("comment_colour", "Color", value ="blue", palette = "limited", 
                                        showColour = "background", 
                                        allowedCols = comment_reactive_table[["colorsHex"]])
              # textInput("comment_color", "Color", placeholder = "Color")
              # renderText({update_request_email_validation()})
            )
          }),
          br(),
          renderUI({
            list(
              textAreaInput("comment_comments", "Comments", resize = "none", width = "100%", 
                            height = "150px", placeholder = "Your comment goes here.")
              # renderText({update_request_comment_validation()})
            )
          }),
          
          
          br(),
          actionButton("ms_submit_request", "Submit"),
          easyClose = TRUE,
          footer = NULL,
          style = "width:100%"
        ))
      })
    })
    
    # Pop-up message that saves comment
    observeEvent(input$ms_submit_request, {
      # saveComment <- eventReactive(input$ma_submit_request, {
      newRow <- c(input$comment_instrument, input$comment_colour, input$comment_comments, as.character(paste(input$comment_date, strftime(input$comment_time, "%T"))))
      
      write.table(t(newRow), file = paste(getwd(), "/allComments.csv", sep = ""), append = T, sep = ",", col.names = F)
      showModal(modalDialog(
        title = "Comment saved!",
        p("Your comment is saved!"),
        br(),
        easyClose = TRUE,
        footer = NULL,
        style = "width:100%"
      ))
    })
    
    comment_fileReader <- reactiveFileReader(1000, session, paste(getwd(), "/allComments.csv", sep = ""), function(commentPath) {
      read.csv(commentPath, check.names = F, header = T)
    })
    
    
    # modal to edit comment
    lapply(1:length(instrumentNames), function(i) {
      observeEvent(input[[paste("ms_edit_comment_", instrumentNames[i], sep = "")]], {
        if (nrow(comment_fileReader()) != 0) {
          showModal(modalDialog(
            size = "l",
            title = "Comment editor",
            rHandsontableOutput("comment_editor"),
            # rHandsontableOutput(paste("comment_editor_", instrumentNames[i], sep = "")),
            # d3tfOutput("comment_editor", height = "auto"),
            br(),
            actionButton("ms_save_request", "Save"),
            easyClose = TRUE,
            footer = NULL,
            style = "width:100%"
          ))
        } else {
          showModal(modalDialog(
            size = "l",
            title = "Comment editor",
            h4("No Comments so far"),
            br(),
            easyClose = TRUE,
            footer = NULL,
            style = "width:100%"
          ))
        }
        
      })
    })
    
    
    
    output$comment_editor <- renderRHandsontable({
      # output$comment_editor <- renderD3tf({
      comment_table <- comment_fileReader()
      comment_table <- comment_table[,-1]

      for (i in 1:length(comment_reactive_table[["colorsHex"]])) {
        levels(comment_table$Colour)[levels(comment_table$Colour) == comment_reactive_table[["colorsHex"]][i]] <- comment_reactive_table[["colorsName"]][i]
      }


      comment_reactive_table[["table"]] <- as.data.frame(comment_table, stringsAsFactors = F)
      comment_reactive_table[["table"]]$Instrument <- factor(comment_reactive_table[["table"]]$Instrument)
      comment_reactive_table[["table"]]$Colour <- factor(comment_reactive_table[["table"]]$Colour)
      rhandsontable(comment_reactive_table[["table"]], stretchH = "all",
                    colHeaders = colnames(comment_reactive_table[["table"]]), search = T, height = 500) %>%
        hot_col("Comment", allowInvalid = T) %>%
        hot_col("Colour", type = "dropdown", source = comment_reactive_table[["colorsName"]]) %>%
        hot_col("Instrument", type = "dropdown", source = instrumentNames) %>%
        hot_col("datetime", type = "date", dateFormat = "YYYY-MM-DD HH:mm:ss") %>%
        hot_cols(columnSorting = TRUE)
      # tableProps <- list(
      #   btn_reset = TRUE,
      #   # alphabetic sorting for the row names column, numeric for all other columns
      #   col_types = c("string", "string", "string", "date")
      # )
      # d3tf(comment_reactive_table[["table"]],
      #      tableProps = tableProps,
      #      extensions = list(
      #        list(name = "sort")
      #      ),
      #      showRowNames = F,
      #      tableStyle = "table table-bordered",
      #      colsResizable = T,
      #      edit = T)
    })

    # Pop-up message that updates the comments
    observeEvent(input$ms_save_request, {
      comment_table <- isolate(hot_to_r(input$comment_editor))
      # comment_table <- isolate(comment_reactive_table[["table"]])
      for (i in 1:length(comment_reactive_table[["colorsHex"]])) {
        levels(comment_table$Colour)[levels(comment_table$Colour) == comment_reactive_table[["colorsName"]][i]] <- comment_reactive_table[["colorsHex"]][i]
        
        # comment_table$Colour[comment_table$Colour == comment_reactive_table[["colorsName"]][i]] <- comment_reactive_table[["colorsHex"]][i]
      }
      write.csv(comment_table, file = paste(getwd(), "/allComments.csv", sep = ""))
      showModal(modalDialog(
        title = "Update",
        p("Your comments are updated!"),
        br(),
        easyClose = TRUE,
        footer = NULL,
        style = "width:100%"
      ))
    })
    
    
    ################################################################################
    # QC_recaptcha 
    # 
    # function to call Google reCAPTCHA on server side
    ################################################################################
    QC_recaptcha <- function(input, output, session, secret = Sys.getenv("recaptcha_secret")) {
      
      status <- reactive({
        if (isTruthy(input$recaptcha_response)) {
          url <- "https://www.google.com/recaptcha/api/siteverify"
          
          resp <- httr::POST(url, body = list(
            secret = secret,
            response = input$recaptcha_response
          ))
          
          fromJSON(httr::content(resp, "text"))
        } else {
          list(success = FALSE)
        }
      })
      
      return(status)
      
    }
    
    
    ms_validateFileNames <- reactive({
      fileNameList <- c("summary.txt", "allPeptides.txt", "msScans.txt", "evidence.txt")
      shiny::validate(
        need(
          (all(fileNameList %in% fileIn$name) & (length(fileIn$name) >= length(fileNameList) ) ),
          "Please choose the correct files"
        )
      )
    })
    
    
    ################################################################################
    # fileInList 
    # 
    # read the files when run is clicked
    ################################################################################
    fileInList <- reactive({
      fileObj <- input$maxQuantFiles
      
      shiny::validate(
        shiny::need(
          (!is.null(fileObj$name)),
          "Please upload your txt files(/zipped folder)."
        )
      )
      
      if (grepl("*\\.txt$", fileObj$name[1])) {
        fileIn$name <- fileObj$name
        fileIn$path <- fileObj$datapath
      } else if (grepl(".*\\.zip$",fileObj$name[1])) {
        if (length(fileObj$name) == 1) { # zipped folder
          outputDir <- gsub("^(.*/)(0\\.zip$)", "\\1", fileObj$datapath) # Assume the filename is alwasy 0.zip
          
          # extraction to temporary directory does not work on windows machine. 
          # This may be due to os directory structuture 
          # (outputDir cannot be found when tried to access with exdir in windows)
          unzippedFolderDir <- unzip(fileObj$datapath, exdir = outputDir)
          unzippedFolderName <- gsub("(.*/)(.*\\.txt)$", "\\2", unzippedFolderDir)
          
          fileIn$path <- unzippedFolderDir
          fileIn$name <- unzippedFolderName
        } 
      } 
    })
    
    
    ################################################################################
    # MS_save_DB 
    # 
    # save input batch files to the database. This is run when "Save" button is clicked
    ################################################################################
    MS_save_DB <- observeEvent(input$MS_saveDB, {
      result <- callModule(QC_recaptcha, "MS_recaptcha_test", secret = "6Lfg_jYUAAAAAI9UEuZ2FI_t0pjllcleGnWD5YfX")

      req(result()$success)
      
      # check all Fields
      
      # save the data
      allFeatures <- readData()
      combinedInputFile <- summariseInput()
      combinedInputFile$Type <- "DB"
      
      
      # order the columns
      combinedInputFile <- combinedInputFile[,match(colnames(allFeatures), colnames(combinedInputFile))]
      
      # combine two files
      allFeatures <- rbind(allFeatures, combinedInputFile)
      
      MS_db_update(allFeatures)
      
      # alert
      showModal(
        modalDialog(
          title = HTML('<span style="color:green; font-size: 20px; font-weight:bold; font-family:sans-serif ">Data saved!<span>
                       <button type = "button" class="close" data-dismiss="modal" ">
                       <span style="color:white; "><span>
                       </button> '),
          p("Your has been saved."),
          p("Changes will affect in your next run."),
          easyClose = TRUE,
          footer = NULL
        )
      )
    })

    
    # ################################################################################
    # # readData 
    # # 
    # # It reads the CSV file (database). It also sets the "Date" as a R Date format
    # ################################################################################
    # readData <- function() {
    #   allFeatures <- read.csv(paste0(getwd(), "/allFeatures.csv", sep = ""), row.names = 1, check.names = FALSE)
    #   allFeatures$Date <- as.POSIXct(as.character(allFeatures$Date))
    #   
    #   return(allFeatures)
    # }
    # 
    
    ################################################################################
    # summariseInput 
    # 
    # This will summarise input text files (summary.txt, allPeptides.txt, msScans.txt, evidence.txt)
    ################################################################################
    summariseInput <- reactive({
      
      sumFileHeaders <- read.delim2(fileIn$path[which(fileIn$name == "summary.txt")], sep = "\t", header = TRUE, check.names = FALSE, nrow = 1)
      sumFileHeadersIndex <- which(colnames(sumFileHeaders) %in% c("Raw file", "Enzyme", "MS/MS", "MS/MS Identified", "MS/MS Identified [%]", 
                                                                   "Mass Standard Deviation [ppm]", "Peaks Repeatedly Sequenced [%]", "Peptide Sequences Identified"))
      
      allPepHeaders <- read.delim2(fileIn$path[which(fileIn$name == "allPeptides.txt")], sep = "\t", header = TRUE, check.names = FALSE, nrow = 1)
      allPepHeadersIndex <- which(colnames(allPepHeaders) %in% c("Raw file", "Intensity", "Mass precision [ppm]", "Retention length (FWHM)", "Score", "Sequence"))
                                                                   
      
      msScansHeaders <- read.delim2(fileIn$path[which(fileIn$name == "msScans.txt")], sep = "\t", header = TRUE, check.names = FALSE, nrow = 1)
      msScansHeadersIndex <- which(colnames(msScansHeaders) %in% c("Raw file", "Cycle time"))
      
      
      evidenceHeaders <- read.delim2(fileIn$path[which(fileIn$name == "evidence.txt")], sep = "\t", header = TRUE, check.names = FALSE, nrow = 1)
      evidenceHeadersIndex <- which(colnames(evidenceHeaders) %in% c("Raw file", "Mass Error [ppm]"))
      
      
      # [ncol == 52] Raw file (1), Experiment(2), MS/MS(20), MS/MS Identified(26), MS/MS Identified [%](30), Peptide Sequences Identified(34), Peaks Repeatedly Sequenced [%](39), Mass Standard Deviation [ppm](49))
      sumFileTable <- read.delim2(fileIn$path[which(fileIn$name == "summary.txt")], sep = "\t", header = TRUE, check.names = FALSE,
                                  colClasses = unlist( lapply(1:ncol(sumFileHeaders), function(i) {if(i %in% sumFileHeadersIndex) {"character"} else {"NULL"} } ) ))
      
      # [ncol == 33] Raw file(1), Mass Precision [ppm](14), FWHM(18), Sequences(23), Score(28), Intensity(29) 
      allPepFileTable <- read.delim2(fileIn$path[which(fileIn$name == "allPeptides.txt")], sep = "\t", header = TRUE, check.names = FALSE,
                                     colClasses = unlist( lapply(1:ncol(allPepHeaders), function(i) {if(i %in% allPepHeadersIndex) {"character"} else {"NULL"} } ) ))
      
      # [ncol == 29] Raw file(1), Cycle time(5)
      msScansFileTable <- read.delim2(fileIn$path[which(fileIn$name == "msScans.txt")], sep = "\t", header = TRUE, check.names = FALSE,
                                      colClasses = unlist( lapply(1:ncol(msScansHeaders), function(i) {if(i %in% msScansHeadersIndex) {"character"} else {"NULL"} } ) ))
      # Raw file, Mass error [ppm]
      # evidenceFileTable <- read.delim2(fileIn$path[which(fileIn$name == "evidence.txt")], sep = "\t", header = TRUE, check.names = FALSE,
      #                                  colClasses = c(rep("NULL", 15), "character", rep("NULL", 9), "character", rep("NULL", 37)))
      evidenceFileTable <- read.delim2(fileIn$path[which(fileIn$name == "evidence.txt")], sep = "\t", header = TRUE, check.names = FALSE,
                                       colClasses = unlist( lapply(1:ncol(evidenceHeaders), function(i) {if(i %in% evidenceHeadersIndex) {"character"} else {"NULL"} } ) ))
      
      
      summary.result <- summariseFile(
        list(
          sumFileTable = sumFileTable,
          allPepFileTable = allPepFileTable,
          msScansFileTable = msScansFileTable,
          evidenceFileTable = evidenceFileTable
        ),
        instrumentNames
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
    
    
    
    
    
    
    output$ms_validationMessage <- renderText({
      fileInList()
      ms_validateFileNames()
      
    })
    
    
    
    # loop bar plot
    lapply(1:length(instrumentNames), function(i) {
      output[[paste0("bar_", instrumentNames[i], "_history", sep = "")]] <- renderPlotly({
        this.instrument <- instrumentNames[i]
        allFeatures <- readData()
        
       batch.1 <- allFeatures
        batch.1 <- batch.1[order(batch.1$Date),]
        
        batch.1 <- batch.1[batch.1$Instrument == this.instrument,]
        dat <- data.frame(
          date = c(as.POSIXct(batch.1$Date)),
          pep = c(batch.1$`Peptide Sequences Identified`),
          batch = c(rep("Database", nrow(batch.1))),
          name = c(rownames(batch.1)),
          instrument = c(as.character(batch.1$Instrument)),
          type = c(as.character(batch.1$Type))
        )
        
        goodColors <- c("#8B4513", "#98FFCC", "#0000FF", "#808080", "#FF00FF", "#f88379", "#9400D3")
        p <- plot_ly(dat, x = ~date, source = "peptideNumPlot", y = ~pep, text = ~name, name = "database", key = ~instrument, showlegend = T, color = ~instrument, colors = "#808080", type = "bar") %>%
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
          rangeslider(start = (as.POSIXct(tail(dat$date,1)) - 1296000), end = as.POSIXct(tail(dat$date,1)))
        
        if (ms_runReader$run == T) {
          dat.batch2 <- ms_runReader$dat.batch2
          if (any(dat.batch2$instrument == this.instrument)) {
            dat.batch2 <- dat.batch2[dat.batch2$instrument == this.instrument,]
            p <- p %>%
              add_bars(data = dat.batch2, x = ~date, y = ~pep, key = ~type, 
                       marker = list(color = "#e60000"), text = ~name, 
                       name = ~instrument, color = ~name, showlegend = T, inherit = T)
            
            p <- p %>%
              rangeslider(start = (as.POSIXct(tail(dat.batch2$date,1)) - 1296000), end = as.POSIXct(tail(dat.batch2$date,1)))
          }
        }
        
        thresholdData <- paste(getwd(), "/thresholds.txt", sep = "")
        if (!file.exists(thresholdData)) {
          thresholdData = NA
        }
        if (!is.na(thresholdData)) {
          inst_threshold$present = T
          tmp <- read.delim(thresholdData, check.names = F, header = T)
          for (i in 1:nrow(tmp)) {
            inst_threshold[[as.character(tmp[i,1])]] <- as.numeric(tmp[i,2])
          }
        }
        if (inst_threshold$present == T) {
          if (ms_runReader$run == T) {
            p <- p %>%
              add_lines(x = c(dat$date, dat.batch2$date, tail(dat.batch2$date,1)+86400), y = inst_threshold[[this.instrument]], name = paste(this.instrument, "<br />threshold", sep = ""),
                        key = "threshold", line = list(color = "#ff7f50"), hoverinfo = "none", showlegend = T, inherit = F)
          } else {
            p <- p %>%
              add_lines(y = inst_threshold[[this.instrument]], name = "threshold",
                        key = "threshold", line = list(color = "#ff7f50"), hoverinfo = "none", showlegend = T, inherit = T)
          }
          
        }
        commentData <- paste(getwd(), "/allComments.csv", sep = "")
        if (!file.exists(commentData)) {
          commentData <- NA
        }
        if (!is.na(commentData)) {
          comment_text$present = T
          
          tmp <- comment_fileReader()
          # tmp <- read.csv(commentData, check.names = F, header = T)
          # remove numbering
          tmp <- tmp[,-1]
          tmp$datetime <- as.POSIXct(tmp$datetime)
          this.comment <- tmp[tmp$Instrument == this.instrument,]
          if (nrow(this.comment) > 0) {
            p <- p %>%
              add_markers(x = this.comment$datetime, y = 0, hoverinfo = "text", showlegend = T, inherit = F, name = paste(this.instrument, "<br />comments", sep = ""),
                          key = "comment", hovertext = ~this.comment$Comment, marker = list(color = ~this.comment$Colour, symbol = "x", size = 10))
          }
        }
        p %>%
          onRender(jsCode = JS("comment_javascript"))
        p %>%
          onRender(jsCode = JS("javascript"))
        
          # layout(barmode = "overlay")
      })
    })
    
    # loop box plot
    lapply(1:length(instrumentNames), function(i) {
      output[[paste0("box_", instrumentNames[i], sep = "")]] <- renderPlotly({
        this.Instrument = instrumentNames[i]
        s <- event_data("plotly_click", source = "peptideNumPlot")
        
        allFeatures <- readData()
        
        batch.1 <- allFeatures
        batch.1 <- batch.1[order(batch.1$Date),]
        
        # Reshape - for raw
        ds1 <- reshape2::melt(batch.1, id = c("Date","Instrument"))
        ds1 <- filter(ds1, variable != "Type")
        ds1$value <- as.numeric(ds1$value)
        
        ds1$value <- round(ds1$value, digits = 3)
        
        goodColors <- c("#8B4513", "#98FFCC", "#0000FF", "#808080", "#FF00FF", "#f88379", "#9400D3")
        # i goes through each instrument
        # find rows in ds1 that is current instrument
        ds1.i.row <- which(as.character(ds1$Instrument) %in% this.Instrument)
        ds1.i.inst.table <- ds1[ds1.i.row,]
        # j goes through each features
        
        if (ms_runReader$run) { # this means that input was made
          batch.2 <- ms_runReader$batch.2
          ds2 <- reshape2::melt(batch.2, id = c("Date", "Instrument"))
          ds2$value <- as.numeric(ds2$value)
        }
        
        p <- list()
        for (j in 1:length(unique(ds1$variable))) {
          # find rows in ds1.i.inst.table that is current variable
          ds1.j.row <- which(as.character(ds1.i.inst.table$variable) %in% as.character(unique(ds1.i.inst.table$variable)[j]))
          if (j == 1) {
            plotLegend = T
          } else {
            plotLegend = F
          }
          q <- plot_ly(ds1.i.inst.table[ds1.j.row,], x = ~variable, y = ~value, type = "box", color = ~Instrument, colors = "#0033cc", name = "database", legendgroup = this.Instrument, showlegend = plotLegend) %>%
            layout(boxmode = "group", paper_bgcolor = 'rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)', xaxis = list(showticklabels = T, tickangle = -45))
          
          
          if (length(s)) {
            if ((s[["key"]] %in% this.Instrument) & (s[["curveNumber"]] == 0)) {
              selectedInstrument <- batch.1[batch.1$Instrument == s[["key"]], ]
              selectedFileName <- rownames(selectedInstrument)[s[["pointNumber"]]+1]
              
              selected.ds1 <- ds1[ds1$Instrument == s[["key"]],]
              ds1Row.selected <- which(as.character(selected.ds1$variable) %in% as.character(unique(ds1$variable)[j]))
              q <- q %>%
                add_markers(data = selected.ds1[ds1Row.selected[s[["pointNumber"]]+1],], x = ~variable, y = ~value, 
                            legendgroup = selectedFileName, showlegend = plotLegend, marker = list(size = 13, color = "#00cc00"),  # #ff9933
                            name = selectedFileName)
              
            } else if (s[["key"]] == "input") {
              selectedFileName <- rownames(batch.2[batch.2$Instrument == this.Instrument,])[s[["curveNumber"]]]
              
              ds2Row.selected <- which(as.character(ds2$variable) %in% as.character(unique(ds1$variable)[j]))
              ds2.inst <- ds2[ds2Row.selected,]
              ds2.inst <- ds2.inst[ds2.inst$Instrument == this.Instrument,]
              q <- q %>%
                add_markers(data = ds2.inst[s[["curveNumber"]],], x = ~variable, y = ~value, 
                            legendgroup = selectedFileName, showlegend = plotLegend, marker = list(size = 13, color = "#00cc00"),  
                            name = selectedFileName)
            }
            
          }
          
          
          tmp.batch.1 <- batch.1[batch.1$Instrument == this.Instrument,]
          tmp.batch.1 <- tmp.batch.1[order(tmp.batch.1$Date, decreasing = FALSE),]
          tmp.batch.1 <- tmp.batch.1[,order(colnames(tmp.batch.1))]
          if (ms_runReader$run) {
            batch.2 <- ms_runReader$batch.2
            tmp.batch.2 <- batch.2[batch.2$Instrument == this.Instrument,]
            tmp.batch.2 <- tmp.batch.2[order(tmp.batch.2$Date, decreasing = FALSE),]
            tmp.batch.2 <- tmp.batch.2[,order(colnames(tmp.batch.2))]
            lastRuns <- rbind(tail(tmp.batch.1,5), tail(tmp.batch.2,5))
            lastRuns <- lastRuns[order(lastRuns$Date, decreasing = FALSE),]
            
            lastRuns <- tail(lastRuns, 5)
          } else {
            lastRuns <- tail(tmp.batch.1,5)
          }
          lastRuns <- lastRuns[,-(which(colnames(lastRuns) %in% c("Date", "Instrument")))]
          lastRuns.dat <- data.frame(
            filename = rep(rownames(lastRuns), ncol(lastRuns)),
            variable = rep(colnames(lastRuns), each = nrow(lastRuns)), # nrow should be 5 (last 5 runs)
            value = as.vector(as.matrix(lastRuns)),
            Instrument = rep(this.Instrument, nrow(lastRuns)*ncol(lastRuns))
          )
                     
          q <- q %>%
            add_markers(data = lastRuns.dat[lastRuns.dat$variable == unique(ds1$variable)[j],], x = ~variable, y = ~value,
                       showlegend = plotLegend, marker = list(size = 10, color = c("#fee391", "#fec44f", "#fe9929", "#d95f0e", "#993404")),#"#00cc00"), # #00FF00", "#00cc00", "#009933", "#336600", "#003300")), 
                       text = ~filename, name = ~filename)

          
          
          p <- append(p, list(q))
        }
        
        subplot(p, nrows = 1, titleX = F) %>%
          layout(title = paste0(this.Instrument, " Features", sep = ""), showlegend = T, margin = list(l = 100, r = 50, b = 200, t = 50, pad = 4))
      })
    })
    
    
    
    ################################################################################
    # output$bar_plotHistory
    #
    # This will output bar plots of "peptide sequences identified" by time
    ################################################################################
    output$bar_plotHistory <- renderPlotly({
      
      allFeatures <- readData()
      
      batch.1 <- allFeatures
      batch.1 <- batch.1[order(batch.1$Date),]
      
      dat <- data.frame(
        date = c(batch.1$Date),
        pep = c(batch.1$`Peptide Sequences Identified`),
        batch = c(rep("Database", nrow(batch.1))),
        name = c(rownames(batch.1)),
        instrument = c(as.character(batch.1$Instrument))
      )
      
      goodColors <- c("#8B4513", "#98FFCC", "#0000FF", "#808080", "#FF00FF", "#f88379", "#9400D3")
      p <- plot_ly(dat, x = ~date, source = "allPeptidePlot", y = ~pep, text = ~name, key = ~instrument, showlegend = T, color = ~instrument, colors = c(goodColors[1:(length(levels(dat$instrument))-1)], "#9400D3"), type = "bar") %>%
        layout(
          title = "Peptide Sequences Identified",
          xaxis = list(
            rangeselector = list(
              buttons = list(
                list(
                  count = 1,
                  label = "1 month",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 3,
                  label = "3 months",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 6,
                  label = "6 months",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "1 year",
                  step = "year",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "Year To Date",
                  step = "year",
                  stepmode = "todate"),
                list(step = "all")
              )
            ),
            rangeslider = list(type = "date")
          ),
          
          
          yaxis = list(title = "Peptide Sequences Identified")
        )
      
      if (ms_runReader$run == T) {
        dat.batch2 <- ms_runReader$dat.batch2
        p <- p %>%
          add_bars(data = dat.batch2, x = ~date, y = ~pep, key = ~instrument, marker = list(color = "#e60000"), text = ~name, name = "inputs", showlegend = T, inherit = F)
      }
      p
      
    })
    
    
    
    ################################################################################
    # output$box_individualFeatures
    #
    # This will plot a selected bar's (from output$bar_plotHistory) each features in
    # a box plots with raw values
    ################################################################################
    output$box_individualFeatures <- renderPlotly({
      s <- event_data("plotly_click", source = "allPeptidePlot")
      
      allFeatures <- readData()
      
      batch.1 <- allFeatures
      batch.1 <- batch.1[order(batch.1$Date),]
      
      # Reshape - for raw
      ds1 <- reshape2::melt(batch.1, id = c("Date","Instrument"))
      ds1$value <- as.numeric(ds1$value)
     
      # when a peak (bar) is clicked from history plot, find a file that is selected
      if (length(s)) {
        if (s[["key"]] %in% c("QEplus", "QECl", "QEHF", "QEFFX1", "Fusion")) {
          selectedInstrument <- batch.1[batch.1$Instrument == s[["key"]], ]
          selectedFileName <- rownames(selectedInstrument)[s[["pointNumber"]]+1]
          
        } else if (s[["key"]] == "input") {
          batch.2 <- ms_runReader$batch.2
          ds2 <- reshape2::melt(batch.2, id = c("Date", "Instrument"))
          # ds2 <- filter(ds2, variable != "File size (kb)")
          ds2$value <- as.numeric(ds2$value)
          ds2$Instrument <- input$inst
          ds2$Instrument <- as.factor(ds2$Instrument)
          
          selectedFileName <- rownames(batch.2)[s[["pointNumber"]]+1]
        }
        
      }
      
      # plot the first 4 features
      p <- list()
      goodColors <- c("#8B4513", "#98FFCC", "#0000FF", "#808080", "#FF00FF", "#f88379", "#9400D3")
      
      for (i in 1:4) {
        
        ds1Row <- which(as.character(ds1$variable) %in% as.character(unique(ds1$variable)[i]))
        
        q <- plot_ly(ds1[ds1Row,], y = ~value, color = ~Instrument, colors = goodColors[1:(length(levels(ds1[ds1Row,]$Instrument)))], type = "box" ) %>%
          layout(boxmode = "group", paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)', xaxis = list(title = as.character(unique(ds1$variable)[i]), showticklabels = T))
        
        # when a peak (bar) is clicked from history plot
        if (length(s)) {
          if (s[["key"]] %in% c("QEplus", "QECl", "QEHF", "QEFFX1", "Fusion")) {
            selected.ds1 <- ds1[ds1$Instrument == s[["key"]],]
            ds1Row.selected <- which(as.character(selected.ds1$variable) %in% as.character(unique(ds1$variable)[i]))
            q <- q %>%
              add_markers(data = selected.ds1[ds1Row.selected[s[["pointNumber"]]+1],], x = ~Instrument, y = ~value, marker = list(size = 10, color = "#ff9933"),  name = selectedFileName)
            
          } else if (s[["key"]] == "input") {
            ds2Row <- which(as.character(ds2$variable) %in% as.character(unique(ds1$variable)[i]))
            q <- q %>%
              add_markers(data = ds2[ds2Row[s[["pointNumber"]]+1],], x = ~Instrument, y = ~value, marker = list(size = 10, color = "#ff9933"),  name = selectedFileName)
            
          }
          
        }
        p <- append(p, list(q))
      }
      
      s1 <- subplot(p, nrows = 1, titleX = TRUE)
      
      
      # plot next 4 features
      p <- list()
      for (i in 5:8) {
        
        ds1Row <- which(as.character(ds1$variable) %in% as.character(unique(ds1$variable)[i]))
        
        q <- plot_ly(ds1[ds1Row,], y = ~value, color = ~Instrument, colors = goodColors[1:(length(levels(ds1[ds1Row,]$Instrument)))], type = "box" ) %>%
          layout(boxmode = "group", paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)', xaxis = list(title = as.character(unique(ds1$variable)[i]), showticklabels = T))
        
        # when a peak (bar) is clicked from history plot
        if (length(s)) {
          if (s[["key"]] %in% c("QEplus", "QECl", "QEHF", "QEFFX1", "Fusion")) {
            selected.ds1 <- ds1[ds1$Instrument == s[["key"]],]
            ds1Row.selected <- which(as.character(selected.ds1$variable) %in% as.character(unique(ds1$variable)[i]))
            q <- q %>%
              add_markers(data = selected.ds1[ds1Row.selected[s[["pointNumber"]]+1],], x = ~Instrument, y = ~value, marker = list(size = 10, color = "#ff9933"),  name = selectedFileName)
            
          } else if (s[["key"]] == "input") {
            ds2Row <- which(as.character(ds2$variable) %in% as.character(unique(ds1$variable)[i]))
            q <- q %>%
              add_markers(data = ds2[ds2Row[s[["pointNumber"]]+1],], x = ~Instrument, y = ~value, marker = list(size = 10, color = "#ff9933"),  name = selectedFileName)
            
          }
          
        }
        p <- append(p, list(q))
      }
      
      s2 <- subplot(p, nrows = 1, titleX = TRUE)
      
      
      # plot next 4 features
      p <- list()
      for (i in 9:12) {
        
        ds1Row <- which(as.character(ds1$variable) %in% as.character(unique(ds1$variable)[i]))
        
        q <- plot_ly(ds1[ds1Row,], y = ~value, color = ~Instrument, colors = goodColors[1:(length(levels(ds1[ds1Row,]$Instrument)))], type = "box" ) %>%
          layout(boxmode = "group", paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)', xaxis = list(title = as.character(unique(ds1$variable)[i]), showticklabels = T))
        
        # when a peak (bar) is clicked from history plot
        if (length(s)) {
          if (s[["key"]] %in% c("QEplus", "QECl", "QEHF", "QEFFX1", "Fusion")) {
            selected.ds1 <- ds1[ds1$Instrument == s[["key"]],]
            ds1Row.selected <- which(as.character(selected.ds1$variable) %in% as.character(unique(ds1$variable)[i]))
            q <- q %>%
              add_markers(data = selected.ds1[ds1Row.selected[s[["pointNumber"]]+1],], x = ~Instrument, y = ~value, marker = list(size = 10, color = "#ff9933"),  name = selectedFileName)
            
          } else if (s[["key"]] == "input") {
            ds2Row <- which(as.character(ds2$variable) %in% as.character(unique(ds1$variable)[i]))
            q <- q %>%
              add_markers(data = ds2[ds2Row[s[["pointNumber"]]+1],], x = ~Instrument, y = ~value, marker = list(size = 10, color = "#ff9933"),  name = selectedFileName)
            
          }
          
        }
        p <- append(p, list(q))
      }
      
      s3 <- subplot(p, nrows = 1, titleX = TRUE)
      
      # plot all feature boxplots
      subplot(s1,s2,s3, nrows = 3, margin = 0.05, heights = rep(1/3, 3), titleX = TRUE) %>%
        layout(showlegend = FALSE)
    })
    
    
    
    output$ms_saveUI <- renderUI({
      if (ms_runReader$run & !ms_runReader$example) {
        sidebarPanel(
          div(
            h4("Click to save this batch to the Database"),
            QC_recaptchaUI("MS_recaptcha_test", sitekey = "6Lfg_jYUAAAAAAVwxpEHlaRupSp87bl2Wj1pACkn"),
            actionButton("MS_saveDB", "Save")
          ),
          width = "100%"
        ) 
      }
    })
    
    
####Irene's addition#########################################################################################################    
  #calculate score function#############################################################################################  
    calculateQC <- function(calculateFile, Instrument_50_re) {
      peptideScore<- 0.5*calculateFile[1,1]
      Feature11Score <- Instrument_50_re*calculateFile[1,2:12]
      sum11sCORE<- sum(Feature11Score)
      QCscore<-peptideScore+sum11sCORE
      QCScore <- round(QCscore, digits=3)
      return(QCScore)
    }
    #####################################################################################################################    
    #calculate the QC score
    #####################################################################################################################    
    lapply(1:length(instrumentNames), function(i) {
      output[[paste0("score_", instrumentNames[i], "_check", sep = "")]] <- renderText({
        this.instrument <- instrumentNames[i]
        allFeatures <- readData()
        batch.1 <- allFeatures
        if (ms_runReader$run == T) {
          batch.2 <- ms_runReader$batch.2
          if (any(batch.2$instrument == this.instrument)) {
            batch.2 <- batch.2[batch.2$instrument == this.instrument,]
          }
        }
        
        #if a peak bar being selected, find the file that is selected
        s <- event_data("plotly_click",source = "peptideNumPlot")
        if (length(s)) {
          if ((s[["key"]] %in% this.instrument) & (s[["curveNumber"]] == 0)) {
            selectedInstrument <- batch.1[batch.1$Instrument == s[["key"]], ]
            #read raw data X (11 features)for that selectedInstrument data
            
            X.raw_Instrument <- selectedInstrument[,2:12]
            #read raw data Y (peptide value) for that selectedInstrument data
            y_Instrument <- selectedInstrument[,1]
            
            #find the high leverage points and remove from the existing database#############
            
            selectInstrument_leverage <- selectedInstrument[,1:12]
            
            colnames(X.raw_Instrument) <- c("MS.MS.Identifiedp", "MassStandardDevisationppm","MS.MS.Identified","PeaksRepeatedlySequencedp" ,"MS.MS" ,"Retention.length.FWHM","Score","Intensity","MassPrecision.ppm","CycleTime","MassError")
            
            results_Instrument <- lm(y_Instrument ~ MS.MS.Identifiedp + MassStandardDevisationppm+MS.MS.Identified+PeaksRepeatedlySequencedp +MS.MS +Retention.length.FWHM+Score+Intensity+MassPrecision.ppm+CycleTime+MassError, data=X.raw_Instrument)
            D<- cooks.distance(results_Instrument)
            
            #no of Feature(parametrs) / no of observers (p/n)
            
            #find the cut off value, identify the leverage datasets, then remove them
            HL<- as.matrix(which (D> qf(0.5,11,nrow(selectInstrument_leverage)-11)), rownames.force = NA)
            
            removeName <- as.list(row.names(HL)) 
            rownames(HL)<- row.names(HL)
            remove <- rownames(HL)
            
            #REMOVE the high leverage points from the datasets
            selectInstrument_remove <- selectInstrument_leverage[!rownames(selectInstrument_leverage)%in% remove,]
            
            #normalize the data from the existing database
            #Function to normalise the data (only existing database) #
            #firstly normalize the y value
            
            
            y_Instrument_remove <- selectInstrument_remove[,1]
            scaled_y_Instrument_re<-(y_Instrument_remove - min(y_Instrument_remove))/(max(y_Instrument_remove)-min(y_Instrument_remove))
            
            #secondly normalize the x values
            
            X_Instrument_remove <- selectInstrument_remove[,2:12]
            
            median_features <- apply(X_Instrument_remove, 2, FUN = median)
            median_features <- t(median_features)
            median_features <- as.data.frame(median_features)
            
            # assumes that  the column order of "median_features" and "X_QECl_remove" are identical
            range01_Instrument_t <- X_Instrument_remove
            for (i in 1:ncol(X_Instrument_remove)) {
              range01_Instrument_t[,i] <- range01_Instrument_t[,i]/median_features[1,i]
            }
            
            
            
            
            #combine the y and x after normalization
            range01_Instrument_t <- as.matrix(range01_Instrument_t)
            normalize_Instrument <- cbind(scaled_y_Instrument_re,range01_Instrument_t)
            
            ####Build the function for Instrument Specific Model analysis with normalized  datasets#############
            set.seed(1)
            cv.out_Instrument_t <- cv.glmnet(x=range01_Instrument_t, y= scaled_y_Instrument_re,alpha=0, nfolds=5)
            
            #regression with the best lambda
            ridge.mod_Instrument_t <- glmnet (x=range01_Instrument_t, y=scaled_y_Instrument_re,alpha = 0)
            
            #contribution values from 11 features
            ridge.coef_Instrument_t <- predict (ridge.mod_Instrument_t, type="coefficients", s=cv.out_Instrument_t$lambda.min)
            
            #before converting the 50% weights
            contributionInstrument_t <- t(ridge.coef_Instrument_t[-1,1])
            abs_contributionInstrument_t <- abs(contributionInstrument_t)
            sum_Instrument_t <- sum(abs_contributionInstrument_t)
            #get each weights of 11 feature among 50%
            weight_Instrument_t <- function(t){t/sum_Instrument_t} 
            #keep the sign to get the percentage of the weights
            norm_Instrument_t <- weight_Instrument_t(contributionInstrument_t)
            
            Instrument_50_t <- norm_Instrument_t *0.5
            #calculate the weights for 11 features and to 3 digits
            Instrument_50_re<- round(Instrument_50_t,digits=3)
            #write.csv(Instrument_50_re, file="./weights.csv")
            
            #selected the data when user click on specific bar
            selectedFileName <- rownames(selectedInstrument)[s[["pointNumber"]]+1]
            
            oldData<- selectedInstrument[grep(selectedFileName,row.names(selectedInstrument),ignore.case=TRUE),]
            #Function to normalise the olddata #
            #firstly normalize the y value
            
            y_oldData <- oldData[,1]
            scaled_y_oldData<-(y_oldData - min(selectedInstrument[,1]))/(max(selectedInstrument[,1])-min(selectedInstrument[,1]))
            
            #secondly normalize the x values
            
            range01_Instrument_t <- oldData[,2:12]
            
            # assumes order of columns are identical
            for (i in 1:ncol(range01_Instrument_t)) {
              range01_Instrument_t[,i] <- range01_Instrument_t[,i]/median_features[1,i]
            }
            
            ##########################################################################
            # range01 <- function(x){(x-min(selectedInstrument[,2:12]))/(max(selectedInstrument[,2:12])-min(selectedInstrument[,2:12]))}
            # range01_Instrument_msms<- range01(X_oldData$`MS/MS Identified [%]`)
            # range01_Instrument_msd <- range01(X_oldData$`Mass Standard Deviation [ppm]`)
            # range01_Instrument_msms2<-range01(X_oldData$`MS/MS Identified`)
            # range01_Instrument_PRS <- range01(X_oldData$`Peaks Repeatedly Sequenced [%]`)
            # range01_Instrument_ms_ms<- range01(X_oldData$`MS/MS`)
            # range01_Instrument_FWHM <- range01(X_oldData$`Retention length (FWHM)`)
            # range01_Instrument_score <- range01(X_oldData$Score)
            # range01_Instrument_Intensity <- range01(X_oldData$Intensity)
            # range01_Instrument_ppm <- range01(X_oldData$`Mass precision [ppm]`)
            # range01_Instrument_cycletime<- range01(X_oldData$`Cycle time`)
            # range01_Instrument_masserror<- range01(X_oldData$`Mass Error [ppm]`)
            # 
            # #combine all the variable to the table
            # range01_Instrument_t <- cbind(range01_Instrument_msms, range01_Instrument_msd,range01_Instrument_msms2,range01_Instrument_PRS,range01_Instrument_ms_ms,range01_Instrument_FWHM,range01_Instrument_score,range01_Instrument_Intensity,range01_Instrument_ppm,range01_Instrument_cycletime,range01_Instrument_masserror)
            # #format the column names
            # colnames(range01_Instrument_t) <- c("MS/MS Identified [%]","Mass Standard Deviation [ppm]","MS/MS Identified","Peaks Repeatedly Sequenced [%]","MS/MS","Retention length (FWHM)","Score","Intensity","Mass precision [ppm]","Cycle time","Mass Error [ppm]")
            ##########################################################################
            
            # combine the selected (y and x) after normalization
            
            NoroldData <- cbind(scaled_y_oldData,range01_Instrument_t)
            calculateFile <- as.matrix(NoroldData[1,,drop=F])
            # use function to calculate the QC score for each old dataset, 
            Score <- round(calculateQC(calculateFile, Instrument_50_re),digits=3)
            Score <- paste('Quality score for "',selectedFileName, '" is: ',Score, sep="")
            ###If new dataset was clicked
          } else if (s[["key"]] == "input") {
            # order the columns
            
            newInput <- batch.2[,match(colnames(allFeatures), colnames(batch.2))]
            #this.instrument <- as.character(batch.2$Instrument)
            selectedInstrument <- batch.1[grep(this.instrument,batch.1$Instrument,ignore.case = FALSE), ]
            oldselectedInstrument <- selectedInstrument[,match(colnames(allFeatures),colnames(selectedInstrument))]
            selectedCombined<- rbind(oldselectedInstrument, newInput)
            
            
            # normalise the combineddata #
            #firstly normalize the combined y value
            
            #when the file being selected from the new input
            selectedFileName <- rownames(batch.2[batch.2$Instrument == this.instrument,])[s[["curveNumber"]]]
            #get that row with the selected filename for the 12 features
            newData <- newInput[grep(selectedFileName,row.names(newInput),ignore.case=TRUE),]
            #to normalise the new data #
            #firstly normalize the new y value
            
            y_newData <- newData[,1]
            scaled_y_newData<-(y_newData - min(selectedCombined[,1]))/(max(selectedCombined[,1])-min(selectedCombined[,1]))
            
            median_features <- apply(oldselectedInstrument[,2:12], 2, FUN = median)
            median_features <- t(median_features)
            median_features <- as.data.frame(median_features)
            
            #secondly normalize the new x values
            X_newData <- newData[,2:12]
            # assumes that  the column order of "median_features" and "X_QECl_remove" are identical
            range01_x_newData <- X_newData
            for (i in 1:ncol(range01_x_newData)) {
              range01_x_newData[,i] <- range01_x_newData[,i]/median_features[1,i]
            }
            
            # range01 <- function(x){(x-min(selectedCombined[,2:12]))/(max(selectedCombined[,2:12])-min(selectedCombined[,2:12]))}
            # range01_Instrument_msms<- range01(X_newData$`MS/MS Identified [%]`)
            # range01_Instrument_msd <- range01(X_newData$`Mass Standard Deviation [ppm]`)
            # range01_Instrument_msms2<-range01(X_newData$`MS/MS Identified`)
            # range01_Instrument_PRS <- range01(X_newData$`Peaks Repeatedly Sequenced [%]`)
            # range01_Instrument_ms_ms<- range01(X_newData$`MS/MS`)
            # range01_Instrument_FWHM <- range01(X_newData$`Retention length (FWHM)`)
            # range01_Instrument_score <- range01(X_newData$Score)
            # range01_Instrument_Intensity <- range01(X_newData$Intensity)
            # range01_Instrument_ppm <- range01(X_newData$`Mass precision [ppm]`)
            # range01_Instrument_cycletime<- range01(X_newData$`Cycle time`)
            # range01_Instrument_masserror<- range01(X_newData$`Mass Error [ppm]`)
            # 
            # #combine all the variable to the table
            # range01_x_newData <- cbind(range01_Instrument_msms, range01_Instrument_msd,range01_Instrument_msms2,range01_Instrument_PRS,range01_Instrument_ms_ms,range01_Instrument_FWHM,range01_Instrument_score,range01_Instrument_Intensity,range01_Instrument_ppm,range01_Instrument_cycletime,range01_Instrument_masserror)
            # #format the column names
            # colnames(range01_x_newData) <- c("MS/MS Identified [%]","Mass Standard Deviation [ppm]","MS/MS Identified","Peaks Repeatedly Sequenced [%]","MS/MS","Retention length (FWHM)","Score","Intensity","Mass precision [ppm]","Cycle time","Mass Error [ppm]")
            
            # combine the y and x after normalization
            normalize_newxy <- cbind(scaled_y_newData,as.matrix(range01_x_newData))
            
            ################################################################################################################
            #build the model depend on which instrument from the existing database for the new input selected 
            ################################################################################################################
            X.raw_Instrument <- selectedInstrument[,2:12]
            #read raw data Y (peptide value) for that selectedInstrument data
            y_Instrument <- selectedInstrument[,1]
            
            #find the high leverage points and remove from the existing database#############
            
            selectInstrument_leverage <- selectedInstrument[,1:12]
            
            colnames(X.raw_Instrument) <- c("MS.MS.Identifiedp", "MassStandardDevisationppm","MS.MS.Identified","PeaksRepeatedlySequencedp" ,"MS.MS" ,"Retention.length.FWHM","Score","Intensity","MassPrecision.ppm","CycleTime","MassError")
            results_Instrument <- lm(y_Instrument ~ MS.MS.Identifiedp + MassStandardDevisationppm+MS.MS.Identified+PeaksRepeatedlySequencedp +MS.MS +Retention.length.FWHM+Score+Intensity+MassPrecision.ppm+CycleTime+MassError, data=X.raw_Instrument)
            D<- cooks.distance(results_Instrument)
            
            #no of Feature(parametrs) / no of observers (p/n)
            
            #find the cut off value, identify the leverage datasets, then remove them
            HL<- as.matrix(which (D> qf(0.5,11,nrow(selectInstrument_leverage)-11)), rownames.force = NA)
            
            removeName <- as.list(row.names(HL)) 
            rownames(HL)<- row.names(HL)
            remove <- rownames(HL)
            
            #REMOVE the high leverage points from the datasets
            selectInstrument_remove <- selectInstrument_leverage[!rownames(selectInstrument_leverage)%in% remove,]
            
            #normalize the data from the existing database
            #Function to normalise the data (only existing database) #
            #firstly normalize the y value
            
            y_Instrument_remove <- selectInstrument_remove[,1]
            scaled_y_Instrument_re<-(y_Instrument_remove - min(y_Instrument_remove))/(max(y_Instrument_remove)-min(y_Instrument_remove))
            
            #secondly normalize the x values
            
            range01_Instrument_t <- selectInstrument_remove[,2:12]
            
            for (i in 1:ncol(range01_Instrument_t)) {
              range01_Instrument_t[,i] <- range01_Instrument_t[,i]/median_features[1,i]
            }
            
            # range01 <- function(k){(k-min(k))/(max(k)-min(k))}
            # 
            # range01_Instrument_msms<- range01(X_Instrument_remove$`MS/MS Identified [%]`)
            # range01_Instrument_msd <- range01(X_Instrument_remove$`Mass Standard Deviation [ppm]`)
            # range01_Instrument_msms2<-range01(X_Instrument_remove$`MS/MS Identified`)
            # range01_Instrument_PRS <- range01(X_Instrument_remove$`Peaks Repeatedly Sequenced [%]`)
            # range01_Instrument_ms_ms<- range01(X_Instrument_remove$`MS/MS`)
            # range01_Instrument_FWHM <- range01(X_Instrument_remove$`Retention length (FWHM)`)
            # range01_Instrument_score <- range01(X_Instrument_remove$Score)
            # range01_Instrument_Intensity <- range01(X_Instrument_remove$Intensity)
            # range01_Instrument_ppm <- range01(X_Instrument_remove$`Mass precision [ppm]`)
            # range01_Instrument_cycletime<- range01(X_Instrument_remove$`Cycle time`)
            # range01_Instrument_masserror<- range01(X_Instrument_remove$`Mass Error [ppm]`)
            # 
            #combine all the variable to the table
            # range01_Instrument_t <- cbind(range01_Instrument_msms, range01_Instrument_msd,range01_Instrument_msms2,range01_Instrument_PRS,range01_Instrument_ms_ms,range01_Instrument_FWHM,range01_Instrument_score,range01_Instrument_Intensity,range01_Instrument_ppm,range01_Instrument_cycletime,range01_Instrument_masserror)
            #format the column names
            # colnames(range01_Instrument_t) <- c("MS/MS Identified [%]","Mass Standard Deviation [ppm]","MS/MS Identified","Peaks Repeatedly Sequenced [%]","MS/MS","Retention length (FWHM)","Score","Intensity","Mass precision [ppm]","Cycle time","Mass Error [ppm]")
            
            
            #combine the y and x after normalization
            
            normalize_Instrument <- cbind(scaled_y_Instrument_re,range01_Instrument_t)
            
            
            
            ####Build the function for Instrument Specific Model analysis with normalized  datasets#############
            
            set.seed(1)
            cv.out_Instrument_t <- cv.glmnet(x=as.matrix(range01_Instrument_t), y= scaled_y_Instrument_re,alpha=0, nfolds=5)
            
            #regression with the best lambda
            ridge.mod_Instrument_t <- glmnet (x=as.matrix(range01_Instrument_t), y=scaled_y_Instrument_re,alpha = 0)
            
            
            #contribution values from 11 features
            ridge.coef_Instrument_t <- predict (ridge.mod_Instrument_t, type="coefficients", s=cv.out_Instrument_t$lambda.min)
            
            #before converting the 50% weights
            contributionInstrument_t <- t(ridge.coef_Instrument_t[-1,1])
            abs_contributionInstrument_t <- abs(contributionInstrument_t)
            sum_Instrument_t <- sum(abs_contributionInstrument_t)
            #get each weights of 11 feature among 50%
            weight_Instrument_t <- function(t){t/sum_Instrument_t} 
            #keep the sign to get the percentage of the weights
            norm_Instrument_t <- weight_Instrument_t(contributionInstrument_t)
            
            Instrument_50_t <- norm_Instrument_t *0.5
            #calculate the weights for 11 features and to 3 digits
            Instrument_50_re<- round(Instrument_50_t,digits=3)
            #write.csv(Instrument_50_re, file="./weights.csv") 
            
            ################################################################################################################
            calculateFile <- as.matrix(normalize_newxy[1,,drop=F])
            # to calculate the QC score for each new dataset, x is the new dataset of the row
            Score <- round(calculateQC(calculateFile, Instrument_50_re),digits=3)
            Score <- paste('Quality score for new input "',selectedFileName, '" is: ',Score, sep="")
            #tmp <- NormalizenewXY(var1, cvar2)
            # qcscore <- calculateQC(tmp, var4)
          }
          
        }
        
      })
    
    })
  
    
    
    
    
  }
