# author - Taiyun Kim

library(shiny)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(plotly)

options(shiny.maxRequestSize=5*1024^3)
Sys.setlocale("LC_ALL","C")

# lastCodeUpdate = "24th April, 2018"
# instrumentNames <- read.csv("allFeatures.csv", colClasses = c(rep("NULL", 14), "character"), check.names = F)[,1]
# instrumentNames <- unique(instrumentNames)


################################################################################
# QC_recaptchaUI 
# 
# function to call Google reCAPTCHA on ui side
################################################################################
QC_recaptchaUI <- function(id, sitekey = Sys.getenv("recaptcha_sitekey"), ...) {
  ns <- NS(id)
  
  tagList(tags$div(
    shiny::tags$script(
      src = "https://www.google.com/recaptcha/api.js",
      async = NA,
      defer = NA
    ),
    tags$script(
      paste0("shinyCaptcha = function(response) {
             Shiny.onInputChange('", ns("recaptcha_response"),"', response);
}"
    )),
    tags$form(
      class = "shinyCAPTCHA-form",
      action = "?",
      method = "POST",
      tags$div(class = "g-recaptcha", `data-sitekey` = sitekey, `data-callback` = I("shinyCaptcha")),
      tags$br()
    )
  ))
}


################################################################################
# ms_box
#
# a ui function that calls side panels and main panels.
################################################################################
ms_box <- function() {
  list(
    headerPanel("Quality Control of MAss spectrometry-based Protemics systems"),
    fluidRow(
      column(3,
        fluidRow(
          sidebarPanel(
            ms_box_sidebarPanel(),
            width = "100%"
          )
        ),
        fluidRow(
          uiOutput("ms_saveUI")
        )
      ),
      column(9,
        mainPanel(
          ms_box_mainPanel(),
          width = "100%"
        )
      )
    )
    # div(h4(paste("Code last updated on: ", lastCodeUpdate)))
  )
}


################################################################################
# ms_box_sidebarPanel
#
# a function that return list of inputs for the MS application side panel
################################################################################
ms_box_sidebarPanel <- function() {
  list(
    checkboxInput(
      "ms_example",
      label = "Try with demo files",
      value = FALSE
    ),
    hr(),
    conditionalPanel(
      condition = "input.ms_example == true",
      selectizeInput(
        'ms_try_example', 'Demo files', choices = c("20171031_QEHF_demo1", 
                                                    "20171103_QEHF_demo2", 
                                                    "20171109_QEplus_demo3", 
                                                    "20171123_QEplus_demo4"),
        options = list(
          placeholder = 'Please select a demo file below',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    ),
    conditionalPanel(
      condition = "input.ms_example == false",
      fileInput(
        "maxQuantFiles", 
        label = h4(
          "Select your file(s)", 
          tags$span(
            `data-toggle` = "tooltip",
            title = "4 txt files (summary.txt, allPeptides.txt, msScans.txt and evidence.txt) OR zipped (.zip) file of your txt folder",
            tags$span(class = "glyphicon glyphicon-info-sign")
          )
          
        ), 
        multiple = TRUE, 
        accept = c('.txt', ".zip")
      ),
      
      div("Note: Maximum file size limit is 5GB", style="color:red"),
      div(textOutput("ms_validationMessage"))
    )
    
    
    
  )
}


################################################################################
# saveDB_sidebarPanel
#
# A function that return an action button with QC_recaptchaUI to save the output data to the database
# This panel should only be shown when there is an output.
################################################################################
saveDB_sidebarPanel <- function() {
  div(
    h4("Click to save this batch to the Database"),
    QC_recaptchaUI("MS_recaptcha_test", sitekey = "6Lfg_jYUAAAAAAVwxpEHlaRupSp87bl2Wj1pACkn"),
    actionButton("MS_saveDB", "Save")
  )
}



################################################################################
# ms_box_mainPanel
#
# A function that return a list of outputs for the main panel int the MS application 
################################################################################
ms_box_mainPanel <- function() {
  # uiOutput("ms_box_mainPanel_ui")
  
  instrumentNames <- read.csv("allFeatures.csv", colClasses = c(rep("NULL", 14), "character"), check.names = F)[,1]
  instrumentNames <- unique(instrumentNames)

  list(
    tags$head(tags$script(type = "text/javascript", src = "js/proteomicsPlot.js")),
    tags$head(tags$script(type = "text/javascript", src = "js/tooltip.js")),

    tags$head(tags$link(type = "text/css", rel = "stylesheet", href = "css/style.css")),

    do.call(
      tabsetPanel, c(
        id = "t",
        lapply(1:length(instrumentNames), function(i) {
          tabPanel(
            title = instrumentNames[i],
            plotlyOutput(paste0("bar_", instrumentNames[i], "_history", sep = "")),
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(paste("ms_comment_", instrumentNames[i], sep = ""), "ADD comment", style = "background-color:#3B5998; position: right;"),
                actionButton(paste("ms_edit_comment_", instrumentNames[i], sep = ""), "EDIT comment", style = "background-color:#FFFFFF; color: #000000; position: right;")
              )
            ),
            br(),
            h3(textOutput(paste0("score_",instrumentNames[i],"_check",sep=""))),
            br(),
            hr(),

            #textOutput(paste0("score_",instrumentNames[i],"_check",sep="")),
            plotlyOutput(paste0("box_", instrumentNames[i], sep = ""), height = "500px")
          )
        }),

        list(
          tabPanel(
            title = "Comparisons",
            plotlyOutput("bar_plotHistory"),
            plotlyOutput("box_individualFeatures", height = "1000px")
          )
        )
      )
    )
  )
}








