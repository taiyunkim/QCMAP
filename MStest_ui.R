

mstest_box <- function() {
  list(
    headerPanel("Quality Prediction with QCMAP for user provided data"),
    fluidRow(
      column(3,
             fluidRow(
               sidebarPanel(
                 mstest_box_sidebarPanel(),
                 width = "100%"
               )
             )
      ),
      column(9,
             mainPanel(
               mstest_box_mainPanel(),
               width = "100%"
             )
      )
    )
  )
}




mstest_box_sidebarPanel <- function() {
  list(
    checkboxInput(
      "ms_pred_example",
      label = "Example: Mouse brain sample",
      value = FALSE
    ),
    hr(),
    conditionalPanel(
      condition = "input.ms_pred_example == false",
      fileInput(
        "mstest_maxQuantFiles",
        label = h4(
          "Select your file(s) *",
          tags$span(
            `data-toggle` = "tooltip",
            title = "4 txt files (summary.txt, allPeptides.txt, msScans.txt and evidence.txt) OR zipped (.zip) file of your txt folder.\n
            Multiple zipped txt folder can be submitted for analysis of multiple experiment studies.",
            tags$span(class = "glyphicon glyphicon-info-sign")
          )
  
        ),
        multiple = TRUE,
        accept = c('.txt', ".zip")
      )
    ),
    
    
    div("Note: Maximum file size limit is 5GB", style="color:red"),
    div(textOutput("mstest_validationMessage"))
  )
}


mstest_box_mainPanel <- function() {
  list(
    tags$head(tags$script(type = "text/javascript", src = "js/proteomicsPlot.js")),
    tags$head(tags$script(type = "text/javascript", src = "js/tooltip.js")),

    tags$head(tags$link(type = "text/css", rel = "stylesheet", href = "css/style.css")),
    uiOutput("mstest_outputUI")

  )

}





