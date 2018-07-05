library(shiny)
library(httr)
library(jsonlite)

source("./MS_ui.R")
source("./MS_server.R")
source("./MSdb_ui.R")
source("./MSdb_server.R")
source("./MStest_ui.R")
source("./MStest_server.R")


shinyApp(
  ui <- shinyUI(
    fluidPage(
      br(),
      tags$head(
        tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');"))
      ),
      navbarPage(
        theme=shinytheme("cosmo"),
        title = "QCMAP",
        id = "navbarPage",
        tabPanel(
          "HeLa",
          ms_box()
        ),
        tabPanel(
          "Predictions",
          mstest_box()
        ),
        tabPanel(
          "Database",
          ms_newDB()
        )
      )
    )
  ),
  server <- shinyServer(
    function(input,output,session) {
      msServer(input, output, session)
      msDbServer(input, output, session)
      # msuserServer(input, output, session)
      mstestServer(input, output, session)
    }
  )
)

