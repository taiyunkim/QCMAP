################################################################################
# ms_newDB
#
# a ui function that calls interface for creating a database.
################################################################################
ms_newDB <- function() {
  list(
    headerPanel(
      list("Create your database",
      tags$span(
        `data-toggle` = "tooltip",
        title = "This is to create a database (similar structure to 'allFeatures.csv') for this app.",
        tags$span(class = "glyphicon glyphicon-info-sign")
      ))
    ),
    textInput("ms_dbName", "Name of your Database"), # validate the name in the future
    textAreaInput(
      "ms_dbInstName", 
      list(
        "Names of your instrument in the dataset", 
        tags$span(
          `data-toggle` = "tooltip",
          title = "Write the names of you instrument specified in the filename for matching. Please separate the names by a comma (,).",
          tags$span(class = "glyphicon glyphicon-info-sign")
        )
      )
    ),
    fileInput("ms_dbInput", "Files", multiple = T, accept = ".zip"), # add a tooltip to explain what is expected as input
    div(textOutput("ms_dbInputWarning")),
    textOutput("ms_message"),
    downloadButton("ms_dbCreate", "Create")
  )
}