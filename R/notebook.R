# Notebook's basic functions
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


#' Return a Notebook
#'
#' If \code{id=NULL} the function runs an new empty notebook, else the notebook given as argument is run.
#'
#' @param id id of the notebook
#'
#' @return A shinyApp object which run in browser (see https://shiny.rstudio.com/reference/shiny/latest/shinyApp.html for more information).
#'
#' @examples
#' runNotebook()
#'
#' @import shiny
#' @import shinydashboard
#' @export
runNotebook <- function(id=NULL, port=1994) {
  message("Start notebook...") # add notebook name/id in the future

  ui <- notebookUI()

  shinyApp(ui=ui, server=notebookServer, options = list(port=port))
}


#' Notebook UI
#'
#' If \code{id=NULL} the function runs an new empty notebook, else the notebook given as argument is run.
#'
#' @param id id of the notebook
#'
#' @return A shinyApp object which run in browser (see https://shiny.rstudio.com/reference/shiny/latest/shinyApp.html for more information).
#'
#' @examples
#' runNotebook()
#'
#' @import shiny
#' @import shinydashboard
#' @export
notebookUI <- function(){
  header <- dashboardHeader(title = "{ ShinyNotebook }",
                            tags$li(tags$a(tags$span(icon("plus"), style="margin-right:10px")," Cell",
                                           `class`="dropdown-toggle action-button shiny-bound-input",
                                           `id`="addCellBtn"), `class`="dropdown"))
  sidebar <- dashboardSidebar()
  body <- dashboardBody()

  return(dashboardPage(header, sidebar, body))
}



#' Notebook Server
#'
#' If \code{id=NULL} the function runs an new empty notebook, else the notebook given as argument is run.
#'
#' @param id id of the notebook
#'
#' @return A shinyApp object which run in browser (see https://shiny.rstudio.com/reference/shiny/latest/shinyApp.html for more information).
#'
#' @examples
#' runNotebook()
#'
#' @import shiny
#' @import shinydashboard
#' @export
notebookServer <- function(input, output){

  # > Define session variable
  session.variables  <- SessionVariables$new(reactive=reactiveValues(),
                                             static=list(),
                                             private.reactive=reactiveValues(),
                                             private.static=list())

  session.variables$private.reactive[["cellCount"]] <- 0

  observeEvent(input[['addCellBtn']], {
    insertUI(
      selector = ".content",
      where = "beforeEnd",
      ui = cellUI(session.variables$private.reactive[["cellCount"]]) #otcBoxUI(l, "output_box")
    )
    session.variables$private.reactive[["cellCount"]] <- session.variables$private.reactive[["cellCount"]] + 1
    callModule(cell, session.variables$private.reactive[["cellCount"]])
  })
}

runNotebook()
