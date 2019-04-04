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
                                           `id`="addCellBtn"), `class`="dropdown"),
                            tags$li(tags$a(tags$span(icon("save")),
                                           `class`="dropdown-toggle action-button shiny-bound-input",
                                           `id`="._bookmark_"), `class`="dropdown"))
  sidebar <- dashboardSidebar(
    sidebarMenu(style="position:fixed; width:230px; height:calc(100vh - 50px); overflow-y:scroll",
                div(id='end_menu_out_treat')
    )
  )
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

  enableBookmarking(store = "server")

  # > Define session variable
  session.variables  <- SessionVariables$new(reactive=reactiveValues(),
                                             static=list(),
                                             private.reactive=reactiveValues(),
                                             private.static=list())

  session.variables$private.reactive[["cellCount"]] <- 0
  session.variables$private.reactive[["cellNames"]] <- c()
  session.variables$private.reactive[["SessionCells"]] <- SessionCells$new(id=NULL, name=NULL)

  observeEvent(input[['addCellBtn']], {
    # Compute cell id
    new_cell_id <- session.variables$private.reactive[["cellCount"]] + 1
    session.variables$private.reactive[["cellCount"]] <- new_cell_id
    session.variables$private.reactive[["cellNames"]] <- c(session.variables$private.reactive[["cellNames"]], paste("Cell", new_cell_id))
    session.variables$private.reactive[["SessionCells"]]$addCell(id = new_cell_id, name = paste("Cell", new_cell_id))

    # Insert cell UI in content
    insertUI(
      selector = ".content",
      where = "beforeEnd",
      ui = cellUI(session.variables$private.reactive[["cellCount"]])
    )
    callModule(cell, session.variables$private.reactive[["cellCount"]], new_cell_id, session.variables)

    # Insert cell link in sidebar
    insertUI(
      selector = "#end_menu_out_treat",
      where = "beforeBegin",
      ui = tags$li(
        tags$a(renderText({ session.variables$private.reactive[["cellNames"]][new_cell_id] }),
               href=paste0("#", new_cell_id, "-cell")))
    )
  })

  # S'exécute lors de la sauvegarde
  onBookmark(function(state){
    state$values$session.variables <- SessionVariables$new(reactive=list(),
                                                           static=list(),
                                                           private.reactive=list(),
                                                           private.static=list())
    state$values$session.variables$static <- session.variables$static
    state$values$session.variables$private.static <- session.variables$private.static
    state$values$session.variables$reactive <- reactiveValuesToList(session.variables$reactive)
    state$values$session.variables$private.reactive <- reactiveValuesToList(session.variables$private.reactive)

  })

  onRestore(function(state){
    # Restore la session.variable
    session.variables <<- SessionVariables$new(reactive=list(),
                                               static=list(),
                                               private.reactive=list(),
                                               private.static=list())
    session.variables$static <<- state$values$session.variables$static
    session.variables$private.static <<- state$values$session.variables$private.static
    session.variables$reactive <<- reactiveValues()
    for (key in names(state$values$session.variables$reactive)){
      session.variables$reactive[[key]] <<- state$values$session.variables$reactive[[key]]
    }
    session.variables$private.reactive <<- reactiveValues()
    for (key in names(state$values$session.variables$private.reactive)){
      session.variables$private.reactive[[key]] <<- state$values$session.variables$private.reactive[[key]]
    }

    # Restore les cellules
  })
}

