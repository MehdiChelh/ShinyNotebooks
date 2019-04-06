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

  shinyApp(ui=notebookUI, server=notebookServer, options = list(port=port))
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
notebookUI <- function(request){
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
notebookServer <- function(input, output, session){
  # get_page <- function(session = shiny::getDefaultReactiveDomain()) {
  #   session$userData$shiny.router.page()$path
  # }
  enableBookmarking(store = "server")

  # NotebookSession
  #   NotebookSession is an Reference Class (see Reference Class documentation)
  #   which is pass through all the modules of the session through the session$userData$NS variable
  #
  #   This object can be seen as a slight wrapper to shiny library.
  #   It allows to easily share variable between notebook cells and bookmark/restore notebooks.
  session$userData$NS <- NotebookSession$new(reactive=reactiveValues(),
                                             static=list(),
                                             private.reactive=reactiveValues(),
                                             private.static=list())

  session$userData$NS$private.reactive[["cellCount"]] <- 0
  session$userData$NS$private.reactive[["cellNames"]] <- c()
  session$userData$NS$private.reactive[["SessionCells"]] <- SessionCells$new(id=NULL, name=NULL)

  # UI button
  #   The notebook UI is quite minimalist, thus there arren't a lot of observeEvent()
  #   The few ones are defined bellow.
  #   They concern the following buttons : addCellBtn, ...
  observeEvent(input[['addCellBtn']], {
    # Compute cell id
    new_cell_id <- session$userData$NS$private.reactive[["cellCount"]] + 1
    session$userData$NS$private.reactive[["cellCount"]] <- new_cell_id
    session$userData$NS$private.reactive[["cellNames"]] <- c(session$userData$NS$private.reactive[["cellNames"]], paste("Cell", new_cell_id))
    # session$userData$NS$private.reactive[["SessionCells"]]$addCell(id = new_cell_id, name = paste("Cell", new_cell_id))

    # Insert cell in UI
    session$userData$NS$insert_cell_UI(new_cell_id)
  })


  # Bookmarking is slightly refined with NotebookSession
  #
  #   Bookmarking is very convenient in shiny as it automates bookmark procedure.
  #   However, bookmarking has some intrinsinc limitations : can't handle UI's inserted through insertUI(), ...
  #   Thus bookmarking need to be revised in the NotebookSession framework.
  #
  #   Currently onBookmark and onRestore call session bookmarking are defined below.
  #   Future improvements : make onBookmark() and onRestore() wrappers
  #
  #   > Bookmark NotebookSession
  #     called if the session is being bookmarked (bookmark button)
  onBookmark(function(state){
  #     1. Bookmark NotebookSession
    state$values$NS <- session$userData$NS$bookmark_state()
  #     2. Bookmark cells
  })
  #
  #   > Restore NotebookSession
  #     called if the session is being restored from a bookmark
  onRestore(function(state){
  #     1. Restore NotebookSession
    session$userData$NS$restore_from_bookmarked_state(state)
  #     2. Restore cells
    lapply(1:session$userData$NS$private.reactive[["cellCount"]], function(cell_id){
      session$userData$NS$insert_cell_UI(cell_id)
    })
  })
}

