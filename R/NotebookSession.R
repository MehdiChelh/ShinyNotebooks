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


#' SessionVariables object
#'
#' Container of the session variables.
#'
#'
#' @examples
#' SessionVariables()
#'
#' @import shiny
#' @export
NotebookSession <- setRefClass("SessionVariables", fields = c("reactive", "static", "private.reactive", "private.static"))
NotebookSession$methods(
  filterVariableName = function(typeFilter, onField=c("reactive", "static")){
    # no type pour l'instant
    if (onField == "static"){
      names(static)
    } else if (onField == "reactive"){
      names(reactiveValuesToList(reactive))
    } else {
      warning("onFiled should take either value 'reactive' or 'static'.")
    }
  },
  # > insertCellUI : Insert a Cell with id cell_id
  insert_cell_UI = function(cell_id, session.cell){
    insertUI(
      selector = ".content",
      where = "beforeEnd",
      ui = cellUI(cell_id, session.cell)
    )
    callModule(cell, cell_id, cell_id, session.cell)

    # Insert cell link in sidebar
    insertUI(
      selector = "#end_menu_out_treat",
      where = "beforeBegin",
      ui = tags$li(
        tags$a(renderText({ .self$private.reactive[["cellNames"]][cell_id] }),
               href=paste0("#", cell_id, "-cell")))
    )
  },
  # > restore_from_bookmarked_state : Restore previous NotebookSession from borrmarked values (bookmarked values are not reactive).
  restore_from_bookmarked_state = function(state){
    .self$static <- state$values$NS$static
    .self$private.static <- state$values$NS$private.static
    .self$reactive <- reactiveValues()
    for (key in names(state$values$NS$reactive)){
      .self$reactive[[key]] <- state$values$NS$reactive[[key]]
    }
    .self$private.reactive <- reactiveValues()
    for (key in names(state$values$NS$private.reactive)){
      .self$private.reactive[[key]] <- state$values$NS$private.reactive[[key]]
    }
  },
  # > bookmark_state : Bookmark current NotebookSession (bookmarked values are not reactive).
  bookmark_state = function(){
    NS <- NotebookSession$new(reactive=list(),
                              static=list(),
                              private.reactive=list(),
                              private.static=list())
    NS$static <- .self$static
    NS$private.static <- .self$private.static
    NS$reactive <- reactiveValuesToList(.self$reactive)
    NS$private.reactive <- reactiveValuesToList(.self$private.reactive)
    return(NS)
  }
)

# a <- SessionVariables$new(reactive=reactiveValues(), static=list(), private.reactive=reactiveValues(), private.static=list())
# a$private.reactive <- reactiveValues()
# a$field("private.static")
