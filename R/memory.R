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
SessionVariables <- setRefClass("SessionVariables", fields = c("reactive", "static", "private.reactive", "private.static"))
SessionVariables$methods(
  filterVariableName = function(typeFilter, onField=c("reactive", "static")){
    # no type pour l'instant
    if (onField == "static"){
      names(static)
    } else if (onField == "reactive"){
      names(reactiveValuesToList(reactive))
    } else {
      warning("onFiled should take either value 'reactive' or 'static'.")
    }
  }
)

a <- SessionVariables$new(reactive=reactiveValues(), static=list(), private.reactive=reactiveValues(), private.static=list())
a$private.reactive <- reactiveValues()
a$field("private.static")
