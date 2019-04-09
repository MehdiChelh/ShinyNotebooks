# Cell's basic functions
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


#' Cell UI
#'
#' If \code{id=NULL} the function runs an new empty notebook, else the notebook given as argument is run.
#'
#' @param id id of the notebook
#'
#' @return A shinyApp object which run in browser (see https://shiny.rstudio.com/reference/shiny/latest/shinyApp.html for more information).
#'
#' @examples
#' cellUI(id = 3)
#'
#' @import shiny
#' @import shinydashboard
#' @export
cellUI <- function(id){
  ns <- NS(id)

  fluidRow(id=ns("cell"),
    shinydashboard::box(width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
                        title=tags$span(tags$span(actionLink(
                          inputId = ns("title-btn"),
                          label = icon("pencil"))),
                          tags$span(textOutput(ns("title-text"), inline = TRUE))),
                        selectizeInput(
                          ns('cellContentChoice'), 'Cell content', choices = c("Data import",
                                                                               "Data export",
                                                                               "Plot > Histogram",
                                                                               "Plot > Scatter",
                                                                               "Plot > Bar",
                                                                               "Plot > Pie",
                                                                               "Plot > Contour",
                                                                               "Plot > Surface"),
                          options = list(
                            placeholder = 'Please select an option below',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
                        ),
                        #uiOutput(ns("cellContent")),
                        plotlyOutput(ns("cellContent")),
                        textInput(ns("txtIn"), label = "ok"),
                        textOutput(ns("txtOut")))
  )
}

#' Cell server logic
#'
#' If \code{id=NULL} the function runs an new empty notebook, else the notebook given as argument is run.
#'
#' @param id id of the notebook
#'
#' @return A shinyApp object which run in browser (see https://shiny.rstudio.com/reference/shiny/latest/shinyApp.html for more information).
#'
#' @examples
#' cellUI(id = 3)
#'
#' @import shiny
#' @import shinydashboard
#' @import plotly
#' @export
cell <- function(input, output, session, cell_id, cell.session){
  # Cell title/name
  output[["title-text"]] <- renderText({session$userData$NS$private.reactive[["cellNames"]][cell_id]})

  # Button for modifying cell title/name
  #   This button allow you to open a modal to change cell name
  observeEvent(input[["title-btn"]], {
    showModal(modalDialog(
      textInput(session$ns("modalTxtInput"), "Modify Box Name",
                value = session$userData$NS$private.reactive[["cellNames"]][cell_id],
                placeholder = ''
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(session$ns("okModal"), "OK")
      )
    ))
    observeEvent(input$okModal,{
      session$userData$NS$private.reactive[["cellNames"]][cell_id] <- input$modalTxtInput
      cell.session$userData$name <- input$modalTxtInput
      removeModal()
    })

  })

  # Cell content
  # Input parameters : cell, session.variables,
  observeEvent(input[["cellContentChoice"]], {
    if (input[["cellContentChoice"]] != "") {

      # renderUI({
      #   moduleUI[[input[["cellContentChoice"]]]](session.variables)
      # })
      # callModule()

      # f <- function(){
      #   if (input$txtIn == "m"){
      #     3
      #   }else{
      #     4
      #   }
      #   cell.session$output_state[[output_name]] <- p
      # }
      # output$txtOut <- renderText(f())

      # ModuleServer
      #   server <- function(){
      #     cell.session$bookmarked_inputs <- c()
      #     cell.session$bookmarked_outputs <- c()
      #
      #     output[["plot"]] <- renderPlotly({
      #       p <- plot_ly(x = 1:10, y = 2 * (1:10))
      #       p
      #     })
      #
      #     onBookmark({
      #       cell.session$Bookmark()
      #     })
      #     onRestore({
      #       cell.session$Restore()
      #     })
      #   }

    }
  })


  # onBookmark({
  #
  # })
  #
  # onRestore({
  #
  # })
}


#' Cell object
#'
#' If \code{id=NULL} the function runs an new empty notebook, else the notebook given as argument is run.
#'
#' @param id id of the notebook
#'
#' @return A shinyApp object which run in browser (see https://shiny.rstudio.com/reference/shiny/latest/shinyApp.html for more information).
#'
#' @examples
#' cellUI(id = 3)
#'
#' @import shiny
#' @import shinydashboard
#' @export
CellSession <- setRefClass("CellSession", fields = c("id", "name", "userData"))
CellSession$methods(

)
