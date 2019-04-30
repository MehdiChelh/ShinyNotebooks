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
cellUI <- function(id, cell.session){
  ns <- NS(id)

  if ( TRUE ){ #("textInput" == "textInput")# & (bk_id %in% names(cell.session$bookmark))
    # print(cell.session$bookmark[[bk_id]]$value)

    sel <- "Data import"

  }
  else{
    sel <- NULL
  }
  sel <- "Data import"
  fluidRow(id=ns("cell"),
    shinydashboard::box(width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
                        title=tags$span(tags$span(actionLink(
                          inputId = ns("title-btn"),
                          label = icon("pencil"))),
                          tags$span(textOutput(ns("title-text"), inline = TRUE))),
                        selectizeInput(
                          ns('cellContentChoice'), 'Cell content', choices = c("Empty cell",
                                                                               "Data import",
                                                                               "Data export",
                                                                               "Plot > Histogram",
                                                                               "Plot > Scatter",
                                                                               "Plot > Bar",
                                                                               "Plot > Pie",
                                                                               "Plot > Contour",
                                                                               "Plot > Surface"),
                          selected = "Data import",
                          options = list(
                            placeholder = 'Please select an option below'
                            # ,
                            # onInitialize = I('function() { this.setValue(""); }')
                          )
                        ),
                        #uiOutput(ns("cellContent")),
                        plotlyOutput(ns("cellContent")))
                        # textInput(ns("txtIn"), label = "ok"),
                        # textInput(ns("txtIn2"), label = "ok2"),
                        # textOutput(ns("txtOut")))
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
  # Need to be at the begining of the server
  cell.session$bookmarkIds <- list("txtIn"="textInput", "txtOut"="textOutput") # les outputs ne doivent être restoré qu'en cas de force majeure
  # cell.session$restore()
  # for (bk_id in names(cell.session$bookmarkIds)){
  #   if ((cell.session$bookmarkIds[[bk_id]] == "textInput") ){ # & (bk_id %in% names(cell.session$bookmark))
  #     # print(cell.session$bookmark[[bk_id]]$value)
  #     print("where is w")
  #     print(session$ns(bk_id))
  #     updateTextInput(session = session, inputId = session$ns(bk_id), value = "wooo")
  #   }
  # }

  updateTextInput(session = session, inputId = "txtIn2", value = "wooo")
  updateTextInput(session = session, inputId = "#1-txtIn", value = "wooo")
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
    if (input[["cellContentChoice"]] != "Empty cell") {

      output[["cellContent"]] <- renderPlotly({
        plot_ly(mtcars, x=~mpg, y=~disp, type='scatter', mode='markers')
      })
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
      # print(attributes(output))
      # print(names(output))
      # print(names(output$ns))
      # print(names(output$impl))
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
  # le restore n'est pas capté, il faut fabriqué le restore, mais en fait c'est avantageux ici
  # onRestore(function(state){
  #   print(session$userData$NS$static$test)
  #   print("wesh")
  #   output$txtOut <- renderText({ session$userData$NS$static$test })
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
CellSession <- setRefClass("CellSession", fields = c("id", "name", "userData", "bookmark", "bookmarkIds", "ns"))
CellSession$methods(

)
