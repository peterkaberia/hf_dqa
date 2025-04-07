#' discrepancy_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_discrepancy_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('indicator_ui')),
    reactableOutput(ns('discrepancies_table'))
  )
}

#' discrepancy_table Server Functions
#'
#' @noRd
mod_discrepancy_table_server <- function(id, flagged_discrepancies){
  stopifnot(is.reactive(flagged_discrepancies))

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    discrepancies <- reactive({
      req(flagged_discrepancies(), input$indicator)
      get_discrepancies(flagged_discrepancies(), input$indicator)
    })

    output$indicator_ui <- renderUI({
      req(flagged_discrepancies())
      col_names <- colnames(flagged_discrepancies())
      indicator_choices <- sort(unique(gsub('flag_|_old', '', col_names[grepl('^flag_', col_names)])))
      selectInput(ns('indicator'), 'Select Indicator', choices = indicator_choices, selected = indicator_choices[1])
    })

    output$discrepancies_table <- renderReactable({
      req(discrepancies())
      reactable(discrepancies(),
                striped = TRUE,
                sortable = TRUE,
                resizable = TRUE,
                highlight = TRUE,
                pagination = TRUE,
                compact = TRUE,
                wrap = TRUE,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(25, 50, 100),
                defaultPageSize = 25)
    })

  })
}
