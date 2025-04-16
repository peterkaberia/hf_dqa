#' discrepancy_heatmap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_discrepancy_heatmap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns('heatmap'))
  )
}

#' discrepancy_heatmap Server Functions
#'
#' @noRd
mod_discrepancy_heatmap_server <- function(id, flagged_discrepancies){
  stopifnot(is.reactive(flagged_discrepancies))

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    summary_discrepancies <- reactive({
      req(flagged_discrepancies())
      summarize_discrepancies(flagged_discrepancies())
    })

    output$heatmap <- renderPlot({
      req(summary_discrepancies())
      generate_heatmap(summary_discrepancies())
    })
  })
}
