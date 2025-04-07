#' district_checks UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_district_checks_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    row_heights = list('auto', 1),
    col_widths = 12,
    layout_column_wrap(
      width = 1/4,
      fill = FALSE,
      numericInput(ns('start_year'), 'Start Year', value = NULL, min = 2019, max = 2025, step = 1),
      numericInput(ns('end_year'), 'End Year', value = NULL, min = 2019, max = 2025, step = 1),
    ),
    reactableOutput(ns('data_checks'))
  )
}

#' district_checks Server Functions
#'
#' @noRd
mod_district_checks_server <- function(id, data_hfd, data){
  stopifnot(is.reactive(data_hfd))
  stopifnot(is.reactive(data))

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    years <- reactive({
      req(input$start_year, input$end_year)
      if (is.null(input$start_year) || is.null(input$end_year)) return(NULL)
      if (input$end_year < input$start_year) return(NULL)
      return(input$end_year - input$start_year + 1)
    })

    audit_summary <- reactive({
      req(data_hfd(), years())
      summarize_hfd_audit(data_hfd(), years())
    })

    observeEvent(data(), {
      req(data())

      min_year <- min(data()$year, na.rm = TRUE)
      max_year <- max(data()$year, na.rm = TRUE)

      updateNumericInput(session, 'start_year', value = min_year)
      updateNumericInput(session, 'end_year', value = max_year)
    })

    output$data_checks <- renderReactable({
      req(audit_summary())
      summary_table <- audit_summary()
      reactable(summary_table,
                pagination = FALSE,
                resizable = TRUE,
                highlight = TRUE,
                compact = TRUE,
                wrap = TRUE,
                striped  = TRUE,
                outlined  = TRUE,
                columns = list(
                  district = colDef(name = 'District', sticky = 'left', footer = 'Total Districts'),
                  Occurrences_Admin_data = colDef(name = '# of Occurrences in \'Admin_data\'', cell = function(value, index) {
                    missing <- summary_table[index, 'Missing_in_Reference_Admin_data']

                    if (is.na(missing) || missing == 'CHECK') {
                      paste('\u274c', value)
                    } else {
                      paste('\u2714\ufe0f', value)
                    }
                  }, footer = function(values) sprintf("%.0f", sum(values))),
                  Occurrences_Population_data = colDef(name = '# of Occurrences in \'Population_data\'', cell = function(value, index) {
                    missing <- summary_table[index, 'Missing_in_Reference_Population_data']

                    if (is.na(missing) || missing == 'CHECK') {
                      paste('\u274c', value)
                    } else {
                      paste('\u2714\ufe0f', value)
                    }
                  }),
                  Occurrences_Reporting_completeness = colDef(name = '# of Occurrences in \'Reporting_completeness\'', cell = function(value, index) {
                    missing <- summary_table[index, 'Missing_in_Reference_Reporting_completeness']

                    if (is.na(missing) || missing == 'CHECK') {
                      paste('\u274c', value)
                    } else {
                      paste('\u2714\ufe0f', value)
                    }
                  }),
                  Occurrences_Service_data_1 = colDef(name = '# of Occurrences in \'Service_data_1\'', cell = function(value, index) {
                    missing <- summary_table[index, 'Missing_in_Reference_Service_data_1']

                    if (is.na(missing) || missing == 'CHECK') {
                      paste('\u274c', value)
                    } else {
                      paste('\u2714\ufe0f', value)
                    }
                  }),
                  Occurrences_Service_data_2 = colDef(name = '# of Occurrences in \'Service_data_2\'', cell = function(value, index) {
                    missing <- summary_table[index, 'Missing_in_Reference_Service_data_2']

                    if (is.na(missing) || missing == 'CHECK') {
                      paste('\u274c', value)
                    } else {
                      paste('\u2714\ufe0f', value)
                    }
                  }),
                  Occurrences_Service_data_3 = colDef(name = '# of Occurrences in \'Service_data_3\'', cell = function(value, index) {
                    missing <- summary_table[index, 'Missing_in_Reference_Service_data_3']

                    if (is.na(missing) || missing == 'CHECK') {
                      paste('\u274c', value)
                    } else {
                      paste('\u2714\ufe0f', value)
                    }
                  }),
                  Missing_in_Reference_Admin_data = colDef(show = FALSE),
                  Missing_in_Reference_Population_data = colDef(show = FALSE),
                  Missing_in_Reference_Reporting_completeness = colDef(show = FALSE),
                  Missing_in_Reference_Service_data_1 = colDef(show = FALSE),
                  Missing_in_Reference_Service_data_2 = colDef(show = FALSE),
                  Missing_in_Reference_Service_data_3 = colDef(show = FALSE)
                ),
                defaultColDef = colDef(footerStyle = list(fontWeight = "bold")))
    })
  })
}
