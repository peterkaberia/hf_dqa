#' audit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_audit_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    title = 'Discrepancy Analysis',
    theme = bs_theme(
      bootswatch = 'zephyr',
      navbar_bg = "#25443B"
    ),
    sidebar = sidebar(
      fileInput(ns('data_old'), 'Upload Old Data', accept = '.xlsx'),
      fileInput(ns('data_new'), 'Upload New Data', accept = '.xlsx'),
      numericInput(ns('threshold'), 'Discrepancy Threshold (%)', value = 5, min = 0, step = 0.1),
      actionButton(ns('process'), 'Process Data')
    ),
    navset_card_underline(
      nav_panel('New Data District Checks', mod_district_checks_ui(ns('district_checks_1'))),
      nav_panel('Discrepancies Heatmap', mod_discrepancy_heatmap_ui(ns('discrepancy_heatmap_1'))),
      nav_panel('Discrepancies Table', mod_discrepancy_table_ui(ns('discrepancy_table_1')))
    )
  )
}

#' audit Server Functions
#'
#' @noRd
mod_audit_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    data_old_hfd <- reactive({
      req(input$data_old)
      read_hfd_data(input$data_old$datapath)
    })

    data_new_hfd <- reactive({
      req(input$data_new)
      read_hfd_data(input$data_new$datapath)
    })

    data_old <- reactive({
      req(data_old_hfd())
      merge_hfd_service_data(data_old_hfd()) %>%
        select(district, year, month,any_of(c("bcg", "penta1", "penta2", "penta3", "measles1", "measles2", "opv1", "opv2",
                                              "opv3", "pcv1" , "pcv2", "pcv3", "rota1", "rota2", "ipv1", "ipv2", "anc1" )), ends_with("reporting_rate"))
    })

    data_new <- reactive({
      req(data_new_hfd())
      merge_hfd_service_data(data_new_hfd()) %>%
        select(district, year, month,any_of(c("bcg", "penta1", "penta2", "penta3", "measles1", "measles2", "opv1", "opv2",
                                              "opv3", "pcv1" , "pcv2", "pcv3", "rota1", "rota2", "ipv1", "ipv2", "anc1" )), ends_with("reporting_rate"))
    })

    flagged_discrepancies <- eventReactive(input$process, {
      req(data_old(), data_new())
      flag_discrepancies(data_old(), data_new(), input$threshold / 100)
    })

    mod_district_checks_server('district_checks_1', data_new_hfd, data_new)
    mod_discrepancy_heatmap_server('discrepancy_heatmap_1', flagged_discrepancies)
    mod_discrepancy_table_server('discrepancy_table_1', flagged_discrepancies)
  })
}
