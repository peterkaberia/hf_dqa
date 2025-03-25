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
      nav_panel('Summary Heatmap', plotOutput(ns('heatmap'))),
      nav_panel(
        'Discrepancies Table',
        tagList(
          uiOutput(ns('indicator_ui')),
          reactableOutput(ns('discrepancies_table'))
        )
      )
    )
  )
}

#' audit Server Functions
#'
#' @noRd
mod_audit_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    data_old <- reactive({
      req(input$data_old)
      load_data(input$data_old$datapath, c('Service_data_1', 'Service_data_2', 'Service_data_3', "Reporting_completeness")) |>
        select(district, year, month,any_of(c("bcg", "penta1", "penta2", "penta3", "measles1", "measles2", "opv1", "opv2",
                                              "opv3", "pcv1" , "pcv2", "pcv3", "rota1", "rota2", "ipv1", "ipv2", "anc1" )), ends_with("reporting_rate"))
    })

    data_new <- reactive({
      req(input$data_new)
      load_data(input$data_new$datapath, c('Service_data_1', 'Service_data_2', 'Service_data_3', "Reporting_completeness")) |>
      select(district, year, month,any_of(c("bcg", "penta1", "penta2", "penta3", "measles1", "measles2", "opv1", "opv2",
        "opv3", "pcv1" , "pcv2", "pcv3", "rota1", "rota2", "ipv1", "ipv2", "anc1" )), ends_with("reporting_rate"))
    })

    flagged_discrepancies <- eventReactive(input$process, {
      req(data_old(), data_new())
      flag_discrepancies(data_old(), data_new(), input$threshold / 100)
    })

    output$indicator_ui <- renderUI({
      req(flagged_discrepancies())
      col_names <- colnames(flagged_discrepancies())
      indicator_choices <- sort(unique(gsub('flag_|_old', '', col_names[grepl('^flag_', col_names)])))
      selectInput(session$ns('indicator'), 'Select Indicator', choices = indicator_choices, selected = indicator_choices[1])
    })

    discrepancies <- reactive({
      req(flagged_discrepancies(), input$indicator)
      get_discrepancies(flagged_discrepancies(), input$indicator)
    })

    output$discrepancies_table <- renderReactable({
      req(discrepancies())
      reactable(discrepancies(),
                searchable = FALSE,
                striped = TRUE,
                sortable = TRUE,
                resizable = TRUE,
                highlight = TRUE,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(25, 50, 100),
                defaultPageSize = 25)
    })

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
