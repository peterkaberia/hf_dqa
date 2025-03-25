#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  tagList(
    # theme = bs_theme(version = 5),
    golem_add_external_resources(),
    mod_audit_ui('audit_1')
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "dataAudit"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
