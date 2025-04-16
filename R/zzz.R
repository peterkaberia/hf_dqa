.onLoad <- function(libname, pkgname) {

  options(
    shiny.maxRequestSize = 1024 * 1024^2 # Increase the maximum upload size to 1GB
  )

  invisible()
}
