#' Read and Clean HFD Data from Excel
#'
#' Reads multiple sheets from a Health Facility Data (HFD) Excel file, cleans
#' column names, removes unnecessary rows, converts relevant columns to numeric,
#' and ensures required columns are present.
#'
#' @param path A string specifying the file path to the Excel file.
#'
#' @return  A named list (classed as `hfd_data`) of tibble objects, each
#'   representing a cleaned version of one sheet in the Excel file.
#'
#' @details
#' This function expects the following sheets in the Excel file:
#'
#' - **Admin_data**
#' - **Population_data**
#' - **Reporting_completeness**
#' - **Service_data_1**
#' - **Service_data_2**
#' - **Service_data_3**
#'
#' Column names are converted to lower case, spaces removed, and default names
#' (like `...1`, `...2`) are cleaned up. The first two rows of each sheet are
#' typically header rows, so they are removed. Columns that are irrelevant for
#' downstream analysis are dropped, and remaining columns are coerced to numeric
#' where appropriate.
#'
#' @export
read_hfd_data  <- function(path) {

  check_required(path)

  sheet_names <- c('Admin_data', 'Population_data', 'Reporting_completeness', 'Service_data_1', 'Service_data_2', 'Service_data_3')
  required_cols <- c('country', 'first_admin_level', 'district', 'year', 'month')

  sheet_names <- set_names(sheet_names)
  sheet_data <- map(sheet_names, ~ {
    tryCatch({
      read_excel(path, sheet = .x) %>%
        rename_with(~ tolower(gsub(' ', '', .))) %>%
        slice(-c(1,2)) %>%
        select(-starts_with('..')) %>%
        rename_with(~ gsub('\\.\\.\\.\\d+$', '', .)) %>%
        rename(district = any_of('district_name')) %>%
        mutate(
          across(any_of('year'), ~ as.integer(.)),
          across(any_of('month'), ~ factor(month, levels = month.name)),
          across(any_of(setdiff(names(.), required_cols)), ~ suppressWarnings(as.numeric(.)))
        ) %>%
        drop_na(any_of(required_cols))
    }, error = function(e) NULL
    )
  })

  return(structure(sheet_data, class = 'hfd_data'))
}

#' Merge HFD Service Data
#'
#' Merges multiple service-related sheets from an `hfd_data` object by
#' **district**, **year**, and **month**.
#'
#' @param .data An object of class `hfd_data`, as returned by [read_hfd_data()].
#'
#' @return A tibble with merged service data. If more than one service sheet is
#'   present, they are joined on the columns `district`, `year`, and `month`. If
#'   no service sheets are found, the function returns `NULL`.
#'
#' @details
#' The function first filters out non-service sheets (e.g., `Admin_data` and
#' `Population_data`) and removes any NULL or empty data. Then it uses
#' [left_join()] to progressively merge all service sheets into one.
#'
#' @export
merge_hfd_service_data  <- function(.data) {
  if (!inherits(.data, 'hfd_data')) {
    stop('\'hfd_data\' data is required.')
  }

  sheet_data <- .data[!names(.data) %in% c("Admin_data", "Population_data")] %>%
    discard(~ is.null(.) || nrow(.) == 0)

  data_length <- length(sheet_data)

  if (data_length > 1) {
    reduce(sheet_data, left_join, by = c("district", "year", "month"))
  } else if(data_length == 1) {
    sheet_data[[1]]
  } else {
    NULL
  }
}

#' Compare HFD Data Districts to a Reference
#'
#' Creates a summary of district occurrences in a given sheet and checks whether
#' these districts are found in a reference data frame.
#'
#' @param .data A tibble representing one sheet from the `hfd_data` object, or `NULL`.
#' @param source_name A string that names the sheet or data source (e.g., `"Admin_data"`).
#' @param reference_df A data frame containing a reference set of districts.
#' @param years The number of years expected for the data
#'
#' @return A tibble summarizing the number of occurrences of each district and
#'   marking whether the district is recognized in the reference data (`OK`) or
#'   needs further review (`CHECK`).
#'
#' @details
#' This function counts the number of times each district appears in a sheet,
#' then flags them as **OK** or **CHECK** depending on whether they exist in
#' the reference data frame. If the input `.data` is `NULL`, a default empty
#' summary is returned.
#'
#' @noRd
check_hfd_data  <- function(.data, source_name, reference_df, years) {
  reference_district <- reference_df %>%
    distinct(district) %>%
    pull(district)

  dt <- if (!is.null(.data)) {
    .data %>%
      count(district) %>%
      rename(Occurrences = n) %>%
      complete(district = reference_district,  fill = list(Occurrences = 0)) %>%
      mutate(
        Source = source_name,
        Missing_in_Reference = case_when(
          str_detect(source_name, 'Admin_data') & Occurrences == 1 ~ 'OK',
          str_detect(source_name, 'Population_data') & Occurrences == years ~ 'OK',
          str_detect(source_name, 'Service_data|Reporting_completeness') & Occurrences == years * 12 ~ 'OK',
          .default = 'CHECK'
        )
      )
  } else {
    tibble(district = character(),
           Occurrences = integer(),
           Missing_in_Reference = character(),
           Source = character())
  }

  dt %>%
    pivot_wider(
      names_from = Source,
      values_from = c(Occurrences, Missing_in_Reference),
      values_fill = list(Occurrences = 0, Missing_in_Reference = NA)
    )
}

#' Summarize HFD Data Audit
#'
#' Creates a summary table combining occurrences from various sheets to check if
#' districts match the reference `Admin_data`. Useful for a quick overview of
#' missing or inconsistent districts across multiple sheets in the `hfd_data`.
#'
#' @param .data An object of class `hfd_data`, as returned by [read_hfd_data()].
#' @param years The number of years expected for the data.
#'
#' @return A combined tibble showing the number of occurrences in each sheet and
#'   marking whether each district is recognized (OK) or requires manual review
#'   (CHECK).
#'
#' @details
#' This function internally calls [check_hfd_data()] on the various sheets in
#' the `hfd_data` object (Admin, Population, Reporting Completeness, Service
#' Data 1/2/3) and merges all of the results into a single table.
#'
#' @export
summarize_hfd_audit  <- function(.data, years) {

  if (!inherits(.data, 'hfd_data')) {
    stop('\'hfd_data\' data is required.')
  }

  check_required(years)
  if (!is_scalar_integerish(years)) {
    stop('\'years\' should be an integer.')
  }

  admin_data <- .data$Admin_data

  admin_count <- check_hfd_data(admin_data, "Admin_data", admin_data, years)
  pop_count <- check_hfd_data(.data$Population_data, "Population_data", admin_data, years)
  reporting_count <- check_hfd_data(.data$Reporting_completeness, "Reporting_completeness", admin_data, years)
  serv1_count <- check_hfd_data(.data$Service_data_1, "Service_data_1", admin_data, years)
  serv2_count <- check_hfd_data(.data$Service_data_2, "Service_data_2", admin_data, years)
  serv3_count <- check_hfd_data(.data$Service_data_3, "Service_data_3", admin_data, years)

  reduce(
    list(admin_count, pop_count, reporting_count, serv1_count, serv2_count, serv3_count),
    left_join,
    join_by(district)
  ) %>%
    mutate(
      across(starts_with('Occurrences_'), ~ replace_na(.x, 0))
    )
}
