#' Load and Clean Data from Excel Sheets
#'
#' Reads multiple sheets from an Excel file, cleans column names, removes unnecessary
#' rows, and converts relevant columns to numeric format for analysis.
#'
#' @param path A string specifying the file path to the Excel file.
#' @param sheet_names A character vector of sheet names to read from the Excel file.
#'
#' @return A cleaned `tibble` containing merged data from the specified sheets.
#'
#' @export
load_data <- function(path, sheet_names) {
  sheet_data <- map(sheet_names, ~ {
    tryCatch({
      read_excel(path, sheet = .x) %>%
        rename_with(~ tolower(gsub(' ', '', .))) %>%
        slice(-c(1,2)) %>%
        select(-starts_with('..')) %>%
        rename_with(~ gsub('\\.\\.\\.\\d+$', '', .)) %>%
        mutate(
          year = as.integer(year),
          month = factor(month, levels = month.name),  # Ensure months are ordered
          across(-c(district, year, month), ~ suppressWarnings(as.numeric(.))) # Convert all other columns to numeric
        )
    }, error = function(e) NULL
    )
  }) %>%
    discard(~ is.null(.) || nrow(.) == 0)

  reduce(sheet_data, left_join, by = c("district", "year", "month"))
}

#' Flag Data Discrepancies Between Old and New Data
#'
#' Compares `data_old` and `data_new`, flagging discrepancies where values differ
#' beyond a given threshold.
#'
#' @param data_old A `tibble` representing the old dataset.
#' @param data_new A `tibble` representing the new dataset.
#' @param threshold A numeric value (default = `0.05` or 5%) specifying the discrepancy threshold.
#'
#' @return A `tibble` with flagged discrepancies (`TRUE`/`FALSE`) for each column.
#'
#' @export
flag_discrepancies <- function(data_old, data_new, threshold = 0.05) {
  data_old %>%
    inner_join(data_new, by = c("district", "year", "month"), suffix = c("_old", "_new")) %>%
    mutate(
      across(ends_with('_old'), ~ {
        diff <-  abs(. - get(sub("_old", "_new", cur_column())))
        diff > threshold * .
      }, .names = 'flag_{.col}')
    )
}

#' Extract Discrepancies for a Specific Indicator
#'
#' Filters the flagged discrepancies for a specific indicator and retains relevant columns.
#'
#' @param discrepancies A `tibble` containing flagged discrepancies.
#' @param indicator A string specifying the name of the indicator to extract discrepancies for.
#'
#' @return A `tibble` containing only the discrepancies for the specified indicator.
#'
#' @export
get_discrepancies <- function(discrepancies, indicator) {
  discrepancies %>%
    filter(if_any(matches(paste0('^flag_', indicator)), ~ . == TRUE)) %>%
    select(district, year, month, starts_with(indicator))
}

#' Summarize Discrepancies by Year
#'
#' Computes the proportion of discrepancies for each indicator, summarized by year.
#'
#' @param discrepancies A `tibble` containing flagged discrepancies.
#'
#' @return A `tibble` summarizing the proportion of discrepancies per year for each indicator.
#'
#' @export
summarize_discrepancies <- function(discrepancies) {
  discrepancies %>%
    select(district, year, month, starts_with("flag_")) %>%
    rename_with(~ gsub('flag_(.+)_old', '\\1', .), starts_with('flag_')) %>%
    # mutate(across(where(is.logical), as.integer)) %>%
    summarise(
      across(-any_of(c("district", "year", "month")), ~ mean(.x, na.rm = TRUE) * 100),
      .by = year
    )
}

#' Generate a Heatmap of Discrepancies
#'
#' Creates a heatmap visualizing the proportion of discrepancies for each indicator over time.
#'
#' @param summary A `tibble` containing summarized discrepancy proportions by year.
#'
#' @return A `ggplot2` heatmap displaying discrepancy proportions for each indicator over time.
#'
#' @export
generate_heatmap <- function(summary) {
  summary %>%
    pivot_longer(cols = -year, names_to = 'vaccine', values_to = 'value') %>%
    drop_na() %>%
    ggplot(aes(x = year, y = vaccine, fill = value)) +
      geom_tile(color = "black", size = 0.5) +
      geom_text(aes(label = round(value, 1)), color = "black", size = 5) +
      scale_fill_gradient(low = "lightgreen", high = "red") +
      labs(title = "Proportion Discrepant Entries Per Year", x = "Year", y = "Vaccine", fill = "Proportion") +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
      )
}
