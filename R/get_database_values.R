#' Retrieve Non-Missing Values from a Dataset
#'
#' @description
#' Extracts specified columns (and optionally an event column) from a dataset and drops any rows containing missing values.
#'
#' @param ... One or more unquoted column names to extract.
#' @param date A character vector consisting of the columns that are used to idenfity the dates in the data set
#' @param data A data frame or tibble containing the variables of interest.
#' @param event_name (Optional) A variable name representing an event column, used to subset data along with the specified variables.
#'
#' @return
#' A data frame containing the specified columns with all rows containing `NA` values removed.
#'
#' @details
#' This function is designed to simplify the process of subsetting and cleaning datasets, particularly for event-based data
#' (e.g., longitudinal REDCap exports). If `event_name` is supplied, the function ensures the event column is included and
#' removes rows with missing data across all selected variables. If not, only the specified variables are included.
#'
#' @examples
#' \dontrun{
#' get_database_values(age, sex, date = date, data = mydata)
#' get_database_values(age, bmi, data = mydata, event_name = redcap_event_name)
#' }
#'
#' @export
#' @importFrom rlang ensym ensyms

get_database_values <- function(..., date = NULL, data, event_name = NULL) {
  event_name <- try(as.character(rlang::ensym(event_name)), silent = TRUE)
  variables <- rlang::ensyms(...)
  variables <- gsub("`", "", as.character(variables))
  renamed <- character() # will store all preferred names

  for (v in variables) {
    opts <- unlist(strsplit(v, "\\|"))
    preferred <- opts[1]
    existing <- intersect(opts, names(data))

    if (length(existing) == 0) {
      next
    } else if (length(existing) >= 2) {
      dplyr::mutate(
        data,
        !!preferred := dplyr::coalesce(!!!rlang::syms(existing))
      )
    } else if (!(preferred %in% names(data)) && length(existing) == 1) {
      data <- dplyr::rename(data, !!preferred := !!rlang::sym(existing[1]))
    }
    renamed <- c(renamed, preferred)
  }

  na_vals <- c(888, -999)

  # Filter out rows where all renamed variables are missing
  indx <- which(apply(data[, renamed], 1, function(x) {
    !all(is.na(x) | x %in% na_vals)
  }))

  if (!is.null(date)) {
    renamed <- c(date, renamed)
  }

  if (!inherits(event_name, "try-error")) {
    data[indx, c(event_name, renamed)]
  } else {
    data[indx, renamed]
  }
}
