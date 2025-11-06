retrieve_accel_summary <- function(data) {
  accel_df <- cbind(
    filename = data$filename,
    date = data$calendar_date,
    dur_spt_min = data$dur_spt_min,
    data[, grep("total.*min", colnames(data))]
  )

  accel_df$dur_day_total_MVPA_min <- accel_df$dur_day_total_MOD_min +
    accel_df$dur_day_total_VIG_min

  accel_df$dur_day_total_MOD_min <- accel_df$dur_day_total_VIG_min <- NULL

  accel_df_per <-
    data.frame(
      filename = accel_df$filename,
      date = accel_df$date,
      apply(
        accel_df[, 3:6],
        2,
        FUN = function(x) x / rowSums(accel_df[, 3:6])
      ) *
        100
    )

  accel_df[, 3:6] <- round(accel_df[, 3:6])
  accel_df$date <- format(accel_df$date, "%m/%d/%Y")

  return(accel_df)
}

pet_amyloid_description <- function(
  centiloid,
  cognitive_decline = FALSE,
  petScanDate = NULL
) {
  if (centiloid >= 18 & cognitive_decline) {
    "Your amyloid is high and there are some changes in your memory and thinking."
  } else if (centiloid >= 18 & !cognitive_decline) {
    paste(
      "Your amyloid is high but you do not have changes in memory and thinking.",
      "You are at higher risk of developing memory changes.",
      "Regular exercise, a healthy diet, and keeping your brain busy are great ways to build your brain health."
    )
  } else if (centiloid < 18 & cognitive_decline) {
    glue::glue(
      "Your amyloid is not high but you have some changes in memory and thinking.",
      "Anytime you have changes in memory and thinking, it is important to talk to your doctor about what could be causing the changes.",
      "You can share with your doctor that you did not have high amyloid as of {petScanDate}.",
      "It is likely the cause is something other than Alzheimer\u0027s disease.",
      "Talk to your doctor about the things that can cause memory and thinking problems, including sleep apnea, depression, low levels of B12, or low levels of thyroid.",
      petScanDate = petScanDate,
      .sep = " "
    )
  } else {
    paste(
      "Your amyloid is not high and you do not have changes in memory thinking.",
      "This means brain changes common in Alzheimer\u0027s were not found at this time.",
      "It is possible amyloid is present in the brain but not at a high level.",
      "It is still possible to develop high amyloid or dementia in the future."
    )
  }
}


#' Retrieve Non-Missing Values from a Dataset
#'
#' @description
#' Extracts specified columns (and optionally an event column) from a dataset and drops any rows containing missing values.
#'
#' @param ... One or more unquoted column names to extract.
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
#' get_database_values(age, sex, data = mydata)
#' get_database_values(age, bmi, data = mydata, event_name = redcap_event_name)
#' }
#'
#' @export
#' @importFrom rlang ensym ensyms

get_database_values <- function(..., data, event_name = NULL) {
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
    }
    if (!(preferred %in% names(data)) && length(existing) > 0) {
      data <- dplyr::rename(data, !!preferred := !!rlang::sym(existing[1]))
    }
    renamed <- c(renamed, preferred)
  }

  indx <- which(apply(data[, renamed], 1, function(x) {
    !all(is.na(x))
  }))
  if (!inherits(event_name, "try-error")) {
    data[indx, c(event_name, renamed)]
  } else {
    data[indx, renamed]
  }
}


#' Concatenate Strings Using the `+` Operator
#'
#' @description
#' Provides syntactic sugar for string concatenation, allowing the use of `+` as a shorthand for `paste0()`.
#'
#' @param x An expression using the `+` operator for string concatenation.
#'
#' @return
#' A character string resulting from concatenating the provided components.
#'
#' @details
#' This function evaluates an expression in a temporary environment where `+` is redefined
#' as `paste0()`. It enables writing cleaner, more readable string concatenations.
#'
#' @examples
#' add_string("Hello, " + "world" + "!")
#' # Returns "Hello, world!"
#'
#' @export
#' @importFrom rlang env caller_env enexpr

add_string <- function(x) {
  e <- rlang::env(
    rlang::caller_env(),
    `+` = function(x, y) paste0(x, y)
  )
  eval(rlang::enexpr(x), e)
}

#' Format Output for Quarto Documents
#'
#' @description
#' Outputs formatted LaTeX-compatible elements or wrapped text for use in Quarto documents.
#'
#' @param x A character string to format or print.
#' @param type A character string specifying the output type. Options are:
#'   \itemize{
#'     \item `"section"` — Print as a LaTeX `\section{}`.
#'     \item `"subsection"` — Print as a LaTeX `\subsection{}`.
#'     \item `"newpage"` — Insert a LaTeX `\newpage` command.
#'     \item `"strwrap"` — Print wrapped text with a specified width.
#'   }
#' @param width The maximum line width for text wrapping (default = 100).
#'
#' @return
#' No return value. Called for its side effects of printing to the console or Quarto document.
#'
#' @examples
#' format_quarto("Participant Characteristics", type = "section")
#' format_quarto("Next Section", type = "newpage")
#' format_quarto("This is some long text that should be wrapped.", type = "strwrap", width = 60)
#'
#' @export
#' @importFrom glue glue

format_quarto <- function(x = NULL, type, width = 100) {
  if (type == "section") {
    cat(glue::glue("\\section{{{x}}}"), sep = "\n")
  } else if (type == "subsection") {
    cat(glue::glue("\\subsection{{{x}}}"), sep = "\n")
  } else if (type == "newpage") {
    cat("\\newpage")
  } else if (type == "input") {
    cat(glue::glue("\\input{{{x}}}"), sep = "\n")
  } else if (type == "strwrap") {
    cat(strwrap(x, width = width), fill = TRUE)
    cat("\n\n")
  }
}

#' Expand Reports with Sub-Reports
#'
#' Expands a named list of reports by adding their designated sub-reports.
#' Returns a named list containing all original reports (both TRUE and FALSE)
#' plus their expanded sub-reports set to TRUE for activated parent reports.
#'
#' @param reports Named list where names are report identifiers and values are
#'   logical (TRUE/FALSE) indicating whether the report is activated.
#' @param sub_reports Named list where each name is a parent report and each value
#'   is a list/vector of sub-report suffixes. Default is `.subReports` if available
#'   in the parent environment.
#'
#' @return Named list where names are report identifiers and values are logical.
#'   Includes all original reports with their original TRUE/FALSE values, plus
#'   expanded sub-reports set to TRUE if their parent report is TRUE, or FALSE
#'   if their parent report is FALSE. Reports without sub-reports are returned
#'   unchanged.
#'
#' @examples
#' sub_reports <- list(
#'   kuadrc_blood = c("karyotype", "ptau217"),
#'   kuadrc_lifestyle = c("blood_pressure", "bmi", "dxa")
#' )
#'
#' reports <- list(
#'   kuadrc_blood = TRUE,
#'   other_report = FALSE,
#'   kuadrc_lifestyle = FALSE
#' )
#' expand_reports(reports, sub_reports)
#' # Returns: list(
#' #   kuadrc_blood = TRUE,
#' #   kuadrc_karyotype = TRUE,
#' #   kuadrc_ptau217 = TRUE,
#' #   other_report = FALSE,
#' #   kuadrc_lifestyle = FALSE,
#' #   kuadrc_blood_pressure = FALSE,
#' #   kuadrc_bmi = FALSE,
#' #   kuadrc_dxa = FALSE
#' # )
#'
#' @export

expand_reports <- function(reports, sub_reports = .subReports) {
  # Start with original reports
  result <- reports

  # Expand reports with sub-reports
  for (r in names(reports)) {
    if (r %in% names(sub_reports)) {
      prefix <- sub("_.*", "", r)
      sub_report_names <- paste0(prefix, "_", unlist(sub_reports[[r]]))

      # Add sub-reports with same TRUE/FALSE value as parent
      for (sub_name in sub_report_names) {
        result[[sub_name]] <- reports[[r]]
      }
    }
  }

  result
}


#' Calculate Body Mass Index (BMI)
#'
#' @description
#' Calculates BMI from weight and height columns in a data frame. Handles both
#' metric and imperial units with automatic conversion to metric for calculation.
#'
#' @param data A data frame containing weight and height columns
#' @param weight Column name for weight (unquoted). Should be in kg for metric
#'   or pounds for imperial units
#' @param height Column name for height (unquoted). Should be in cm for metric
#'   or inches for imperial units
#' @param units Character string specifying the unit system. Either "metric"
#'   (default) or "imperial"
#'
#' @return The input data frame with an additional `bmi` column containing
#'   calculated BMI values
#'
#' @details
#' BMI is calculated using the formula: weight (kg) / (height (m))^2
#'
#' For imperial units, the function converts:
#' - Weight: pounds to kg (divide by 2.205)
#' - Height: inches to cm (multiply by 2.54)
#'
#' @examples
#' # Metric units (kg and cm)
#' df_metric <- data.frame(weight_kg = c(70, 80), height_cm = c(170, 180))
#' calculate_bmi(df_metric, weight_kg, height_cm, units = "metric")
#'
#' # Imperial units (pounds and inches)
#' df_imperial <- data.frame(weight_lbs = c(154, 176), height_in = c(67, 71))
#' calculate_bmi(df_imperial, weight_lbs, height_in, units = "imperial")
#'
#' @export

calculate_bmi <- function(
  data,
  weight,
  height,
  units = c("metric", "imperial")
) {
  units <- match.arg(units)
  expr <- rlang::expr(!!rlang::enquo(weight) / (!!rlang::enquo(height) / 100)^2)
  wt <- rlang::ensym(weight)
  ht <- rlang::ensym(height)

  if (units == "imperial") {
    data[[wt]] <- data[[wt]] / 2.205
    data[[ht]] <- data[[ht]] * 2.54
  }

  data$bmi <- rlang::eval_tidy(expr, data = data)
  return(data)
}

#' Format Date Columns
#'
#' @description
#' Formats date information into a standardized "Date" column with mm/dd/yyyy
#' format. Handles single date columns or composite dates built from separate
#' month, day, and year columns using tidy evaluation.
#'
#' @param data A data frame containing date information
#' @param date Date specification (unquoted). Can be:
#'   - A single column name (e.g., `visitdt`)
#'   - Composite date using `/` operator (e.g., `visitmo/visitday/visityr`)
#'   - A `paste()` expression (e.g., `paste(visitmo, visitday, visityr, sep = "/")`)
#' @param desc Display the date in descending order
#' @param display Customize how the dates are displayed, Default: %m/%d/%Y
#'
#' @return The input data frame with a "Date" column formatted as "mm/dd/yyyy".
#'   For single column input, the original column is renamed to "Date". For
#'   composite dates, the "Date" column is added while preserving original columns.
#'
#' @details
#' The function uses rlang metaprogramming to accept unquoted column names.
#'
#' For composite dates using the `/` operator, the function:
#' 1. Intercepts the expression before numeric division occurs
#' 2. Converts it to a paste operation to create date strings
#' 3. Parses the strings as dates using the "%m/%d/%Y" format
#'
#' For single date columns, the function attempts to parse using `as.Date()`
#' which handles common date formats automatically.
#'
#' @examples
#' # Single date column
#' df1 <- data.frame(visitdt = c("2024-01-15", "2024-02-20"))
#' format_date(df1, visitdt)
#'
#' # Composite date from separate columns using / operator
#' df2 <- data.frame(
#'   visitmo = c(1, 2),
#'   visitday = c(15, 20),
#'   visityr = c(2024, 2024)
#' )
#' format_date(df2, visitmo/visitday/visityr)
#'
#' # Composite date using paste()
#' format_date(df2, paste(visitmo, visitday, visityr, sep = "/"))
#'
#' @export

format_date <- function(data, date, desc = FALSE, display = "%m/%d/%Y") {
  # Capture the date expression without evaluating it

  date <- rlang::enexpr(date)

  # Check if it's a composite expression (e.g., visitmo/visitday/visityr)
  if (rlang::is_call(date, "/")) {
    extract_parts <- function(expr) {
      if (rlang::is_call(expr, "/")) {
        c(extract_parts(expr[[2]]), extract_parts(expr[[3]]))
      } else {
        list(expr)
      }
    }

    parts <- extract_parts(date)
    paste_expr <- rlang::expr(paste(!!!parts, sep = "/"))

    # Evaluate the expression to create the date string
    data <- data %>%
      dplyr::mutate(Date = !!paste_expr, .before = 1) %>%
      dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
      dplyr::select(-c(!!!parts))
  } else if (rlang::is_call(date, "paste")) {
    data <- data %>%
      dplyr::mutate(Date = !!date, .before = 1) %>%
      dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
  } else {
    # Single column case - rename and format
    col_name <- rlang::as_string(date)

    data <- data %>%
      dplyr::rename(Date = !!date) %>%
      dplyr::relocate(Date, .before = 1) %>%
      dplyr::mutate(Date = as.Date(Date))
  }

  # Arrange in ascending or descending order
  if (desc) {
    data <- dplyr::arrange(data, dplyr::desc(Date))
  } else {
    data <- dplyr::arrange(data, Date)
  }

  # Display as character in customized format (default: month/day/year)
  data <- data %>%
    dplyr::mutate(Date = format(Date, display))

  return(data)
}


#' Check if data contains only missing or placeholder values
#'
#' Determines whether all non-date columns in a dataset contain only missing
#' values (NA) or specified placeholder values (e.g., 0, -999). Returns TRUE
#' if no valid data exists, FALSE if any valid data is present.
#'
#' @param data A data frame to check for missing values
#' @param date Character vector of column name(s) to exclude from the check
#'   (typically date columns). Can be a single string or vector of strings.
#' @param missing_values Numeric vector of values to treat as missing data.
#'   Default is `c(0, -999)`. NA values are always considered missing.
#'
#' @return Logical. Returns `TRUE` if all non-date columns contain only NA
#'   or values in `missing_values`. Returns `FALSE` if any valid data exists.
#'   Also returns `TRUE` for empty datasets or datasets with no data columns.
#'
#' @examples
#' # Example with all missing data
#' df1 <- data.frame(
#'   visitdt = c("2024-01-01", "2024-01-02"),
#'   value1 = c(0, -999),
#'   value2 = c(NA, 0)
#' )
#' check_missing_values(df1, "visitdt")  # TRUE
#'
#' # Example with valid data
#' df2 <- data.frame(
#'   visitdt = c("2024-01-01", "2024-01-02"),
#'   value1 = c(0, 5)
#' )
#' check_missing_values(df2, "visitdt")  # FALSE
#'
#' # Example with multiple date columns
#' df3 <- data.frame(
#'   visitmo = c(1, 2),
#'   visitday = c(15, 20),
#'   visityr = c(2024, 2024),
#'   value = c(NA, NA)
#' )
#' check_missing_values(df3, c("visitmo", "visitday", "visityr"))  # TRUE
#'
#' @export

check_missing_values <- function(data, date, missing_values = c(0, -999)) {
  data_cols <- setdiff(colnames(data), date)

  if (length(data_cols) == 0 || nrow(data) == 0) {
    return(TRUE)
  }

  data_subset <- data[data_cols]

  all_missing <- all(
    apply(data_subset, 2, function(col) {
      all(is.na(col) | col %in% missing_values)
    })
  )

  return(all_missing)
}

#' Interpret percent change in a continuous score
#'
#' Computes and interprets the percent change of a numeric measure relative to
#' its first (baseline) value. This function provides a short, readable message
#' describing whether the measure increased, decreased, or stayed about the same
#' based on customizable thresholds.
#'
#' @param x A numeric vector of scores, ordered by time or visit, where the
#'   first element represents the baseline value.
#' @param type A character string specifying the name of the score or measure
#'   (e.g., `"fruit and vegetable"`, `"sleep"`, `"activity"`). This term is
#'   inserted dynamically into the interpretation message.
#' @param thresholds A numeric vector of two values indicating the lower and
#'   upper bounds (in percent) for what is considered "about the same".
#'   Defaults to `c(-5, 5)`, meaning less than –5% is a decrease, between –5%
#'   and +5% is the same, and greater than +5% is an increase.
#'
#' @details
#' The function treats the first value of `x` as the baseline score.
#' Percent change is computed as:
#' \deqn{((x - baseline) / baseline) * 100}
#'
#' Change categories are determined using \code{\link[base]{cut}} with
#' boundaries \code{c(-Inf, thresholds, Inf)}. The first observation is always
#' labeled `"baseline"`.
#'
#' The returned messages include:
#' \itemize{
#'   \item \strong{"Baseline measurement"}
#'   \item \strong{"Nice job! You increased your [type] score by X%"}
#'   \item \strong{"Your [type] score decreased by X%"}
#'   \item \strong{"Your [type] score stayed about the same"}
#' }
#'
#' @return A character vector of the same length as `x`, with a human-readable
#'   interpretation for each value.
#'
#' @examples
#' # Default thresholds
#' interpret_percent_change(c(400, 420, 395, 410), type = "fruit and vegetable")
#'
#' # Custom thresholds for a stricter definition of "same"
#' interpret_percent_change(c(100, 104, 110), type = "activity", thresholds = c(-3, 3))
#'
#' @seealso [cut()], [dplyr::case_match()]
#'
#' @export

interpret_percent_change <- function(x, type, thresholds = c(-5, 5)) {
  if (length(x) == 1) {
    return("Baseline measurement")
  }

  baseline <- x[1]
  pct_change <- ((x - baseline) / baseline) * 100

  change_type <- as.character(
    cut(
      pct_change,
      breaks = c(-Inf, thresholds, Inf),
      labels = c("decrease", "same", "increase"),
      right = TRUE
    )
  )

  change_type[1] <- "baseline"

  base_message <- list(
    increase = sprintf("Nice job! You increased your %s score by", type),
    decrease = sprintf("Your %s score decreased by", type),
    same = sprintf("Your %s score stayed about the same", type)
  )

  dplyr::case_match(
    change_type,
    "baseline" ~ "Baseline measurement",
    "increase" ~ sprintf("%s %.1f%%", base_message$increase, abs(pct_change)),
    "decrease" ~ sprintf("%s %.1f%%", base_message$decrease, abs(pct_change)),
    "same" ~ base_message$same
  )
}
