#' Build a formatted flextable
#'
#' A wrapper around [flextable::flextable()] that applies the common
#' formatting used in reports (column widths, alignment, bolding, extra header
#' rows, and footers) in a single, reproducible call. It returns a regular
#' flextable, so additional flextable calls can be piped onto the result.
#'
#' @param data A data frame or tibble to display.
#' @param col_labels Optional header labels that replace the column names
#'   shown in the table without changing `data`. Either a named character
#'   vector (`c(old_name = "New label")`), which can relabel a subset of
#'   columns, or an unnamed character vector with one label per column.
#'   Defaults to `NULL` (use the column names).
#' @param header_rows Optional extra header rows placed above the column
#'   labels, such as titles or spanning headers. Either a character vector
#'   (one full-width row per element) or a list of specifications. Each
#'   specification is a list with:
#'   * `text`: character vector of cell values (required)
#'   * `colwidths`: integer vector giving how many columns each value spans.
#'     Must have the same length as `text` and sum to `ncol(data)`. Defaults
#'     to one full-width cell when `text` has length 1.
#'   * `bold`: logical, default `TRUE`
#'   * `italic`: logical, default `FALSE`
#'   * `align`: `"left"`, `"center"`, or `"right"`, default `"center"`
#'
#'   Rows appear top to bottom in the order supplied, above the column labels.
#' @param widths Column widths in inches. `NULL` (default) autofits the table.
#'   A single value is recycled to every column; otherwise supply one value per
#'   column.
#' @param align Alignment of the body cells: `"left"`, `"center"`, `"right"`,
#'   or `"justify"`. A single value is recycled; otherwise supply one per
#'   column. Defaults to `"center"`.
#' @param header_align Alignment of the column labels, with the same rules as
#'   `align`. Defaults to the value of `align`.
#' @param header Logical. If `FALSE`, the entire header part is removed,
#'   **including** any rows added through `header_rows`. Defaults to `TRUE`.
#' @param bold_header Logical. Bold the column label row(s). Defaults to `TRUE`.
#' @param bold_cols Columns whose body cells should be bold, given as column
#'   names or indices. Defaults to `NULL` (none).
#' @param bold_rows Integer row indices of the body to bold. Defaults to
#'   `NULL` (none).
#' @param footers Optional footer rows, each spanning the full table width.
#'   Either a character vector (one footer per element) or a list of
#'   specifications. Each specification is a list with:
#'   * `text`: the footer text (required)
#'   * `bold`: logical, default `FALSE`
#'   * `italic`: logical, default `FALSE`
#'   * `align`: `"left"`, `"center"`, or `"right"`, default `"left"`
#'
#'   Footers appear in the order supplied. Defaults to `NULL` (no footer).
#' @param fontsize Font size in points applied to all parts of the table
#'   (header, body, footer). Use `NULL` to keep the flextable default.
#'   Defaults to `11`.
#' @param borders Logical. If `FALSE`, all borders are removed. Defaults to
#'   `TRUE`.
#'
#' @details
#' Formatting is applied in this order: column labels, borders, font size,
#' alignment, bolding, widths, extra header rows, footers, and finally header
#' removal (when `header = FALSE`). Because widths are set with
#' [flextable::width()], they are only honored for fixed-layout tables, which
#' is why `widths` and autofit are mutually exclusive. If widths do not take
#' effect in a particular output format, add
#' `flextable::set_table_properties(layout = "fixed")` to the result.
#'
#' Arguments that accept "one value or one per column" raise an informative
#' error when the length does not match `ncol(data)`.
#'
#' @return A [flextable::flextable()] object.
#'
#' @seealso [flextable::flextable()], [flextable::set_header_labels()],
#'   [flextable::add_header_row()], [flextable::add_footer_row()]
#'
#' @examples
#' df <- data.frame(
#'   id = c("A01", "A02"),
#'   sleep = c(452, 478),
#'   mvpa = c(31, 27)
#' )
#'
#' # Basic: custom widths and a single footer
#' flex_table(df, widths = c(1.5, 1.2, 1.2), footers = "Average minutes per day")
#'
#' # Relabel columns and add a spanning header
#' flex_table(
#'   df,
#'   col_labels = c(id = "Participant", sleep = "Sleep", mvpa = "MVPA"),
#'   header_rows = list(text = c("", "Minutes per Day"), colwidths = c(1, 2))
#' )
#'
#' # Per-column alignment, bold first column, styled footers
#' flex_table(
#'   df,
#'   align = c("left", "center", "center"),
#'   bold_cols = 1,
#'   footers = list(
#'     list(text = "MVPA: Moderate-to-Vigorous Physical Activity", italic = TRUE),
#'     list(text = "Source: ABC-DS", align = "right")
#'   )
#' )
#'
#' @export

flex_table <- function(
  data,
  col_labels = NULL,
  header_rows = NULL,
  widths = NULL, # NULL = autofit; one value = recycled; or one per column
  align = "center", # one value or one per column
  header_align = align,
  header = TRUE, # FALSE drops the header row
  bold_header = TRUE,
  bold_cols = NULL, # column names or indices
  bold_rows = NULL, # row indices
  footers = NULL, # character vector, or list of lists (see below)
  fontsize = 11,
  borders = TRUE
) {
  ncols <- ncol(data)

  recycle <- function(x, arg) {
    if (length(x) == 1) {
      return(rep(x, ncols))
    }
    if (length(x) != ncols) {
      stop("`", arg, "` must have length 1 or ", ncols, " (one per column).")
    }
    x
  }

  align <- recycle(align, "align")
  header_align <- recycle(header_align, "header_align")

  ft <- flextable::flextable(data)

  # Header labels
  if (!is.null(col_labels)) {
    if (is.null(names(col_labels))) {
      if (length(col_labels) != ncols) {
        stop(
          "Unnamed `col_labels` must have length ",
          ncols,
          " (one per column)."
        )
      }
      col_labels <- stats::setNames(col_labels, names(data))
    }

    unknown <- setdiff(names(col_labels), names(data))
    if (length(unknown) > 0) {
      stop(
        "`col_labels` names not found in data: ",
        paste(unknown, collapse = ", ")
      )
    }

    ft <- flextable::set_header_labels(ft, values = as.list(col_labels))
  }

  if (!borders) {
    ft <- flextable::border_remove(ft)
  }

  if (!is.null(fontsize)) {
    ft <- flextable::fontsize(ft, size = fontsize, part = "all")
  }

  # Per-column alignment
  for (j in seq_len(ncols)) {
    ft <- ft |>
      flextable::align(j = j, align = align[j], part = "body") |>
      flextable::align(j = j, align = header_align[j], part = "header")
  }

  # Bold
  if (bold_header) {
    ft <- flextable::bold(ft, part = "header")
  }
  if (!is.null(bold_cols)) {
    ft <- flextable::bold(ft, j = bold_cols, part = "body")
  }
  if (!is.null(bold_rows)) {
    ft <- flextable::bold(ft, i = bold_rows, part = "body")
  }

  # Widths (inches)
  if (is.null(widths)) {
    ft <- flextable::autofit(ft)
  } else {
    ft <- flextable::width(
      ft,
      j = seq_len(ncols),
      width = recycle(widths, "widths")
    )
  }

  if (!is.null(header_rows)) {
    # character vector -> one full-width row per element
    if (is.character(header_rows)) {
      header_rows <- lapply(header_rows, function(x) list(text = x))
    }
    # single spec like list(text = ..., colwidths = ...)
    if (!is.null(header_rows$text)) {
      header_rows <- list(header_rows)
    }

    header_rows <- lapply(header_rows, function(h) {
      if (is.character(h)) {
        h <- list(text = h)
      }
      h <- utils::modifyList(
        list(bold = TRUE, italic = FALSE, align = "center"),
        h
      )
      if (is.null(h$colwidths)) {
        h$colwidths <- if (length(h$text) == 1) {
          ncols
        } else {
          rep(1, length(h$text))
        }
      }
      if (length(h$text) != length(h$colwidths) || sum(h$colwidths) != ncols) {
        stop(
          "Header `colwidths` must match `text` in length and sum to ",
          ncols,
          "."
        )
      }
      h
    })

    # Each new row goes on top, so add in reverse to keep the order you passed
    for (h in rev(header_rows)) {
      ft <- ft |>
        flextable::add_header_row(
          values = h$text,
          colwidths = h$colwidths,
          top = TRUE
        ) |>
        flextable::align(i = 1, align = h$align, part = "header") |>
        flextable::bold(i = 1, bold = h$bold, part = "header") |>
        flextable::italic(i = 1, italic = h$italic, part = "header")
    }
  }

  # Footers: each one spans the full table width
  if (!is.null(footers)) {
    if (is.list(footers) && !is.null(footers$text)) {
      footers <- list(footers)
    } # single spec
    footers <- lapply(footers, function(f) {
      if (is.character(f)) {
        f <- list(text = f)
      }
      utils::modifyList(
        list(bold = FALSE, italic = FALSE, align = "left"),
        f
      )
    })

    for (k in seq_along(footers)) {
      f <- footers[[k]]
      ft <- ft |>
        flextable::add_footer_row(
          values = f$text,
          colwidths = ncols,
          top = FALSE
        ) |>
        flextable::align(i = k, align = f$align, part = "footer") |>
        flextable::bold(i = k, bold = f$bold, part = "footer") |>
        flextable::italic(i = k, italic = f$italic, part = "footer")
    }
  }

  if (!header) {
    ft <- flextable::delete_part(ft, part = "header")
  }

  ft
}
