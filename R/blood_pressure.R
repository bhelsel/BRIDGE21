#' @title create_blood_pressure_table
#' @description Creates a table containing systolic and diastolic blood pressure
#'     values and their respective risk categories.
#' @return A table containing blood pressure and risk categories
#' @details Creates a table containing systolic and diastolic blood pressure
#'     values and their respective risk categories.
#' @rdname create_blood_pressure_table
#' @keywords internal
#' @noRd

create_blood_pressure_table <- function() {
  data.frame(
    systolic = c(90, 120, 130, 180),
    diastolic = c(60, 80, 90, 180),
    categories = c("Low BP", "Normal", "Elevated", "High"),
    colors = c("#ADD8E6", "#90EE90", "#FDEE8C", "#FF7F7F")
  )
}

#' @title classify_blood_pressure
#' @description Classifies systolic or diastolic blood pressure and returns the
#'     level of associated risk.
#' @return A risk category based on the value and type of blood pressure measure.
#' @details Classifies systolic or diastolic blood pressure and returns the
#'     level of associated risk.
#' @rdname classify_blood_pressure
#' @keywords internal
#' @noRd

classify_blood_pressure <- function(value, type = c("systolic", "diastolic")) {
  type <- match.arg(type)
  bptab <- create_blood_pressure_table()
  cut(
    value,
    right = FALSE,
    breaks = c(-Inf, bptab[[type]][1:3], Inf),
    labels = bptab$categories
  )
}

#' @title round_rectangle
#' @description A convenience function to plot the rounded rectangles in plot_blood_pressure
#' @inheritParams grid::roundrectGrob
#' @param fill Fill color of the rectangle
#' @return An annotation_custom ggplot2 layer
#' @details A convenience function to plot the rounded rectangles in plot_blood_pressure
#' @seealso
#'  \code{\link[ggplot2]{annotation_custom}}
#'  \code{\link[grid]{roundrect}}, \code{\link[grid]{gpar}}
#' @rdname round_rectangle
#' @keywords internal
#' @noRd
#' @importFrom ggplot2 annotation_custom
#' @importFrom grid roundrectGrob gpar unit

round_rectangle <- function(x, y, width, height, radius, fill) {
  ggplot2::annotation_custom(
    grid::roundrectGrob(
      x = x,
      y = y,
      width = width,
      height = height,
      r = grid::unit(radius, "npc"), # Corner rounding
      gp = grid::gpar(fill = fill, col = NA) # Fill color with no border
    )
  )
}


#' @title plot_blood_pressure
#' @description Generate a plot containing the systolic and diastolic blood pressure values
#' @param bpsys Systolic Blood Pressure
#' @param bpdia Diastolic Blood Pressure
#' @return A ggplot2 object containing the plotted blood pressure values
#' @details Generate a plot containing the systolic and diastolic blood pressure values
#' @seealso
#'  \code{\link[magick]{editing}}
#'  \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{geom_raster}}, \code{\link[ggplot2]{aes}}, \code{\link[ggplot2]{annotation_custom}}, \code{\link[ggplot2]{annotate}}, \code{\link[ggplot2]{geom_label}}, \code{\link[ggplot2]{ggtheme}}
#'  \code{\link[grid]{grid.raster}}, \code{\link[grid]{patterns}}
#' @rdname plot_blood_pressure
#' @export
#' @importFrom magick image_read
#' @importFrom ggplot2 ggplot geom_rect aes annotation_custom annotate geom_label geom_text theme_void
#' @importFrom grid rasterGrob linearGradient
#' @importFrom purrr pmap
#' @importFrom rlang .data

plot_blood_pressure <- function(bpsys, bpdia) {
  bptab <- create_blood_pressure_table()

  img <- magick::image_read(
    system.file("images/blood_pressure.png", package = "BRIDGE21")
  )

  # Define Positional Constants ----

  # Add rounded rectangles
  gradient_rr <- grid::linearGradient(c("gray90", "black"))
  x_rr <- c(rep(0.75, 2), rep(0.67, 2), rep(0.84, 2))
  y_rr <- c(rep(0.5, 2), 0.18, 0.78, 0.78, 0.18)
  width_rr <- c(0.35, 0.32, rep(0.075, 4))
  height_rr <- c(0.97, 0.93, rep(0.2, 4))
  radius_rr <- c(rep(0.2, 2), rep(0.5, 4))
  # fmt: skip
  fill_rr <- c(list(gradient_rr), "white", bptab$colors[c(1, 4)], bptab$colors[c(4, 1)])

  # Add rectangles
  xmin_r <- c(rep(64.6, 4), rep(83.27, 3))
  xmax_r <- c(rep(72.8, 4), rep(91.5, 3))
  ymin_r <- c(60, bptab$systolic[1:3], bptab$diastolic[1:3])
  ymax_r <- c(bptab$systolic[1:4], bptab$diastolic[2:4])
  # fmt: skip
  colors_r <- c(bptab$colors[1:4], bptab$colors[2:4])

  # Add Annotations
  geom_a <- c(rep("text", 3), rep("segment", 4))
  x_a <- c(68.5, 87.5, 78, 64.6, 83.27, 74, 80)
  xend_a <- list(NULL, NULL, NULL, 72.8, 91.5, 76, 82)
  # fmt: skip
  y_a <- list(212, 212, seq(50, 190, 10), bpsys, bpdia, seq(50, 190, 5), seq(50, 190, 5))
  # fmt: skip
  yend_a <- list(NULL, NULL, NULL, bpsys, bpdia, seq(50, 190, 5), seq(50, 190, 5))
  # fmt: skip
  label_a <- list("SYS\n(mmHg)", "DIA\n(mmHg)", seq(50, 190, 10), NULL, NULL, NULL, NULL)
  linetype_a <- list(NULL, NULL, NULL, "dashed", "dashed", "solid", "solid")

  # Add labels
  x_l <- c(68.5, 87.5)
  y_l <- c(bpsys + 7, bpdia + 7)
  label_l <- c(bpsys, bpdia)

  # Convenience Function to Add Text and Segment Annotations

  add_annotations <- function(geom, x, xend, y, yend, label, linetype) {
    if (geom == "text") {
      ggplot2::annotate("text", x = x, y = y, label = label, fontface = "bold")
    } else {
      # fmt: skip
      ggplot2::annotate("segment", x = x, xend = xend, y = y, yend = yend, linetype = linetype)
    }
  }

  # Plot ----

  base_plot <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = 0, xmax = 100, ymin = 30, ymax = 220),
      fill = "white"
    ) +
    ggplot2::annotation_custom(
      grid::rasterGrob(img, width = 0.5, height = 0.25, x = 0.3, y = 0.15)
    )

  plot_with_annotations <-
    base_plot +
    purrr::pmap(
      list(x_rr, y_rr, width_rr, height_rr, radius_rr, fill_rr),
      round_rectangle
    ) +
    purrr::pmap(
      list(xmin_r, xmax_r, ymin_r, ymax_r, colors_r),
      \(xmin, xmax, ymin, ymax, colors) {
        ggplot2::geom_rect(
          ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          fill = colors
        )
      }
    ) +
    purrr::pmap(
      list(geom_a, x_a, xend_a, y_a, yend_a, label_a, linetype_a),
      add_annotations
    ) +
    purrr::pmap(
      list(x_l, y_l, label_l),
      \(x, y, label) {
        ggplot2::geom_label(
          ggplot2::aes(x = x, y = y),
          label = label,
          fontface = "bold",
          fill = "white"
        )
      }
    )

  bp_plot <- plot_with_annotations +
    # Add legend
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = rep(3, 4),
        xmax = rep(5, 4),
        ymin = seq(90, 120, 10),
        ymax = seq(94, 129, 10)
      ),
      fill = bptab$colors
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = rep(7, 4), y = seq(90, 120, 10)),
      label = bptab$categories,
      hjust = 0,
      vjust = 0,
      size = 4
    ) +
    ggplot2::theme_void()

  return(bp_plot)
}
