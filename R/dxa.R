#' @title plot_body_fat
#' @description Generates a bar plot of the participant's body fat percent compared to norms by age and sex.
#' @param total_fat Participant's body fat percent
#' @param subject_sex Participant's sex (1 = male, 2 = female)
#' @param subject_birthdate Date of birth (YYYY-MM-DD)
#' @param fittest_date The date of a DXA scan in a %m/%d/%Y format
#' @return A ggplot object
#' @export
#' @importFrom ggplot2 geom_segment aes geom_point annotate ggplot geom_rect geom_label geom_text scale_x_continuous scale_y_continuous labs coord_fixed theme element_text element_blank margin

plot_body_fat <- function(
  total_fat,
  subject_sex,
  subject_birthdate,
  fittest_date
) {
  # fmt: skip
  age <- as.numeric(as.Date(fittest_date, "%m/%d/%Y") - as.Date(subject_birthdate)) %/% 365.25

  norms <- data.frame(
    sex = rep(c(1, 2), each = 3),
    age_max = rep(c(39, 59, Inf), 2),
    low = c(8, 11, 13, 21, 23, 24),
    mid = c(20, 22, 25, 33, 34, 36),
    high = c(24, 27, 30, 39, 40, 42)
  )

  cutoffs <- norms[norms$sex == subject_sex & age <= norms$age_max, ]
  cutoffs <- as.numeric(cutoffs[1, c("low", "mid", "high")])

  ticks <- seq(5, 65, by = 5)

  ggplot2::ggplot() +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = c(0, cutoffs[1], cutoffs[2]),
        xmax = c(cutoffs[1], cutoffs[2], 70),
        ymin = c(0.7, 0.25, 0.7),
        ymax = rep(0.95, 3)
      ),
      fill = c("#44c1de", "#5cbba2", "#094c92"),
      alpha = 0.25
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = total_fat, y = 0.4),
      color = "black",
      linewidth = 1.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = total_fat, y = 0.4),
      size = 4
    ) +
    ggplot2::geom_label(
      ggplot2::aes(x = total_fat + 3, y = 0.4),
      label = paste0(total_fat, "%"),
      fill = "white",
      fontface = "bold",
      size = 4,
      hjust = 0,
      label.size = 0.25
    ) +
    ggplot2::annotate(
      geom = "text",
      x = c(cutoffs[1] / 2, mean(cutoffs[1:2]), cutoffs[2] + 5),
      y = rep(0.825, 3),
      label = c("Low", "Healthy", "High"),
      size = 4,
      fontface = "bold"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = ticks, y = 0.65, yend = 0.70),
      color = "black"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = 70, y = 0.7, yend = 0.7),
      color = "black",
      linewidth = 1
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = ticks, y = 0.55, label = ticks),
      size = 4,
      color = "black",
      fontface = "bold"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = cutoffs[1:2], y = 0.7, yend = 0.95),
      linewidth = 1
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 70), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0.25, 0.95), expand = c(0, 0)) +
    ggplot2::labs(y = "Body Fat %", x = NULL) +
    ggplot2::coord_fixed(ratio = 15) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(
        size = 14,
        color = "black",
        face = "bold",
        margin = ggplot2::margin(r = 10)
      ),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(color = "black", linewidth = 2),
      plot.background = ggplot2::element_blank()
    )
}
