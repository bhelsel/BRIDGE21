retrieve_accel_summary <- function(data) {
  accel_df <- cbind(
    date = data$calendar_date,
    dur_spt_min = data$dur_spt_min,
    data[, grep("total.*min", colnames(data))]
  )

  accel_df$dur_day_total_MVPA_min <- accel_df$dur_day_total_MOD_min +
    accel_df$dur_day_total_VIG_min

  accel_df$dur_day_total_MOD_min <- accel_df$dur_day_total_VIG_min <- NULL

  accel_df_per <-
    data.frame(
      date = accel_df$date,
      apply(
        accel_df[, 2:5],
        2,
        FUN = function(x) x / rowSums(accel_df[, 2:5])
      ) *
        100
    )

  accel_df[, 2:5] <- round(accel_df[, 2:5])
  accel_df$date <- format(accel_df$date, "%m/%d/%Y")

  return(accel_df)
}
