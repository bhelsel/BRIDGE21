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
