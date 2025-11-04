# Copyright © 2022 University of Kansas. All rights reserved.

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "ptid",
    "record_id",
    "Date",
    "ENMO",
    "category",
    "timestamp",
    "label",
    "coenrol_studyid"
  ))
}

.allReports <- list(
  kuadrc = c(
    "apoe",
    "blood",
    "cognition",
    "demographics",
    "lifestyle",
    "mri",
    "pet"
  ),
  bold = c(
    "apoe",
    "blood",
    "demographics",
    "cognition",
    "lifestyle"
  )
)

.subReports <- list(
  kuadrc_blood = c("karyotype", "ptau217"),
  bold_blood = c("karyotype", "ptau217", "metabolism"),
  kuadrc_lifestyle = c("activity", "blood_pressure", "bmi", "dxa"),
  bold_lifestyle = c("activity", "blood_pressure", "bmi", "dxa", "veggiemeter"),
  kuadrc_cognition = c("dsmse", "mcrt", "ntgedsd"),
  bold_cognition = c("dsmse", "mcrt", "ntgedsd", "vineland")
)
