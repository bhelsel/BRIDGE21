# Copyright © 2022 University of Kansas. All rights reserved.

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "ptid",
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
    "apoe,"
    "demographics",
    "cognition",
    "lifetsyle"
  )
)

.subReports <- list(
  kuadrc_blood = c("karyotype", "ptau217"),
  kuadrc_lifestyle = c("activity", "blood_pressure", "bmi", "dxa"),
  bold_lifestyle = c("activity", "bmi", "blood_pressure", "dxa"),
  kuadrc_cognition = c("dsmse", "mcrt", "ntgedsd"),
  bold_cognition = c("dsmse", "mcrt", "ntgedsd", "vineland")
)
