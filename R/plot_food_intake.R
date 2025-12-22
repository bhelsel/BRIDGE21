#
# df <- subjinfo
#
# "DSQfvl, DSQfvlnf, DSQfrt, DSQvlall, DSQvlnf, DSQdairy, DSQsug, DSQssb, DSQwhgr, DSQfib, DSQcalc"

# plot_food_intake <- function(dsqdata){
#   load(system.file("extdata/constants.RData", package = "ncidsq"))
#   norms <- ncidsq:::retrieve_norms(gender = ifelse(dsqdata$sex[1] == 1, "male", "female"))
#   ncidsqvars <- constants$fm_equation_coeff$variable[!grepl("_low|_high", constants$fm_equation_coeff$variable)]

#   if(!all(ncidsqvars %in% colnames(dsqdata))){
#     varsnotfound <- paste(ncidsqvars[!ncidsqvars %in% colnames(dsqdata)], collapse = "\n")
#     stop("Could not find these variables in the dataset:\n", varsnotfound)
#   }

#   intakedata <-
#     round(t(dsqdata[, ncidsqvars]), 2) %>%
#     as.data.frame() %>%
#     dplyr::mutate(variable = rownames(.), .before = 1) %>%
#     `rownames<-`(NULL) %>%
#     `colnames<-`(gsub("V", "intake", colnames(.))) %>%
#     dplyr::inner_join(x = norms, y = ., by = "variable") %>%
#     dplyr::relocate(unit, .before = 1) %>%
#     dplyr::arrange(unit) %>%
#     dplyr::select(-c(variable, unit))

#   custom_urls <- c("/Users/bhelsel/Desktop/banana.png", "/Users/bhelsel/Desktop/heart.png")

#   intakedata %>%
#     dplyr::mutate(url = c(rep(custom_urls, 4), custom_urls[1]), .after = 1) %>%
#     flextable::as_grouped_data("label") %>%
#     flextable::as_flextable(hide_grouplabel = TRUE) %>%
#     flextable::align(i = ~ is.na(label), part = "body", align = "center") %>%
#     flextable::align(i = 1, part = "header", align = "center") %>%
#     flextable::width(j = 1, width = 1.5) %>%
#     flextable::set_header_labels(values = c("", "NHANES", "Recommendations", "Intake")) %>%
#     flextable::mk_par(
#       j = "url", i = ~ !is.na(url),
#       value = flextable::as_paragraph(
#         flextable::as_image(url),#, width = 1, height = 1),
#         "\n",
#         flextable::as_i(label)
#       )
#     ) %>%
#     flextable::bg(i = seq(2, nrow(intakedata)*2, 4), bg = "#FDEE8C") %>%
#     flextable::bg(i = seq(4, nrow(intakedata)*2, 4), bg = "#FF7F7F") %>%
#     flextable::fontsize(i = ~ is.na(label), size = 18) %>%
#     flextable::fontsize(i = ~ !is.na(label), size = 24) %>%
#     flextable::fontsize(i = 1, part = "header", size = 18) %>%
#     flextable::width(j = 2, width = 2)

# }

