#' @title generate_report
#' @description Generates a Quarto pdf report containing participant feedback from the study visit
#' @param id The patient id as represented in the study database
#' @param datafile The path name of the file containing the data as a .csv file
#' @param outputdir The path name of the directory containing the participant feedback reports
#' @param acceldir The path name of the directory containing the raw .gt3x accelerometer files, Default = NULL
#' @param ... A vector of unquoted or quoted reports to generate from .allReports
#' @param example_report Used with outputdir to generate a sample report using example data, Default = FALSE
#' @return The location of the participant feedback report
#' @details Generates a Quarto pdf report containing participant feedback from the study visit
#' @seealso
#'  \code{\link[readr]{read_delim}}
#'  \code{\link[kuadrc.xnat]{get_projects}}, \code{\link[kuadrc.xnat]{get_experiments}}, \code{\link[kuadrc.xnat]{get_scans}}, \code{\link[kuadrc.xnat]{xnat_download}}, \code{\link[kuadrc.xnat]{convert_to_nifti}}, \code{\link[kuadrc.xnat]{save_nifti_image}}
#'  \code{\link[yaml]{write_yaml}}
#'  \code{\link[quarto]{quarto_render}}
#' @rdname generate_report
#' @export
#' @importFrom readr read_csv
#' @importFrom yaml write_yaml
#' @importFrom quarto quarto_render
#' @importFrom cli cli_abort

generate_report <- function(
  ...,
  id,
  datafile,
  outputdir,
  acceldir = NULL,
  example_report = FALSE
) {
  if (outputdir == getwd()) {
    stop(
      "\nCurrent working directory is: ",
      getwd(),
      "\noutputdir needs to be different than your current working directory"
    )
  }

  reports <- rlang::ensyms(...)

  if (example_report) {
    reports <- reports[grepl("kuadrc", reports)]
    id = "test_jayhawk"
    datafile = system.file("extdata/example.csv", package = "BRIDGE21")
  }

  reports <- generate_report_params(reports)

  prefix <- unique(sub("_.*", "", names(reports[reports == TRUE])))

  logo_path <- switch(
    prefix,
    abcds = "_extensions/BRIDGE21/logos/abcds.png",
    bold = "_extensions/BRIDGE21/logos/bold.jpg",
    kuadrc = "_extensions/BRIDGE21/logos/kuadrc.png",
    trcds = "_extensions/BRIDGE21/logos/abcds.png"
  )

  logo_position <- switch(
    prefix,
    abcds = "24.2",
    bold = "23.2",
    kuadrc = "24.2",
    trcds = "24.2"
  )

  study_name <- switch(
    prefix,
    abcds = "Alzheimer\u0027s Biomarker Consortium \u2013 Down syndrome Study",
    bold = "Brain Outcomes and Lifestyle in Down syndrome Study",
    kuadrc = "KU ADRC Brain Health and Down Syndrome Program",
    trcds = "Trial Ready Cohort - Down syndrome Study"
  )

  # Create new directories
  if (!dir.exists(as.character(id))) {
    dir.create(as.character(id))
  }

  persondir <- file.path(outputdir, id)
  if (!dir.exists(persondir)) {
    dir.create(persondir)
  }

  if (prefix == "kuadrc") {
    data <- readr::read_csv(
      datafile,
      show_col_types = FALSE
    )
    if (!id %in% unique(data$ptid)) {
      stop(sprintf("We did not find patient ID %s in the dataset", id))
    }
    data <- data[data$ptid == id, ]
    readr::write_csv(data, file = sprintf("%s/data.csv", id))
  }

  if (prefix == "bold") {
    data <- readr::read_csv(
      datafile,
      show_col_types = FALSE
    )
    if (!id %in% unique(data$id)) {
      stop(sprintf("We did not find patient ID %s in the dataset", id))
    }
    identifier <- data[which(data$id == id), "record_id", drop = TRUE]
    data <- data[data$record_id == identifier, ]
    readr::write_csv(data, file = sprintf("%s/data.csv", id))
  }

  if (prefix == "abcds" | prefix == "trcds") {
    data <- readr::read_csv(datafile, show_col_types = FALSE)
    if (!id %in% unique(data$subject_label)) {
      stop(sprintf("We did not find patient ID %s in the dataset", id))
    }
    data <- data[data$subject_label == id, ]
    readr::write_csv(data, file = sprintf("%s/data.csv", id))
  }

  if (example_report) {
    mriFiles <- system.file(
      c("images/axialImage.png", "images/sagittalImage.png"),
      package = "BRIDGE21"
    )

    imagedir <- file.path(persondir, "imaging")
    if (!dir.exists(imagedir)) {
      dir.create(imagedir)
    }

    invisible(file.copy(
      from = mriFiles,
      to = file.path(imagedir, basename(mriFiles))
    ))
  } else if (prefix == "kuadrc") {
    imagedir <- file.path(persondir, "imaging")
    if (!dir.exists(imagedir)) {
      dir.create(imagedir)
    }

    invisible(save_xnat_images(imagedir, data, id))
  } else {
    imagedir <- NULL
  }

  # Process accelerometer data
  if (example_report) {
    accelFolder <- system.file("extdata/accelerometer", package = "BRIDGE21")
    file.copy(
      from = accelFolder,
      to = persondir,
      recursive = TRUE
    )
    accelres <- file.path(outputdir, id, "accelerometer", "results")
  } else if (!is.null(acceldir)) {
    accelres <- run_ggir(id = id, acceldir = acceldir, outputdir = outputdir)
  } else {
    accelres <- NULL
  }

  qmdfolder <- system.file("qmd", package = "BRIDGE21")

  invisible(file.copy(
    from = list.files(qmdfolder, full.names = TRUE),
    to = as.character(id),
    recursive = TRUE,
    overwrite = TRUE
  ))

  # Saves a yaml file to pass the id and directory information to the quarto document

  yaml::write_yaml(
    c(
      list(
        id = id,
        outputdir = outputdir,
        imagedir = imagedir,
        accelres = accelres
      ),
      reports
    ),
    file = sprintf("%s/_variables.yaml", id)
  )

  # Name the report using the participant id
  pdffile <- sprintf("%s_Report.pdf", id)
  # Render the quarto document
  quarto::quarto_render(
    input = sprintf("%s/BRIDGE21.qmd", id),
    output_file = pdffile,
    metadata = list(
      logo = logo_path,
      position = logo_position,
      study = study_name
    )
  )

  # Adjust pdffile location if it is added to the id folder
  if (!file.exists(pdffile)) {
    pdffile <- file.path(getwd(), id, pdffile)
  }

  invisible(file.copy(
    from = pdffile,
    to = file.path(persondir, basename(pdffile)),
    overwrite = TRUE
  ))

  # Remove pdf file after it is copied to the output directory
  invisible(file.remove(pdffile))
  # Unlink the temporary folder to remove it
  invisible(unlink(id, recursive = TRUE))
  return(file.path(persondir, basename(pdffile)))
}


generate_report_params <- function(reports) {
  # Extract prefixes (everything before the first underscore)
  prefixes <- sub("_.*", "", reports)

  # Check if there are multiple unique prefixes
  unique_prefixes <- unique(prefixes)

  if (length(unique_prefixes) > 1) {
    cli::cli_abort(c(
      "Reports must be from the same study.",
      "x" = "You requested reports from {length(unique_prefixes)} different studies: {.val {unique_prefixes}}.",
      "i" = "Please provide reports from only one study at a time."
    ))
  }

  allReports <- unname(unlist(purrr::imap(.allReports, ~ paste0(.y, "_", .x))))

  if (is.null(reports)) {
    reports <- allReports
  } else if (length(reports) == 1 & any(reports %in% names(.allReports))) {
    reports <- allReports[grepl(reports, allReports)]
  } else if (any(reports %in% allReports)) {
    reports <- allReports[allReports %in% reports]
  } else {
    stop("Could not match the reports.")
  }

  reports <- lapply(allReports, FUN = function(x) {
    ifelse(x %in% reports, TRUE, FALSE)
  })

  names(reports) <- allReports

  reports <- expand_reports(reports)
  return(reports)
}
