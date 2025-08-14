#' @title generate_report
#' @description Generates a Quarto pdf report containing participant feedback from the study visit
#' @param id The patient id as represented in the study database
#' @param datafile The path name of the file containing the data as a .csv file
#' @param acceldir The path name of the directory containing the raw .gt3x accelerometer files
#' @param outputdir The path name of the directory containing the participant feedback reports
#' @param reports A vector of reports to generate including cognition, mri, pet, blood, apoe, activity, and lifestyle, Default = NULL returns all available reports,
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

generate_report <- function(
  id,
  datafile,
  acceldir,
  outputdir,
  reports = NULL,
  example_report = FALSE
) {
  if (outputdir == getwd()) {
    stop(
      "\nCurrent working directory is: ",
      getwd(),
      "\noutputdir needs to be different than your current working directory"
    )
  }

  allReports <- c(
    "cognition",
    "mri",
    "pet",
    "blood",
    "apoe",
    "activity",
    "lifestyle"
  )

  if (is.null(reports)) {
    reports <- allReports
  }

  reports <- lapply(allReports, FUN = function(x) {
    ifelse(x %in% reports, TRUE, FALSE)
  })

  names(reports) <- allReports

  if (example_report) {
    id = "test_jayhawk"
    datafile = system.file("extdata/example.csv", package = "BRIDGE21")
  }

  data <- readr::read_csv(
    datafile,
    show_col_types = FALSE,
    col_select = c(ptid, coenrol_studyid)
  )
  if (!id %in% unique(data$ptid)) {
    stop(sprintf("We did not find patient ID %s in the dataset", id))
  }
  # Create new directories
  persondir <- file.path(outputdir, id)
  if (!dir.exists(persondir)) {
    dir.create(persondir)
  }
  imagedir <- file.path(persondir, "imaging")
  if (!dir.exists(imagedir)) {
    dir.create(imagedir)
  }

  if (example_report) {
    mriFiles <- system.file(
      c("images/axialImage.png", "images/sagittalImage.png"),
      package = "BRIDGE21"
    )
    invisible(file.copy(
      from = mriFiles,
      to = file.path(imagedir, basename(mriFiles))
    ))
  } else {
    invisible(save_xnat_images(imagedir, data, id))
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
  } else {
    accelres <- run_ggir(id = id, acceldir = acceldir, outputdir = outputdir)
  }

  # Copy the qmd file from the CohortT21Disclosure package to render locally
  qmdfolder <- system.file("qmd", package = "BRIDGE21")
  if (!dir.exists(id)) {
    dir.create(id)
  }
  invisible(file.copy(
    from = list.files(qmdfolder, full.names = TRUE),
    to = id,
    recursive = TRUE,
    overwrite = TRUE
  ))

  # Saves a yaml file to pass the id and directory information to the quarto document
  yaml::write_yaml(
    c(
      list(
        id = id,
        datafile = datafile,
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
    output_file = pdffile
  )
  invisible(file.copy(
    from = pdffile,
    to = file.path(persondir, pdffile),
    overwrite = TRUE
  ))
  # Remove pdf file after it is copied to the output directory
  invisible(file.remove(pdffile))
  # Unlink the temporary folder to remove it
  invisible(unlink(id, recursive = TRUE))
  return(file.path(persondir, pdffile))
}
