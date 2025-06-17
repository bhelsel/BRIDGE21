#' @title generate_report
#' @description Generates a Quarto pdf report containing participant feedback from the study visit
#' @param id The patient id as represented in the study database
#' @param datadir The path name of the directory containing the dataset as a .csv file
#' @param acceldir The path name of the directory containing the raw .gt3x accelerometer files
#' @param outputdir The path name of the directory containing the participant feedback reports
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
#' @importFrom kuadrc.xnat get_projects get_experiments get_scans xnat_download convert_to_nifti save_nifti_image
#' @importFrom yaml write_yaml
#' @importFrom quarto quarto_render

generate_report <- function(id, datadir, acceldir, outputdir) {
  if (outputdir == getwd()) {
    stop(
      "\nCurrent working directory is: ",
      getwd(),
      "\noutputdir needs to be different than your current working directory"
    )
  }
  # Locate data file and verify that id is in the ptid variable
  datafile <- list.files(datadir, "All_Data", full.names = TRUE)
  if (length(datafile) == 0) {
    stop(sprintf("Could not locate the REDCap Data in '%s'", datadir))
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
  # Retrieve and download scans from the XNAT API
  checkdicm <- unique(dirname(list.files(
    imagedir,
    recursive = TRUE,
    pattern = ".dcm$"
  )))

  #todownload = c()
  project <- "DS-Cohort"

  if (!any(grepl("Sagittal_3D_Accelerated_MPRAGE", checkdicm))) {
    project_no_ds_cohort <- kuadrc.xnat::get_projects(name = project)
    subject_no_ds_cohort <- kuadrc.xnat::get_subjects(
      project = project_no_ds_cohort,
      subject = sprintf("%s_%s", project_no_ds_cohort, gsub("RED_", "", id))
    )

    experiments <- tryCatch(
      {
        project_no <- project_no_ds_cohort
        subject_no <- subject_no_ds_cohort

        kuadrc.xnat::get_experiments(
          project = project_no_ds_cohort,
          subject = subject_no_ds_cohort
        )
      },
      error = function(e) {
        return(NULL)
      }
    )

    # Check for ABC-DS ID if experiments is NULL

    if (is.null(experiments)) {
      coenrol_studyid <- data[
        which(data$ptid == id),
        "coenrol_studyid",
        drop = TRUE
      ]

      coenrol_studyid <- coenrol_studyid[
        !is.na(coenrol_studyid) & startsWith(coenrol_studyid, "BDS")
      ]

      if (length(coenrol_studyid) != 0) {
        project <- "ABC-DS"
        project_no_abcds <- kuadrc.xnat::get_projects(name = project)
        subject_no_abcds <- kuadrc.xnat::get_subjects(
          project = project_no_abcds,
          subject = coenrol_studyid
        )
        experiments <- tryCatch(
          {
            project_no <- project_no_abcds
            subject_no <- coenrol_studyid
            kuadrc.xnat::get_experiments(
              project = project_no_abcds,
              subject = subject_no_abcds
            )
          },
          error = function(e) {
            return(NULL)
          }
        )
      }
    }

    if (!is.null(experiments)) {
      experiments <- experiments[grep("mr", experiments$xsiType), ]
      scans <- kuadrc.xnat::get_scans(experiment = experiments$ID)
      if (!any(grepl("Sagittal & Accelerated & MPRAGE", checkdicm))) {
        scan_no <- which(
          scans$type == "Sagittal 3D Accelerated MPRAGE" |
            scans$type == "Accelerated Sagittal MPRAGE (MSV21)"
        )
      }
      if (length(scans) == 1) {
        kuadrc.xnat::xnat_download(
          outdir = imagedir,
          project = project_no,
          subject = subject_no,
          experiment = experiments$ID,
          scan = scan_no
        )
      }
    }
  }

  # Convert to nifti files and save the default image as a .png file
  checkpng <- list.files(imagedir, recursive = TRUE, pattern = ".png$")
  scandir <- list.files(file.path(imagedir, project), full.names = TRUE)

  if (length(checkpng) == 0 & length(scandir) != 0) {
    kuadrc.xnat::convert_to_nifti(directory = scandir)
    kuadrc.xnat::save_nifti_image(
      path = file.path(scandir, "nifti"),
      plane = "axial",
      save_file_as = sprintf("%s_axial.png", id)
    )
    kuadrc.xnat::save_nifti_image(
      path = file.path(scandir, "nifti"),
      plane = "sagittal",
      save_file_as = sprintf("%s_sagittal.png", id)
    )
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
  # Process accelerometer data
  accelres <- run_ggir(id = id, acceldir = acceldir, outputdir = outputdir)
  # Saves a yaml file to pass the id and directory information to the quarto document
  yaml::write_yaml(
    list(
      id = id,
      datadir = datadir,
      outputdir = outputdir,
      imagedir = imagedir,
      accelres = accelres
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
