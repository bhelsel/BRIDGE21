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


generate_report <- function(id, datadir, acceldir, outputdir){
  # Locate data file and verify that id is in the ptid variable
  datafile <- list.files(datadir, "All_Data", full.names = TRUE)
  if(length(datafile) == 0) stop(sprintf("Could not locate the REDCap Data in '%s'", datadir))
  data <- readr::read_csv(datafile, show_col_types = FALSE, col_select = ptid)
  if(!id %in% unique(data$ptid)) stop(sprintf("We did not find patient ID %s in the dataset", id))
  # Create new directories
  persondir <- file.path(outputdir, id)
  if(!dir.exists(persondir)) dir.create(persondir)
  imagedir <- file.path(persondir, "imaging")
  if(!dir.exists(imagedir)) dir.create(imagedir)
  # Retrieve and download scans from the XNAT API
  checkdicm <- unique(dirname(list.files(imagedir, recursive = TRUE, pattern = ".dcm$")))
  todownload = c()
  if(!any(grepl("AX_T2_FLAIR", checkdicm)) | !any(grepl("Sagittal_3D_Accelerated_MPRAGE", checkdicm))){
    project_no <- kuadrc.xnat::get_projects(name = "DS-Cohort")
    experiments <- kuadrc.xnat::get_experiments(project = project_no, subject = gsub("RED_", "", id))
    scans <- kuadrc.xnat::get_scans(experiment = experiments$ID)
  }
  if(!any(grepl("AX_T2_FLAIR", checkdicm))){
    todownload = c(todownload, which(scans$type == "AX_T2_FLAIR"))
  }
  if(!any(grepl("Sagittal_3D_Accelerated_MPRAGE", checkdicm))){
    todownload = c(todownload, which(scans$type == "Sagittal 3D Accelerated MPRAGE"))
  }
  if(length(todownload) >= 1){
    if(length(todownload) == 2) todownload <- paste0(todownload, collapse = ",")
    kuadrc.xnat::xnat_download(
      outdir = imagedir, project = project_no,
      subject = gsub("RED_", "", id), scan = todownload)
  }
  # Convert to nifti files and save the default image as a .png file
  checkpng <- list.files(imagedir, recursive = TRUE, pattern = ".png$")
  if(length(checkpng) == 0){
    scandir <- list.files(file.path(imagedir, "DS-Cohort"), full.names = TRUE)
    kuadrc.xnat::convert_to_nifti(directory = scandir)
    kuadrc.xnat::save_nifti_image(path = file.path(scandir, "nifti"))
  }
  # Copy the qmd file from the CohortT21Disclosure package to render locally
  qmdfolder <- system.file("qmd", package = "BRIDGE21")
  if(!dir.exists(id)) dir.create(id)
  invisible(file.copy(from = list.files(qmdfolder, full.names = TRUE), to = id, recursive = TRUE, overwrite = TRUE))
  # Process accelerometer data
  accelres <- run_ggir(id = id, acceldir = acceldir, outputdir = outputdir)
  # Saves a yaml file to pass the id and directory information to the quarto document
  yaml::write_yaml(
    list(id = id, datadir = datadir, outputdir = outputdir, imagedir = imagedir, accelres = accelres),
    file = sprintf("%s/_variables.yaml", id)
    )
  # Name the report using the participant id
  pdffile <- sprintf("%s_Report.pdf", id)
  # Render the quarto document
  quarto::quarto_render(input = sprintf("%s/BRIDGE21.qmd", id), output_file = pdffile)
  invisible(file.copy(from = pdffile, to = file.path(persondir, pdffile), overwrite = TRUE))
  # Remove pdf file after it is copied to the output directory
  invisible(file.remove(pdffile))
  # Unlink the temporary folder to remove it
  invisible(unlink(id, recursive = TRUE))
  return(file.path(persondir, pdffile))
}
