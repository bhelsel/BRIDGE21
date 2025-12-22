#' @title save_xnat_images
#' @description Retrieves MRI images from the XNAT API and saves as png files
#' @param imagedir The directory where the MRI images should be stored
#' @param data The data set containing the patient's information
#' @param id The patient id as represented in the study database
#' @return NULL
#' @details Retrieves MRI images from the XNAT API and saves as png files
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[kuadrc.xnat]{get_projects}}, \code{\link[kuadrc.xnat]{get_subjects}}, \code{\link[kuadrc.xnat]{get_experiments}}, \code{\link[kuadrc.xnat]{get_scans}}, \code{\link[kuadrc.xnat]{xnat_download}}, \code{\link[kuadrc.xnat]{convert_to_nifti}}, \code{\link[kuadrc.xnat]{save_nifti_image}}
#' @rdname save_xnat_images
#' @export
#' @importFrom kuadrc.xnat get_projects get_subjects get_experiments get_scans xnat_download convert_to_nifti save_nifti_image

save_xnat_images <- function(imagedir, data, id) {
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
          subject = unique(coenrol_studyid)
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
      if (length(scan_no) == 1) {
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
  return(NULL)
}
