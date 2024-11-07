
#' Krijg het basisscript uit het inbobosvitaliteitspackage in werkdir
#'
#' @param output if file copy to 00_base_script.R else show code in console
#'
#' @return script contents in console or base script in a .R file
#' @export
#'
install_base_script <- function(output = c("file", "console")) {
  scriptname <- "00_base_script.R"
  if (file.exists(file.path(getwd(), scriptname))) {
    md5_existing <- tools::md5sum(file.path(getwd(), scriptname))
    exists <- TRUE
  } else {
    exists <- FALSE
  }
  script_in_pkg <- file.path(system.file(package = "inbobosvitaliteit"),
                             "scripts",
                             scriptname)
  md5_pkg <- tools::md5sum(script_in_pkg)

  if (exists) {
    if (md5_existing == md5_pkg) {
      message("File is exact hetzelfde, er wordt geen kopie gemaakt")
    } else {
      message("File is verschillend. De oudere file wordt gebackupt en vervangen")
      file.rename(file.path(getwd(), scriptname),
                  file.path(getwd(), paste0("00_base_script_before_", Sys.Date(), ".R")))
      status <- file.copy(script_in_pkg, scriptname)
      if (status) {
        message(paste0("File ", scriptname, " succevol gekopieerd naar ", getwd()))

      } else {
        warning("File copy is niet gelukt")
      }
    }
  } else {
    if (output[1] == "file") {
      status = file.copy(script_in_pkg, scriptname)
      if (status) {
        message(paste0("File ", scriptname, " succevol gekopieerd naar ", getwd()))

      } else {
        warning("File copy is niet gelukt")
      }

    } else {
      cat(paste(readLines(script_in_pkg), collapse = "\n"), "\n")
    }

  }
}
