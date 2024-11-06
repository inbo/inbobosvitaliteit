#' Get script location (using a local copy or not)
#'
#' @param local do you want a local copy on your hard disk to work from
#' @param target target path when a local copy is made
#'
#' @return path to script location
get_script_location <- function(local = FALSE, target = "scripts") {
  if (!local) {
    script_path <-
      file.path(system.file(package = "inbobosvitaliteit"), "scripts")
    return(script_path)
  }
  #else
  if (!dir.exists(target)) dir.create(target)
  files <- list.files(system.file(package = "inbobosvitaliteit", "scripts"),
                      full.names = TRUE)
  for (fp in files) {
    file.copy(fp, to = file.path(getwd(), target))
  }
  script_path <- target #script directory on local hard drive
  return(script_path)
}
