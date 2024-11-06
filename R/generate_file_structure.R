#' Generate the necessary folders for the project exports
#'
#' @param root directory where the file structure should start from
#'
#' @return directories created on the file system
#' @export
#'
generate_file_structure <- function(root = getwd()) {
  if (!dir.exists("data")) dir.create("data")
  if (!dir.exists("output")) dir.create("output")
  if (!dir.exists("scripts")) dir.create("scripts")
}
