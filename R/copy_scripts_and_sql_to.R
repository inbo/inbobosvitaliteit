
#' Kopieer scripts, sql en csv data bestanden naar lokale directory
#'
#' @param target directory waarin de files weggeschreven worden
#'
#' @return lokale kopie van scripts en sql en csv data in de scripts en data directory onder de target
#'
copy_scripts_and_sql_to <- function(target) {
  lib_dir <- system.file(package = "inbobosvitaliteit")
  script_dir <- file.path(target, "scripts")
  sql_dir <- file.path(target, "data")
  if(!dir.exists(script_dir)) dir.create(script_dir)
  if(!dir.exists(sql_dir)) dir.create(sql_dir)
  script_files <- list.files(file.path(lib_dir, "scripts"))
  for (file in script_files) {
    file.copy(file.path(lib_dir, "scripts", file), script_dir)
  }
  data_files <- list.files(file.path(lib_dir, "extdata"))
  for (file in data_files) {
    file.copy(file.path(lib_dir, "extdata", file), sql_dir)
  }
}
