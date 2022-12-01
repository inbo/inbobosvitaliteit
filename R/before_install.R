
#' To run before newly installing inbobosvitaliteit package
#'
#' @return de nodige files worden in de package directory geschreven
before_install <- function() {
  generate_tree_species_data("inst/extdata")
  generate_sql_files("inst/extdata")
}

#setwd(here::here()
#before_install()

#setwd("2023")
# fp <- file.path(system.file(package = "inbobosvitaliteit"),
#                 "scripts", "00_base_script.R")
#
# file.copy(fp, to = file.path(getwd()), overwrite = TRUE)
