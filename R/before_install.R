
#' To run before newly installing inbobosvitaliteit package
#'
#' @return de nodige files worden in de package directory geschreven
#' @examples
#' \dontrun{
#' library(inbolims)
#' setwd(here::here()
#' before_install()
#' }

before_install <- function() {
  generate_tree_species_data("inst/extdata")
  generate_sql_files("inst/extdata")
}
