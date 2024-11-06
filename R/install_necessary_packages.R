#' Installeer benodigde packages
#'
#' @return installed libraries
#' @importFrom utils install.packages installed.packages
#' @importFrom remotes install_github
#' @export
#'
install_necessary_packages <- function() {
  #benodigde libraries
  packages <- c("here", "odbc", "DBI", "tidyverse", "rkt", "rlang", "lme4", "remotes", "conflicted")
  install.packages(setdiff(packages, rownames(installed.packages())))

  #benodigde libraries vanuit github
  install_github("inbo/INBOmd", dependencies = TRUE)
  install_github("inbo/INBOtheme", dependencies = TRUE)
}
