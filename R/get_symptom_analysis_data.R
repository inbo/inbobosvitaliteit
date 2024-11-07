
#' Join the treedata with the symptomdata on the variable MetingKey
#'
#' @param trees dataset containing tree information
#' @param symptoms  dataset containing symptom information
#'
#' @return data.frame with the joined dataset
#' @export
#'
get_symptom_analysis_data <- function(trees, symptoms) {
  left_join(trees, symptoms, by = "MetingKey")
}
