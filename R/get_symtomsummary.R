#' Find number of symptoms in the whole dataset
#'
#' @param trees dataset with the tree information
#' @param symptoms dataset with the symptom information
#'
#' @return data.frame containng the number of symptoms per tree
#' @export
#'
get_symtomsummary <- function(trees, symptoms) {
  str(trees$MetingKey)
  str(symptoms$MetingKey)
  df <-
    left_join(trees, symptoms, by = c("MetingKey")) %>%
    group_by(.data$WaarnemingKey, .data$Jaar, .data$PlotNr, .data$BoomNr)

  dfsum <-
    summarize(df,
              AantalSymptomen = sum(!is.na(.data$SymptoomCode)))
  dfsum
}
