#' Get the symptom data
#'
#' @param channel open DBI connection
#' @param jaar the year of which the symptoms are to be read
#' @param sqlfile the sql file containing the query for the symptoms. When package then us the sql file that is shipped with the package
#' @param show_query flag if the query should be shown in the standard output
#' @param local whether the data is loaded from load path or imported from the db and then saved in the load_path
#' @param load_path path to save/load data
#'
#' @return data.frame with symptom data
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr if_else
#' @importFrom utils str
#' @export
#'
get_symptomdata <- function(channel,
                            jaar,
                            sqlfile = "package",
                            show_query = FALSE,
                            local = FALSE,
                            load_path = "data/dfSymptoms.RDS") {

  if (local) {
    return(readRDS(load_path))
  }

  if (sqlfile == "package") {
    sqlfile <- file.path(system.file(package = "inbobosvitaliteit"),
                         "extdata",
                         "tree_symptom_info.SQL")
  }
  if (local) {
    return(readRDS(load_path))
  }
  jaarstring <- paste(jaar, collapse = ",")
  whereClause = paste0(" where w.WRNG_JAA in (", jaarstring, ")")
  sql <- paste(c(readLines(sqlfile), whereClause), collapse = "\n")
  if (show_query) cat(sql, "\n")

  df <-
    dbGetQuery(channel, sql, stringsAsFactors = FALSE, nullstring = -1) %>%
    mutate(SymptoomCode = as.numeric(.data$SymptoomCode),
           SymptoomOorzaakCode = as.numeric(.data$SymptoomOorzaakCode),
           AangetastDeelCode = as.numeric(.data$AangetastDeelCode))
  df <- df %>%
    mutate(OnderdeelBoomCat =
             if_else(.data$AangetastDeelCode < 0,
                     "Unknown",
                     if_else(.data$AangetastDeelCode < 20,
                             "Bladeren",
                             if_else(.data$AangetastDeelCode < 30,
                                     "Takken",
                                     "Stam"))),
           SymptoomExtent =
             if_else(.data$SymptoomMeting < 0,
                     "Unknown",
                     if_else(.data$SymptoomMeting > 1,
                             "Zware schade",
                             "Beperkte schade")),
           Symptoom =
             replace(.data$Symptoom, .data$Symptoom == "-1", "NVT"),
           SymptoomSpecificatie =
             replace(.data$SymptoomSpecificatie,
                     .data$SymptoomSpecificatie == "-1",
                     "Unknown"),
           SymptoomExtent =
             if_else(.data$SymptoomMeting < 0,
                     "Unknown",
                     if_else(.data$SymptoomMeting < 2,
                             "Beperkte schade",
                             "Zware schade")),
           SymptoomVerkleurd =
             if_else(.data$SymptoomCode %in% c(2,3),
                     TRUE,
                     FALSE), #nodig omdat bomen beide verkleuringen samen kunnen
           SymptoomAbnormaalVerkleurd =
             if_else(.data$SymptoomCode %in% c(2,3) & .data$SymptoomMeting > 1,
                     TRUE,
                     FALSE),
           LeeftijdSchade =
             replace(.data$LeeftijdSchade,
                     .data$LeeftijdSchade == "-1",
                     "Unknown"),
           SymptoomOorzaak =
             replace(.data$SymptoomOorzaak,
                     .data$SymptoomOorzaak == "-1",
                     "Unknown"),
           SymptoomOrganisme =
             replace(.data$SymptoomOrganisme,
                     .data$SymptoomOrganisme == "-1",
                     "Unknown"),
           SymptoomOorzaakGroep =
             if_else(.data$SymptoomOorzaakCode < 1000,
                     floor(.data$SymptoomOorzaakCode/100)*100,
                     floor(.data$SymptoomOorzaakCode/10000)*100),
           SymptoomOorzaakGroepNaam =
             factor(.data$SymptoomOorzaakGroep,
                    levels = c(100,200,300,400,500,800,900),
                    labels = c("vraat (wild, vee)",
                               "insecten",
                               "schimmels",
                               "abiotisch",
                               "mens",
                               "andere",
                               "onbekend")),
           AantastingsKey =
             paste(.data$MetingKey,
                   .data$AangetastDeelCode,
                   .data$SymptoomCode,
                   .data$SymptoomSpecCode,
                   sep = "_"))

  if (!is.null(df)) try(saveRDS(df, load_path))
  return(df)
}
