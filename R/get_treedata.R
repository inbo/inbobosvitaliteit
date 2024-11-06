
#' Get the basic tree information from the db
#'
#' @param channel open DBI SQL server channel
#' @param jaar current year
#' @param tree_indeling file containing the tree metadata
#' @param sqlfile file containing the sql query
#' @param local if TRUE get the data from load_path else get the data from the DB and save a copy in load_path
#' @param load_path path to save/load data to/from
#' @param show_query flag if query should be shown in standard output
#'
#' @importFrom utils head
#'
#' @return a data.frame containing all the tree information from the query
#' @export
#'
get_treedata <- function(channel,
                         jaar,
                         tree_indeling,
                         sqlfile = "package",
                         load_path = "data/dfTrees.RDS",
                         show_query = FALSE,
                         local = FALSE
) {
  if (local) {
    if (length(jaar) == 1)
      return(readRDS(load_path))
    if (length(jaar) == 2)
      return(readRDS(gsub(".RDS", "_2.RDS", load_path)))
    if (length(jaar) == 3)
      return(readRDS(gsub(".RDS", "_3.RDS", load_path)))
    if (length(jaar) > 3)
      return(readRDS(gsub(".RDS", "_trend.RDS", load_path)))
  }

  if (sqlfile == "package") {
    sqlfile <- file.path(system.file(package = "inbobosvitaliteit"),
                         "extdata",
                         "tree_info.SQL")
  }

  if (substring(load_path, nchar(load_path)-3, nchar(load_path)) != ".RDS")
    stop("File extension of load_path should be .RDS")

  if (length(jaar) == 2)
    load_path <- gsub(".RDS", "_2.RDS", load_path)
  if (length(jaar) == 3)
    load_path <- gsub(".RDS", "_3.RDS", load_path)
  if (length(jaar) > 3)
    load_path <- gsub(".RDS", "_trend.RDS", load_path)

  jaarstring <- paste(jaar, collapse = ",")
  whereClause <-  paste0(" where w.WRNG_JAA in (", jaarstring, ")")
  sql <- paste(c(readLines(sqlfile), whereClause), collapse = "\n")
  if (show_query) cat(sql, "\n")

  df <- dbGetQuery(channel, sql)
  maxOmtrekKlasse <- ceiling(max(df$Omtrek, na.rm = TRUE)/50)
  df <-
    mutate(df,
           BladverliesNetto = as.numeric(.data$BladverliesNetto),
           Soortnummer = as.integer(.data$Soortnummer),
           BVKlasseEur = cut(.data$BladverliesNetto,
                             breaks = c(0, 10, 25, 60, 99, 100),
                             include.lowest = TRUE,
                             labels = c("0-10%",
                                        "10+-25%",
                                        "25+-60%",
                                        "60+-99%",
                                        "100%")),
           Beschadigd = cut(.data$BladverliesNetto,
                            breaks = c(0,25,100),
                            include.lowest = TRUE,
                            labels = c("onbeschadigd", "beschadigd")),
           BeschadigdNum = as.numeric(.data$Beschadigd) - 1,
           Dood = cut(.data$BladverliesNetto,
                      breaks = c(0, 99.9, 100),
                      include.lowest = TRUE,
                      labels = c("levend", "dood")),
           LeeftijdsklasseEur = cut(.data$Leeftijd,
                                    breaks = c(0, 59, Inf),
                                    labels = c("jong", "oud"),
                                    include.lowest = TRUE),
           OmtrekklasseEur = cut(.data$Omtrek,
                                 breaks = 0:maxOmtrekKlasse * 50,
                                 include.lowest = TRUE,
                                 labels = paste0(0:(maxOmtrekKlasse - 1)*50,
                                                 "-",
                                                 1:maxOmtrekKlasse*50,
                                                 "cm")),
           BVKlasse5 = cut(.data$BladverliesNetto,
                           breaks = c(0, 1, seq(5,95,by = 5), 99, 100),
                           include.lowest = TRUE,
                           labels = paste0(c(0,1 + 0:19*5, 100),
                                           c("", rep("-", 20), ""),
                                           c("", 1:19*5, 99, ""),
                                           c(rep("%", 21), "% dode boom")
                           )),
           BVKlasse10 = cut(.data$BladverliesNetto,
                            breaks = c(0:10*10), include.lowest = TRUE,
                            labels = paste0(c(0, seq(11,91, by = 10)), "-",
                                            c(seq(10,100,by = 10)), "%")),
           BVKlassePiechart = cut(.data$BladverliesNetto,
                                  breaks = c(0,10,25,40,100),
                                  include.lowest = TRUE,
                                  labels = paste(c(0,11,26,41),
                                                 c(10,25,40,100),
                                                 sep = "-")),
           prbo = paste0(.data$PlotNr, .data$BoomNr))

  #Indien 2-jaarlijks (2 jaren gespecificeerd)
  if (length(jaar) == 2) {
    df <- df %>%
      mutate(JaarS2 = ifelse(.data$Jaar == jaar[1],
                             "J1",
                             ifelse(.data$Jaar == jaar[2],
                                    "J2",
                                    NA)))
  }

  #Indien 3-jaarlijks (3 jaren gespecificeerd)
  if (length(jaar) == 3) {
    df <- df %>%
      mutate(JaarS3 = ifelse(.data$Jaar == jaar[1],
                             "J1",
                             ifelse(.data$Jaar == jaar[2],
                                    "J2",
                                    ifelse(.data$Jaar == jaar[3],
                                           "J3",
                                           NA))))
  }

  #join met soortindeling
  df <- df %>%
    left_join(tree_indeling, by = c("Soortnummer" = "SPEC_EUR_CDE"))

  #bewaar de data lokaal
  if (!is.null(df)) try(saveRDS(df, load_path))

  #return
  return(df)
}
