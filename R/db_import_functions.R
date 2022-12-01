#' Read species metadata
#'
#' @param path locatie van de csv die de gegevens bevat, standaard "package" wat betekent dat de csv vanuit het package gehaald wordt. Indien je een custom indeling wil gebruiken zorg dat het een csv file is met de kolommen species, species_main_cat, species_sub_cat, species_order
#' @importFrom dplyr rename
#' @importFrom readr read_csv2
#'
#' @return data.frame with species grouping information
#' @export
#'
read_species_information <-
  function(path = "package") {
    if (path == "package")
      path <- file.path(system.file(package = "inbobosvitaliteit"),
                        "extdata",
                        "tree_reportmetadata.csv")
    rv <- read_csv2(path) %>%
      rename(Soort = .data$species,
             SoortType = .data$species_main_cat,
             SoortIndeling = .data$species_sub_cat,
             SoortVolgorde = .data$species_order)
    return(rv)
  }

#########################################################################


#' Connect to bosvitaliteit DB
#'
#' @return DBI DB connection object
#' @export
#'
bosvitaliteit_connect <- function(){
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = "SQL Server",
                        Server = "inbo-sql07-prd.inbo.be",
                        port = 1433,
                        Database = "D0004_00_Bosvitaliteit",
                        Trusted_Connection = "True")
  if(!inherits(con, "Microsoft SQL Server"))
    print("Connectie niet gelukt. Ben je op het INBO netwerk of via VPN verbonden? Contacteer de database administrator")
  con
}

###################################################################

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
    return(readRDS(load_path))
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
  print(head(df))
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

#######################################################################

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

  print(str(df))

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

############################################################

#' Join the treedata with the symptomdata on the variable MetingKey
#'
#' @param trees dataset containing tree information
#' @param symptoms  dataset containing symptom information
#'
#' @return data.frame with the joined dataset
#' @export
#'
get_SymptomAnalysisdata <- function(trees, symptoms) {
  left_join(trees, symptoms, by = "MetingKey")
}

#############################################################
