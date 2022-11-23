#' Read species metadata
#'
#' @param path locatie van de csv die de gegevens bevat
#' @param local if TRUE read the csv and save the results in load_path else load the data from load_path
#' @param load_path path where imported data is saved for load later
#' @importFrom dplyr rename
#' @importFrom readr read_csv2
#'
#' @return data.frame with species grouping information
#' @export
#'
read_species_information <-
  function(path = "data/tree_reportmetadata.csv",
           local = FALSE,
           load_path = "data/dfSoortInfo.RDS") {
    if(!local) {
      rv <- read_csv2(path) %>%
        rename(Soort = .data$species,
               SoortType = .data$species_main_cat,
               SoortIndeling = .data$species_sub_cat,
               SoortVolgorde = .data$species_order)
      dim(rv)
      if (!is.null(rv) & length(rv))
        saveRDS(rv, file = load_path)
    } else {
      rv <- readRDS(load_path)
    }
    return(rv)
  }



###############################################################


#' Generate tree species metadatafile for use in reporting
#'
#' @param target directory to export the species information to
#'
#' @return a csv (;) file generated with columns SPEC_EUR_CDE (European species code), SPEC_DES (scientific name), species, species_main_cat (report categorisation general), species_sub_cat (more detailed categorisation), species_name (English name), species_order (Reporting order)
#' @export
generate_tree_species_data <- function(target = "data") {
  cat("SPEC_EUR_CDE;SPEC_DES;species;species_main_cat;species_sub_cat;species_name;species_order
      999;;totaal;totaal;totaal;total;0
      9998;;totaal jong;totaal jong;totaal jong;total young;1
      9999;;totaal oud;totaal oud;totaal oud;total old;2
      948;;loofbomen;loofbomen;loofbomen;deciduous trees;11
      9997;;jonge loofbomen;jonge loofbomen;loofbomen jong;deciduous young;12
      9996;;oude loofbomen;oude loofbomen;loofbomen oud;deciduous old;13
      998;;naaldbomen;naaldbomen;naaldbomen;conifers;211
      9995;;jonge naaldbomen;jonge naaldbomen;naaldbomen jong;conifers young;212
      9994;;oude naaldbomen;oude naaldbomen;naaldbomen oud;conifers old;213
      51;Quercus robur;zomereik;loofbomen;zomereik;oak;101
      20;Fagus sylvatica;beuk;loofbomen;beuk;beech;102
      53;Quercus rubra;Amerikaanse eik;loofbomen;Amerikaanse eik;northern red oak;103
      947;;overige lbs.;loofbomen;overige lbs.;other deciduous;109
      33;Populus hybrides;populier;loofbomen;overige lbs.;poplar;111
      7;Alnus glutinosa;zwarte els;loofbomen;overige lbs.;black alder;112
      15;Castanea sativa;tamme kastanje;loofbomen;overige lbs.;sweet chestnut;113
      48;Quercus petraea;wintereik;loofbomen;overige lbs.;sessile oak;114
      22;Fraxinus excelsior;es;loofbomen;overige lbs.;ash;115
      5;Acer pseudoplatanus;esdoorn;loofbomen;overige lbs.;sycamore maple;116
      10;Betula pendula;ruwe berk;loofbomen;overige lbs.;silver berch;117
      56;Robinia pseudoacacia;valse acacia;loofbomen;overige lbs.;black locust (false acacia);118
      32;Populus canescens;grauwe abeel;loofbomen;overige lbs.;grey poplar;119
      13;Carpinus betulus;haagbeuk;loofbomen;overige lbs.;hornbeam;120
      8;Alnus incana;witte els;loofbomen;overige lbs.;grey alder;121
      36;Prunus avium;zoete kers;loofbomen;overige lbs.;sweet cherry;122
      11;Betula pubescens;zachte berk;loofbomen;overige lbs.;downy (moor, white) birch;123
      72;Ulmus minor;gladde iep;loofbomen;overige lbs.;field elm;124
      35;Populus tremula;ratelpopulier;loofbomen;overige lbs.;aspen;125
      134;Pinus sylvestris;grove den;naaldbomen;grove den;Scots pine;301
      129;Pinus nigra;Corsicaanse den;naaldbomen;Corsicaanse den;black pine;302
      997;;overige nbs.;naaldbomen;overige nbs.;other conifers;309
      117;Larix kaempferi;Japanse lork;naaldbomen;overige nbs.;Japanese larch;311
      118;Picea abies;fijnspar;naaldbomen;overige nbs.;Norway spruce;312
      136;Pseudotsuga menziesii;douglas;naaldbomen;overige nbs.;douglas fir;313",
      file = file.path(target, "tree_reportmetadata.csv"), append = FALSE)
  return(invisible())
}


#####################################################

#' Generate the SQL files used to communicate with the database
#'
#' @param target target directory where the files should be placed
#'
#' @return sql files created in the target directory
#' @export
#'
generate_sql_files <- function(target = "data") {
  #tree_info.SQL
  cat("select
  p.PLOT_ID as PlotKey
      , b.BOOM_ID as BoomKey
      , w.WRNG_ID as WaarnemingKey
      , m.WRME_ID as MetingKey
      , p.PLOT_NUM as PlotNr
      , p.PLOT_NAM as PlotNaam
      , g.GMTE_NAM as Gemeente
      , b.BOOM_BNR as BoomNr
      , w.WRNG_DTE as Datum
      , w.WRNG_JAA as Jaar
      , s.SPEC_EUR_CDE as Soortnummer
      , m.WRME_OMT as Omtrek
      , m.WRME_LFT as Leeftijd
      , m.WRME_UCLT_CDE AS Leeftijdsklasse
      , m.WRME_UCBL_CDE as BladverliesNetto
      , m.WRME_UCNJ_CDE AS AantalNaaldjaargangen
      , m.WRME_UCZZ_CDE AS Zaadzetting
      , m.WRME_UCWS_CDE AS Waterscheuten
      , b.BOOM_JDO as SterfteJaar
      , b.BOOM_REMO_CDE VerwijderdCode
      , cdem.REMO_DES as VerwijderdReden
      , m.WRME_OPM as MetingOpmerking
      from
      tblProefvlak p left join
      tblWaarneming w on w.WRNG_PLOT_ID = p.PLOT_ID left join
      tblWaarnemingMeting m on m.WRME_WRNG_ID = w.WRNG_ID left join
      tblBoom b on b.BOOM_ID = m.WRME_BOOM_ID left join
      tblSoort s on s.SPEC_ID = b.BOOM_SPEC_ID left join
      cdeRemovalsMortality cdem on b.BOOM_REMO_CDE = cdem.REMO_CDE left join
      tblGemeente g on g.GMTE_NIS_CDE = p.PLOT_GMTE_NIS_CDE\n",
      file = file.path(target, "tree_info.SQL"), append = FALSE)

  #tree_symptom_info.SQL
  cat("
select
  m.WRME_ID as MetingKey
      , sym.WMSY_ATDE_CDE AangetastDeelCode
      , cad.ATDE_DES AangetastDeel
      , sym.WMSY_SYSP_CDE SymptoomSpecCode
      , css.SYSP_DES SymptoomSpecificatie
      , sym.WMSY_SYTO_CDE SymptoomCode
      , csym.SYTO_DES Symptoom
      , sym.WMSY_UCME_CDE SymptoomMeting
      , sym.WMSY_LEFT_CDE LeeftijdSchadeCode
      , clft.LEFT_DES as LeeftijdSchade
      , oorz.OORZ_CDE SymptoomOorzaakCode
      , oorz.OORZ_DES SymptoomOorzaak
      , ozor.OZOR_CDE SymptoomOrganismeCode
      , ozor.OZOR_NAM SymptoomOrganisme
      , sym.WMSY_OPM SymptoomOpmerking
      from
      tblProefvlak p left join
      tblWaarneming w on w.WRNG_PLOT_ID = p.PLOT_ID left join
      tblWaarnemingMeting m on m.WRME_WRNG_ID = w.WRNG_ID left join
      tblWaarnemingMetingSymptoom sym on sym.WMSY_WRME_ID = m.WRME_ID left join
      tblBoom b on b.BOOM_ID = m.WRME_BOOM_ID left join
      tblSoort s on s.SPEC_ID = b.BOOM_SPEC_ID  left join
      cdeAangetastDeel cad on cad.ATDE_CDE = sym.WMSY_ATDE_CDE left join
      cdeSymptoomSpecificatie css on css.SYSP_CDE = sym.WMSY_SYSP_CDE left join
      cdeSymptoom csym on csym.SYTO_CDE = sym.WMSY_SYTO_CDE left join
      cdeLeeftijdSchade clft on clft.LEFT_CDE = sym.WMSY_LEFT_CDE left join
      tblMetingSymptoomOorzaak syo on syo.MSYO_WMSY_ID = sym.WMSY_ID left join
      tblOorzaak oorz on oorz.OORZ_ID = syo.MSYO_OORZ_ID left join
      tblOorzaakOrganisme ozor on ozor.OZOR_ID = syo.MSYO_OZOR_ID\n",
      file = file.path(target, "tree_symptom_info.SQL"), append = FALSE )

  #natuurindicatoren.sql
  cat("
select
p.PLOT_NUM as PlotNr
, b.BOOM_BNR as BoomNr
, w.WRNG_JAA as Jaar
, s.SPEC_EUR_CDE as Soortnummer
, m.WRME_OMT as Omtrek
, m.WRME_LFT as Leeftijd
, m.WRME_UCBL_CDE as BladverliesNetto
from
tblProefvlak p left join
tblWaarneming w on w.WRNG_PLOT_ID = p.PLOT_ID left join
tblWaarnemingMeting m on m.WRME_WRNG_ID = w.WRNG_ID left join
tblBoom b on b.BOOM_ID = m.WRME_BOOM_ID left join
tblSoort s on s.SPEC_ID = b.BOOM_SPEC_ID",
      file = file.path(target, "natuurindicatoren.sql"), append = FALSE)
}


##########################
##########################


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
#' @param sql file containing the sql query
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
                         sql = "data/tree_info.SQL",
                         show_query = FALSE,
                         local = FALSE,
                         load_path = "data/dfTrees.RDS") {
  if (substring(load_path, nchar(load_path)-3, nchar(load_path)) != ".RDS")
    stop("File extension of load_path should be .RDS")

  if (length(jaar) == 2)
    load_path <- gsub(".RDS", "_2.RDS", load_path)
  if (length(jaar) == 3)
    load_path <- gsub(".RDS", "_3.RDS", load_path)
  if (length(jaar) > 3)
    load_path <- gsub(".RDS", "_trend.RDS", load_path)

  if (local) {
      return(readRDS(load_path))
  }

  #deze 2 lijnen code zijn nog niet in orde
  if(is.null(tweejaarlijks)) tweejaarlijks <- c(max(jaar) - 1, max(jaar))
  if(is.null(driejaarlijks))
    driejaarlijks <- c(max(jaar) - 2, max(jaar) -1, max(jaar))

  jaarstring <- paste(jaar, collapse = ",")
  whereClause <-  paste0(" where w.WRNG_JAA in (", jaarstring, ")")
  sql <- paste(c(readLines(sql), whereClause), collapse = "\n")
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
                             labels = c("0-10%", "10+-25%", "25+-60%", "60+-99%", "100%")),
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
                                 labels = paste0(0:(maxOmtrekKlasse - 1)*50, "-", 1:maxOmtrekKlasse*50, "cm")),
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
                                  labels = paste(c(0,11,26,41), c(10,25,40,100), sep = "-")),
           JaarS2 = ifelse(.data$Jaar == tweejaarlijks[1],
                           "J1",
                           ifelse(.data$Jaar == tweejaarlijks[2],
                                  "J2",
                                  NA)),
           JaarS3 = ifelse(.data$Jaar == driejaarlijks[1],
                           "J1",
                           ifelse(.data$Jaar == driejaarlijks[2],
                                  "J2",
                                  ifelse(.data$Jaar == driejaarlijks[3],
                                         "J3",
                                         NA))),
           prbo = paste0(.data$PlotNr, .data$BoomNr)) %>%
    left_join(tree_indeling, by = c("Soortnummer" = "SPEC_EUR_CDE"))
  if (!is.null(df)) saveRDS(df, load_path)
  return(df)
}

#######################################################################

#' Get the symptom data
#'
#' @param channel open DBI connection
#' @param jaar the year of which the symptoms are to be read
#' @param sql the sql file containing the query for the symptoms
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
                            sql = "data/tree_symptom_info.SQL",
                            show_query = FALSE,
                            local = FALSE,
                            load_path = "data/dfSymptoms.RDS") {
  if (local) {
    return(readRDS(load_path))
  }
  jaarstring <- paste(jaar, collapse = ",")
  whereClause = paste0(" where w.WRNG_JAA in (", jaarstring, ")")
  sql <- paste(c(readLines(sql), whereClause), collapse = "\n")
  if (show_query) cat(sql)

  df <-
    dbGetQuery(channel, sql, stringsAsFactors = FALSE, nullstring = -1) %>%
    mutate(SymptoomCode = as.numeric(.data$SymptoomCode),
           SymptoomOorzaakCode = as.numeric(.data$SymptoomOorzaakCode),
           AangetastDeelCode = as.numeric(.data$AangetastDeelCode))

  print(str(df))

  df <- df %>%
    mutate(OnderdeelBoomCat = if_else(.data$AangetastDeelCode < 0, "Unknown",
                                      if_else(.data$AangetastDeelCode < 20,
                                              "Bladeren",
                                              if_else(.data$AangetastDeelCode < 30,
                                                      "Takken", "Stam"))),
           SymptoomExtent = if_else(.data$SymptoomMeting < 0, "Unknown",
                                    if_else(.data$SymptoomMeting > 1, "Zware schade", "Beperkte schade")),
           Symptoom = replace(.data$Symptoom, .data$Symptoom == "-1", "NVT"),
           SymptoomSpecificatie = replace(.data$SymptoomSpecificatie, .data$SymptoomSpecificatie == "-1", "Unknown"),
           SymptoomExtent = if_else(.data$SymptoomMeting < 0, "Unknown",
                                    if_else(.data$SymptoomMeting < 2, "Beperkte schade", "Zware schade")),
           SymptoomVerkleurd = ifelse(.data$SymptoomCode %in% c(2,3), TRUE, FALSE), #nodig omdat bomen beide verkleuringen samen kunnen hebben
           SymptoomAbnormaalVerkleurd = ifelse(.data$SymptoomCode %in% c(2,3) & .data$SymptoomMeting > 1, TRUE, FALSE ),
           LeeftijdSchade = replace(.data$LeeftijdSchade, .data$LeeftijdSchade == "-1", "Unknown"),
           SymptoomOorzaak = replace(.data$SymptoomOorzaak, .data$SymptoomOorzaak == "-1", "Unknown"),
           SymptoomOrganisme = replace(.data$SymptoomOrganisme, .data$SymptoomOrganisme == "-1", "Unknown"),
           SymptoomOorzaakGroep = ifelse(.data$SymptoomOorzaakCode < 1000,
                                         floor(.data$SymptoomOorzaakCode/100)*100,
                                         floor(.data$SymptoomOorzaakCode/10000)*100),
           SymptoomOorzaakGroepNaam = factor(.data$SymptoomOorzaakGroep,
                                             levels = c(100,200,300,400,500,800,900),
                                             labels = c("vraat (wild, vee)", "insecten", "schimmels", "abiotisch",
                                                        "mens", "andere", "onbekend")),
           AantastingsKey = paste(.data$MetingKey, .data$AangetastDeelCode, .data$SymptoomCode, .data$SymptoomSpecCode, sep = "_"))

  if (!is.null(df)) saveRDS(df, load_path)
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
