
### DOOR GEBRUIKER TE WIJZIGEN: configuration variables

#Maak eerst een R project aan voor het rapportagejaar
#Open Rstudio via het .Rproj bestandje

#check de werkdirectory
getwd()

last_year <- 2022       #laatst gebruikte jaar in de dataset
years_2 <- c(last_year - 1, last_year)
years_3 <- c(last_year - 2, last_year - 1, last_year)
years_trend <- 1995:last_year
years_indicator <- 1987:last_year
source_file_vlaanderen_europa <- "vlaanderen_europa.csv" #zet deze file in de correcte werkdirectory (getwd())
setwd(getwd())    #locatie waar de bestanden moeten komen
install_base_script() #maak een lokale kopie van het basissscript

plot_base_size <- 10    #standaard tekstgrootte in figuren
use_local_db_export <- FALSE #gebruik reeds ingeladen data voor dit jaar
copy_local <- FALSE #optioneel: kopieer alles naar de lokale structuur

###Berekening bootstrap op sen-slope

#Eenmalig uitvoeren, duurt enkele uren
#alles wordt bewaard in de output/interim directory, dus eenmalig is genoeg
recalc_sen <- FALSE #eenmalig wel op TRUE, duurt lang
sen_boot <- 200 #hoeveelheid samples voor sen-bootstrap (standaard 200)

###Berekening mixed effect modellen

#Eenmalig uitvoeren, duurt ongeveer 3 uur
#alles wordt bewaard in de output/interim directory, dus eenmalig is genoeg
recalc_lmer <- FALSE #eenmalig wel op TRUE, duurt een hele poos
lmer_samples <- 10000 #hoeveelheid iteratiestappen brm (standaard 10000)



#zorg dat bovenstaande file in je werkdirectory staat en er als volgt uitziet:
# een kommagescheiden bestand hebt, met 3 kolommen met kolommen (zonder de commentaarhekjes zoals in het reële voorbeeld tem 2022  hieronden):
# Jaar,niveau,Aandeel
# 1990,Europa,19.5
# 1990,Vlaanderen,8.3
# 1991,Europa,20.7
# 1991,Vlaanderen,12.9
# 1992,Europa,23.7
# 1992,Vlaanderen,17.3
# 1993,Europa,24.2
# 1993,Vlaanderen,16.8
# 1994,Europa,24.6
# 1994,Vlaanderen,22.2
# 1995,Europa,26.9
# 1995,Vlaanderen,33.2
# 1996,Europa,25.4
# 1996,Vlaanderen,26.4
# 1997,Europa,24.2
# 1997,Vlaanderen,19.3
# 1998,Europa,23.2
# 1998,Vlaanderen,22.1
# 1999,Europa,21
# 1999,Vlaanderen,21.9
# 2000,Europa,22.2
# 2000,Vlaanderen,25.2
# 2001,Europa,22.4
# 2001,Vlaanderen,22.1
# 2002,Europa,21.3
# 2002,Vlaanderen,21.7
# 2003,Europa,22.7
# 2003,Vlaanderen,20
# 2004,Europa,23.3
# 2004,Vlaanderen,20.8
# 2005,Europa,23.2
# 2005,Vlaanderen,21.3
# 2006,Europa,21.9
# 2006,Vlaanderen,19.1
# 2007,Europa,21.8
# 2007,Vlaanderen,17.3
# 2008,Europa,21
# 2008,Vlaanderen,14.3
# 2009,Europa,20.2
# 2009,Vlaanderen,15.1
# 2010,Europa,19.5
# 2010,Vlaanderen,16.1
# 2011,Europa,20
# 2011,Vlaanderen,20.1
# 2012,Europa,22.9
# 2012,Vlaanderen,25
# 2013,Europa,20.5
# 2013,Vlaanderen,20.8
# 2014,Europa,23.8
# 2014,Vlaanderen,21.1
# 2015,Europa,23.3
# 2015,Vlaanderen,21.5
# 2016,Europa,25.2
# 2016,Vlaanderen,20.3
# 2017,Europa,25.1
# 2017,Vlaanderen,20.3
# 2018,Europa,27
# 2018,Vlaanderen,22.8
# 2019,Europa,28.4
# 2019,Vlaanderen,22.7
# 2020,Europa,28.2
# 2020,Vlaanderen,25.3
# 2021,Europa,28.6
# 2021,Vlaanderen,19.9
# 2022,Europa,NA
# 2022,Vlaanderen,26.6


#######################################################################

#Hieronder enkel wijzigen indien er problemen zijn

### INIT SESSIE
#---------------

### installatie en laden packages

if (!"remotes" %in% rownames(installed.packages())) install.packages("remotes")
remotes::install_github("inbo/inbobosvitaliteit", dependencies = TRUE)

inbobosvitaliteit::install_necessary_packages()

library(tidyverse)
#library(dplyr);library(ggplot2);library(readr);library(tidyr);library(purrr)
library(DBI)
library(inbobosvitaliteit)

### init sessie (zet verschillende controlevariabelen in de environment)

init_session(last_year, first_year = 1987, first_multiyear = 1995,
             connect_via_db = !use_local_db_export )

theme_set(INBOtheme::theme_inbo(plot_base_size)) #basisthema figuren

generate_file_structure(getwd()) #maak file structuur aan

if (copy_local) {
  copy_scripts_and_sql_to(getwd()) #maak een lokale kopie van scripts en sql
}


### Import data
#------------------

#db connectie
if (use_local_db_export) {
  conn <- NULL
} else {
  conn <- bosvitaliteit_connect()
}

##soortenmetadata
dfSoortInfo <- read_species_information()

##bomen laatste jaar
dfTrees <- get_treedata(conn,
                        jaar = last_year,
                        tree_indeling = dfSoortInfo,
                        local = use_local_db_export)
dim(dfTrees)


##symptomen laatste jaar
dfSymptoms <- get_symptomdata(conn,
                              jaar = last_year,
                              local = use_local_db_export,
                              show_query = TRUE)
dim(dfSymptoms)

##bomen de laatste 2 jaar
dfTrees2 <- get_treedata(conn,
                         jaar = years_2,
                         tree_indeling = dfSoortInfo,
                         local = use_local_db_export)
dim(dfTrees2)

##bomen de laatste 3 jaar
dfTrees3 <- get_treedata(conn,
                         jaar = years_3,
                         tree_indeling = dfSoortInfo,
                         local = use_local_db_export)
dim(dfTrees3)

##bomen voor trendanalyse
dfTreesTrend <- get_treedata(conn,
                             jaar = years_trend,
                             tree_indeling = dfSoortInfo,
                             local = use_local_db_export)
dim(dfTreesTrend)

### AFGELEIDE DATASETS

dfVolgorde <- dfSoortInfo %>%
  select(selectie = SoortIndeling, volgorde = SoortVolgorde) %>%
  group_by(selectie) %>%
  summarise(volgorde = min(volgorde)) %>%
  arrange(volgorde)
dim(dfVolgorde)


### UITVOEREN ANALYSES
script_path <- file.path(system.file(package = "inbobosvitaliteit"), "scripts")

#jaarlijkse analyse
source(file.path(script_path, "02_jaarlijkse_analyse.R"))

#symptomen analyse
source(file.path(script_path, "03_symptomen_analyse.R"))

#tweejaarlijkse analyse
source(file.path(script_path, "04_tweejaarlijkse_analyse.R"))

#driejaarlijkse analyse
source(file.path(script_path, "05_driejaarlijkse_analyse.R"))

#langere termijn analyse (Sen slope)
source(file.path(script_path, "06_trendanalyse_data.R"))

#langere termijn analyse (Sen slope)
source(file.path(script_path, "07a_trendanalyse_sen_bootstrap.R"))

#langere termijn analyse (Lineair model)
source(file.path(script_path, "07b_trendanalyse_lmer_bootstrap.R"))

#berekening indicator beschadigde bosbomen
source(file.path(script_path, "08_indicator_beschadigde_bosbomen.R"))
