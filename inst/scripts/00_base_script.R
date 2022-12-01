
### DOOR GEBRUIKER TE WIJZIGEN: configuration variables

#Maak eerst een R project aan voor het rapportagejaar
#Open Rstudio via het .Rproj bestandje

#check de werkdirectory
getwd()

last_year <- 2022       #laatst gebruikte jaar in de dataset
years_2 <- c(last_year - 1, last_year)
years_3 <- c(last_year - 2, last_year - 1, last_year)
years_trend <- 1995:2022
setwd(getwd())    #locatie waar de bestanden moeten komen

plot_base_size <- 10    #standaard tekstgrootte in figuren
use_local_db_export <- FALSE #gebruik reeds ingeladen data voor dit jaar
copy_local <- FALSE #optioneel: kopieer alles naar de lokale structuur
recalc_sen <- FALSE #eenmalig wel op TRUE, duurt lang
recalc_lmer <- FALSE #eenmalig wel op TRUE, duurt een hele poos

#######################################################################

#Hieronder enkel wijzigen indien er problemen zijn

### INIT SESSIE
#---------------

### installatie en laden packages

if (!"remotes" %in% rownames(installed.packages())) install.packages("remotes")
remotes::install_github("inbo/inbobosvitaliteit", dependencies = TRUE)

inbobosvitaliteit::install_necessary_packages()

library(tidyverse)
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
                              local = use_local_db_export)
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
source(file.path(script_path, "07a_trendanalyse_sen.R"))

#langere termijn analyse (Lineair model)
source(file.path(script_path, "07b_trendanalyse_lmer.R"))








