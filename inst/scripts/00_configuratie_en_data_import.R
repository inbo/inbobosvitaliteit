

### INIT SESSIE
#---------------

### installatie en laden packages

# if (!"remotes" %in% rownames(installed.packages())) install.packages("remotes")
# remotes::install_github("inbo/inbobosvitaliteit", dependencies = TRUE)

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

### FORMAAT VLAANDEREN_EUROPA

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

