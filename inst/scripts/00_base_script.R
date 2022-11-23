
### DOOR GEBRUIKER TE WIJZIGEN: configuration variables

last_year <- 2022       #laatst gebruikte jaar in de dataset
setwd("C:/bos/2023")    #locatie waar de bestanden moeten komen
plot_base_size <- 10    #standaard tekstgrootte in figuren

#Kopieer de scripts naar de scripts directory en werk hiermee
local_script_copy <- FALSE

#Lees de bestanden uit een eerdere database export met deze scripts
use_local_db_export <- FALSE

### ENKEL AANPASSEN INDIEN PROBLEMEN EN DE SCRIPTS GEWIJZIGD ZIJN

if (!"remotes" %in% rownames(installed.packages())) install.packages("remotes")
remotes::install_github("inbo/inbobosvitaliteit", dependencies = TRUE)

#load packages
library(tidyverse)
library(inbobosvitaliteit)
library(DBI)
install_necessary_packages()

### Laden van databank en voorbereidingen
conn <- bosvitaliteit_connect()
generate_file_structure()
generate_tree_species_data()
generate_sql_files()

### uitvoeren scripts
init_session(last_year, first_year = 1987, first_multiyear = 1995,
             connect_via_db = !use_local_db_export )
theme_set(INBOtheme::theme_inbo(plot_base_size))
(script_path <- get_script_location(local = local_script_copy,
                                   target = 'scripts'))

#werken met de scripts uit het inbobosvitaliteitspackage
#lees de data in
source(file.path(script_path, "01_data_import.R"))

#jaarlijkse analyse
source(file.path(script_path, "02_jaarlijkse_analyse.R"))

#symptomen analyse
source(file.path(script_path, "03_symptomen_analyse.R"))

#tweejaarlijkse analyse
source(file.path(script_path, "04_tweejaarlijkse_analyse.R"))

#driejaarlijkse analyse
source(file.path(script_path, "05_driejaarlijkse_analyse.R"))

#langere termijn analyse (Sen slope)
source(file.path(script_path, "06a_trendanalyse_sen.R"))

#langere termijn analyse (Lineair model)
source(file.path(script_path, "06b_trendanalyse_lmer.R"))








