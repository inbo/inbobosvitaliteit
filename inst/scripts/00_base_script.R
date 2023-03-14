
### DOOR GEBRUIKER TE WIJZIGEN: configuration variables

#Maak eerst een R project aan voor het rapportagejaar
#Open Rstudio via het .Rproj bestandje

#check de werkdirectory
getwd()
library(inbobosvitaliteit)

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

################################################################################
### CONFIG EN IMPORT
################################################################################

### UITVOEREN ANALYSES (ALTIJD UITVOEREN)
script_path <- file.path(system.file(package = "inbobosvitaliteit"), "scripts")

### CONFIGURATIE EN IMPORT (ALTIJD UITVOEREN)
source(file.path(script_path, "00_configuratie_en_data_import.R"))

################################################################################
### ANALYSES (iedere file kan onafhankelijk uitgevoerd worden)
################################################################################

#jaarlijkse analyse
source(file.path(script_path, "01_jaarlijkse_analyse.R"))

#symptomen analyse
source(file.path(script_path, "02_symptomen_analyse.R"))

#tweejaarlijkse analyse
source(file.path(script_path, "03_tweejaarlijkse_analyse.R"))

#driejaarlijkse analyse
source(file.path(script_path, "04_driejaarlijkse_analyse.R"))

#langere termijn analyse (Sen slope)
source(file.path(script_path, "05_trendanalyse_data.R"))

#langere termijn analyse (Sen slope)
source(file.path(script_path, "06_trendanalyse_sen_bootstrap.R"))

#langere termijn analyse (Lineair model)
source(file.path(script_path, "07_trendanalyse_lmer_bootstrap.R"))

#berekening indicator beschadigde bosbomen
source(file.path(script_path, "08_indicator_beschadigde_bosbomen.R"))
