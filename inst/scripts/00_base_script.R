###########################################################
### CONFIGURATIE Variabelen (Door gebruiker wijzigen)
###########################################################

#Maak eerst een R project aan voor het rapportagejaar
#Open Rstudio via het .Rproj bestandje

last_year         <- 2024    # laatst gebruikte jaar in de dataset
plot_base_size    <- 10      # standaard tekstgrootte in figuren (standaard 10)

# Eens de analyse succesvol gelopen is, kan alles hieronder op FALSE
# Indien niet herberekend wordt de data gehaald uit output/interim

get_data_form_db  <- TRUE    # haal data uit DB
recalc_sen        <- TRUE    # moet de SEN slopes (opnieuw) berekend worden
recalc_lmer       <- TRUE    # (her)bereken de Trend analyses
file_vlaan_europa <- "vlaanderen_europa.csv" #VL en EU jaardata (zet in workdir)

#technische configuratie (defaults mogen blijven)
sen_boot          <- 200     # sen-bootstrap samples (standaard 200)
lmer_samples      <- 10000   # iteratiestappen brm (standaard 10000)


### HULPVARIABELEN (niet wijzigen)
#-----------------------------------

years_2 <- c(last_year - 1, last_year)
years_3 <- c(last_year - 2, last_year - 1, last_year)
years_trend <- 1995:last_year
years_indicator <- 1987:last_year

setwd(getwd())    #locatie waar de bestanden moeten komen

###############################################################################
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
