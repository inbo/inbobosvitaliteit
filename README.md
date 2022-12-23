# inbobosvitaliteit
R package with functions used for the forest vitality analysis specific for INBO (not useful from outside)

#Usage

Make a new R project for the current year

Install the package with following code:

````
if (!"remotes" %in% rownames(installed.packages())) install.packages("remotes")
remotes::install_github("inbo/inbobosvitaliteit@main", dependencies = TRUE)
```` 
The @main is optional, and refers to the main branch, with @other_branch_name another branch of the repository can be installed.

All necessary scripts are in the repository, but it can be useful to copy the master script that calls all other scripts to your local project with following code (this code copies it to your current work directory:

```
inbobosvitaliteit::install_base_script()
```

Now you can run the 00_base_scripts line per line, only the most upper block of code should be updated to use the script

```
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
cat_base_script() #maak een lokale kopie van het basissscript

plot_base_size <- 10    #standaard tekstgrootte in figuren
use_local_db_export <- FALSE #gebruik reeds ingeladen data voor dit jaar
copy_local <- FALSE #optioneel: kopieer alles naar de lokale structuur

#Berekening ootstrap op sen-slope
#Eenmalig uitvoeren, duurt enkele uren
#alles wordt bewaard in de output/interim directory, dus eenmalig is genoeg
recalc_sen <- FALSE #eenmalig wel op TRUE, duurt lang
sem_boot <- 200 #hoeveelheid samples voor sen-bootstrap

###Berekening mixed effect modellen
#Eenmalig uitvoeren, duurt ongeveer 3 uur
#alles wordt bewaard in de output/interim directory, dus eenmalig is genoeg
recalc_lmer <- FALSE #eenmalig wel op TRUE, duurt een hele poos
lmer_samples <- 10000 #hoeveelheid iteratiestappen brm
``` 
