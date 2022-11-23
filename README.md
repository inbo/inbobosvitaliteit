# inbobosvitaliteit
R package with functions used for the forest vitality analysis specific for INBO (not useful from outside)

#Usage

Use the functionality of this library by using the start_script 00_base_script.R found in this repository under inst/scripts

Or when using the package, load this script to your own computer by following command:

````
 fp <- file.path(system.file(package = "inbobosvitaliteit"), 
                "scripts", 
                "00_base_script.R")
 
 file.copy(fp, to = file.path(getwd()))
 ````
