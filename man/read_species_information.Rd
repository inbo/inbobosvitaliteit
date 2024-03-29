% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_import_functions.R
\name{read_species_information}
\alias{read_species_information}
\title{Read species metadata}
\usage{
read_species_information(path = "package")
}
\arguments{
\item{path}{locatie van de csv die de gegevens bevat, standaard "package" wat betekent dat de csv vanuit het package gehaald wordt. Indien je een custom indeling wil gebruiken zorg dat het een csv file is met de kolommen species, species_main_cat, species_sub_cat, species_order}
}
\value{
data.frame with species grouping information
}
\description{
Read species metadata
}
