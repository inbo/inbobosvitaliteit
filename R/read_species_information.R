#' Read species metadata
#'
#' @param path locatie van de csv die de gegevens bevat, standaard "package" wat betekent dat de csv vanuit het package gehaald wordt. Indien je een custom indeling wil gebruiken zorg dat het een csv file is met de kolommen species, species_main_cat, species_sub_cat, species_order
#' @importFrom dplyr rename
#' @importFrom readr read_csv2
#'
#' @return data.frame with species grouping information
#' @export
#'
read_species_information <-
  function(path = "package") {
    if (path == "package")
      path <- file.path(system.file(package = "inbobosvitaliteit"),
                        "extdata",
                        "tree_reportmetadata.csv")
    rv <- read_csv2(path) %>%
      rename(Soort = .data$species,
             SoortType = .data$species_main_cat,
             SoortIndeling = .data$species_sub_cat,
             SoortVolgorde = .data$species_order)
    return(rv)
  }
