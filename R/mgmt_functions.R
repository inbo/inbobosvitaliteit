
#' Installeer benodigde packages
#'
#' @return installed libraries
#' @importFrom utils install.packages installed.packages
#' @importFrom remotes install_github
#' @export
#'
install_necessary_packages <- function() {
  #benodigde libraries
  packages <- c("here", "odbc", "DBI", "tidyverse", "rkt", "rlang", "lme4", "remotes")
  install.packages(setdiff(packages, rownames(installed.packages())))

  #benodigde libraries vanuit github
  install_github("inbo/INBOmd", dependencies = TRUE)
  install_github("inbo/INBOtheme", dependencies = TRUE)
}

##########################################################

#' Get script location (using a local copy or not)
#'
#' @param local do you want a local copy on your hard disk to work from
#' @param target target path when a local copy is made
#'
#' @return path to script location
#' @export
get_script_location <- function(local = FALSE, target = "scripts") {
  if (!local) {
    script_path <-
      file.path(system.file(package = "inbobosvitaliteit"), "scripts")
    return(script_path)
  }
  #else
  if (!dir.exists(target)) dir.create(target)
  files <- list.files(system.file(package = "inbobosvitaliteit", "scripts"),
                      full.names = TRUE)
  for (fp in files) {
    file.copy(fp, to = file.path(getwd(), target))
  }
  script_path <- target #script directory on local hard drive
  return(script_path)
}

#######################################################################

#' Generate the necessary folders for the project exports
#'
#' @param root directory where the file structure should start from
#'
#' @return directories created on the file system
#' @export
#'
generate_file_structure <- function(root = getwd()) {
  if (!dir.exists("data")) dir.create("data")
  if (!dir.exists("output")) dir.create("output")
  if (!dir.exists("scripts")) dir.create("scripts")
}

###################################################################

#' Functie om de instelvariabelen te zetten om de scripts te kunnen runnen
#'
#' @param year laatste datajaar van de analyse
#' @param first_year Eerste jaar die meegenomen wordt de heel globale figuren zoals voor de natuurindicatoren
#' @param first_multiyear Eerste jaar die meegenomen wordt voor de meerjaarlijkse analyses
#' @param connect_via_db haal de data uit de db indien TRUE, anders gebruik lokale files die eerder geïmporteerd werden
#' @param outdir output directory relative to working directory
#' @param fig_width standaard figuurbreedte in inch
#' @param fig_height standaard figuurhoogote in inch
#' @param fig_dpi standaard resolutie voor de figuren
#' @param sen_boot aantal bootstrap samples om betrouwbaarheidsintervallen op de sen slope te bepalen, indien 0 dan wordt geen bootstrap uitgevoerd
#' @param lmer_boot aantal bootstrap samples om  betrouwbaarheidsintervallen voor de lineaire modellen te bepalen, indien 0 dan wordt geen bootstrap uitgevoerd
#'
#' @return maakt verschillende golbale variabelen aan: jaarkeuze, pathkeuze, tweejaarlijks, driejaarlijks, meerjaarlijks, jaren_natuurindicatoren, outdir, connect_via_db, normal_groups, all_groups, extended_groups, groups_multiyear, extra_groups
#' @export
#'
init_session <-
  function(year,
           first_year = 1987,
           first_multiyear = 1995,
           connect_via_db = TRUE,
           outdir = "output",
           fig_width = 7,
           fig_height = 5,
           fig_dpi = 300,
           sen_boot = 200,
           lmer_boot = 200) {

    #globale variabelen
    fig_width <<- fig_width
    fig_height <<- fig_height
    fig_dpi <<- fig_dpi
    jaarkeuze <<- year
    jaarkeuze <- year #for removing NOTE in package compilation
    tweejaarlijks <<- tweejaarlijks <- c(jaarkeuze-1, jaarkeuze)
    driejaarlijks <<- driejaarlijks <- c(jaarkeuze-2, jaarkeuze-1, jaarkeuze)
    meerjaarlijks <<- meerjaarlijks <- first_multiyear:jaarkeuze
    connect_via_db <<- connect_via_db <- connect_via_db
    outdir <<- outdir
    jaren_natuurindicatoren <<- jaren_natuurindicatoren <- first_year:jaarkeuze

    #extra variabelen (created globally)

    normal_groups <<- normal_groups <-
      list(c("Jaar"),
           c("Jaar", "SoortType"),
           c("Jaar", "SoortIndeling"))
    all_groups <<- all_groups <-
      list(c("Jaar"),
           c("Jaar", "LeeftijdsklasseEur"),
           c("Jaar", "SoortType"),
           c("Jaar", "SoortIndeling"),
           c("Jaar", "LeeftijdsklasseEur", "SoortType"),
           c("Jaar", "LeeftijdsklasseEur", "SoortIndeling"))
    extended_groups <<- extended_groups <-
      list(c("Jaar"),
           c("Jaar", "SoortType"),
           c("Jaar", "SoortIndeling"),
           c("Jaar", "Soort"))
    groups_multiyear <<- groups_multiyear <-
      list(c("Jaar"),
           c("Jaar", "LeeftijdsklasseEur"),
           c("Jaar", "SoortType"),
           c("Jaar", "SoortType", "LeeftijdsklasseEur"),
           c("Jaar", "SoortIndeling"))

    extra_groups <<- extra_groups <-
      list(c("Jaar"),
           c("Jaar", "LeeftijdsklasseEur"),
           c("Jaar", "SoortType"),
           c("Jaar", "SoortType", "LeeftijdsklasseEur"),
           c("Jaar", "SoortIndeling"))
    invisible()
  }
