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
#' @param sen_seed chosen seed for the sen calculations, when empty a random seed is chosen
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
           sen_seed = NULL
  ) {

    if (is.null(sen_seed)) sen_seed <- sample(1:1000000,1)
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
    sen_seed <<- sen_seed

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
