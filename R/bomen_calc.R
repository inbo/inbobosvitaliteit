#' Bereken de tabellen voor het meeste van de jaarlijkse en symptoomanalyses
#'
#' @param x dataset met minstens de variabele MetingKey en AantastingsKey waarop de berekeningen gebeuren.
#' MetingKey indentificeert unieke bomen per jaar, AantastingsKey identificeert aantastingen uniek, zodat aantastingen met meerdere specificaties of oorzaken niet dubbel geteld worden
#' @param group een vector of lijst met vectoren voor de primaire groepering als basis voor percentageberekenig
#' @param group2 laat nog extra groepering toe, is meestal de laatste categorische variabele, waarover we de percentages willen weten, dus dit is meestal een variabele met x categorieën, en het percentage per categorie wordt berekend
#' @param respons indien ingevuld wordt voor deze variabele de mean, median, sd en se berekend
#' @param uniquecount indien ingevuld wordt per groep (group+group2) geteld hoeveel unieke waarden deze variabele heeft
#' @param na.action functie die aangeeft wat er moet gebeuren met de missings. na.omit is de logische default
#'
#' @return een tabel met per combinatie van groepen (in group meegegeven) een inschatting van het aantal bomen,
#' aantal records, unieke waarden en de overeenkomstige percentages, eventueel nog aangevuld met statistieken op een responsvariabele (mean, se, sd, mediaan)
#' @export
#' @importFrom stats na.omit
#' @importFrom dplyr %>% group_by_at vars mutate n bind_rows filter
#' @importFrom rlang .data syms
#' @importFrom stats sd median
#'
bomen_calc <-
  function(x,
           group = c("Jaar"),
           group2 = NULL,
           respons = NULL,
           uniquecount = NULL,
           na.action = na.omit) {

    #Om in R te kunnen quoten moet de variabele een string zijn, NULL is niet toegalaten, daarom maken we een dummy
    if (is.null(uniquecount)) uniquecount = "do_nothing"
    if (is.null(respons)) respons = "do_nothing"
    x$do_nothing <- 1
    if (is.null(x$AantastingsKey)) x$AantastingsKey <- NA

    #group kan als 1 vector met groeperingen doorgegeven worden,
    #ofwel als een lijst van alle groeperingen die gebruikt worden
    if (!is.list(group))
      grouplist <- list(group)
    else
      grouplist <- group

    if ("Jaar" %in% names(x)) {
      dftotaalBomen <- group_by(x, .data$Jaar) %>%
        summarize(TotaalBomen = length(unique(.data$MetingKey)),
                  TotaalRecords = length(.data$MetingKey))
    }

    #Loop door alle groepen heen
    rv <- NULL
    for (i in 1:length(grouplist)) {
      group <- grouplist[[i]]

      rv[[i]] <-
        group_by_at(x, vars(c(group, group2))) %>%
        summarize(AantalBomen = length(unique(.data$MetingKey)),
                  AantalRecords = length(.data$MetingKey),
                  AantalAantastingen = length(unique(.data$AantastingsKey)),
                  AantalUnique = ifelse(is.null(uniquecount),
                                        NA,
                                        length(unique(!!!syms(uniquecount)))),
                  mean_value = ifelse(is.null(respons),
                                      NA,
                                      mean(.data[[respons]])),
                  sd = ifelse(is.null(respons),
                              NA,
                              ifelse(is.na(sd(.data[[respons]])),
                                     0,
                                     sd(.data[[respons]]))), # NaN vermijden
                  se = ifelse(is.null(respons),
                              NA,
                              ifelse(is.na(sd(.data[[respons]])),
                                     0,
                                     sd(.data[[respons]])/sqrt(n()))),
                  median_value = ifelse(is.null(respons),
                                        NA,
                                        median(.data[[respons]]))) %>%
        group_by_at(vars(group)) %>%
        na.action() %>%
        mutate(
          PctBomen = .data$AantalBomen / sum(.data$AantalBomen) * 100,
          PctRecords = .data$AantalRecords / sum(.data$AantalRecords) * 100,
          PctUnique = .data$AantalUnique / sum(.data$AantalUnique) * 100,
          Set = paste(group, collapse = "."))
      print(rv[[i]])
    }
    #bind alle bekomen datasets samen,
    #en sorteer de kolommen zodat de groepvariabelen eerst komen
    bc <- bind_rows(rv)
    if ("Jaar" %in% names(bc)) {
      bc <-
        left_join(bc, dftotaalBomen, by = "Jaar") %>%
        mutate(
          PctOfTotaalBomen = .data$AantalBomen / .data$TotaalBomen * 100,
          PctOfTotaalRecords = .data$AantalRecords / .data$TotaalRecords * 100)
    }

    gvars <- unique(c(unlist(grouplist), group2))
    gvars2 <- gvars[-length(gvars)]
    gvars2 <- gvars2[gvars2 != "Jaar"]

    if (is.null(bc$Soort)) bc$Soort <- NA
    if (is.null(bc$SoortIndeling)) bc$SoortIndeling <- NA
    if (is.null(bc$SoortType)) bc$SoortType <- NA

    bc <- mutate(bc,
                 selectie =
                   ifelse(!is.na(.data$Soort),
                          as.character(.data$Soort),
                          ifelse(!is.na(.data$SoortIndeling),
                                 as.character(.data$SoortIndeling),
                                 ifelse(!is.na(.data$SoortType),
                                        as.character(.data$SoortType),
                                        "totaal"))))

    if (uniquecount == "do_nothing") {
      bc$AantalUnique <- bc$PctUnique <- NULL
    }
    if (respons == "do_nothing") {
      bc$mean_value <- bc$median_value <- bc$sd <- bc$se <- NULL
    }
    if(all(is.na(x$AantastingsKey))) {
      bc$AantalAantastingen <- NULL
    }
    bc
  }
