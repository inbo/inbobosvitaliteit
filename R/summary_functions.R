
######################################################

#' Find number of symptoms in the whole dataset
#'
#' @param trees dataset with the tree information
#' @param symptoms dataset with the symptom information
#'
#' @return data.frame containng the number of symptoms per tree
#' @export
#'
get_symtomsummary <- function(trees, symptoms) {
  str(trees$MetingKey)
  str(symptoms$MetingKey)
  df <-
    left_join(trees, symptoms, by = c("MetingKey")) %>%
    group_by(.data$WaarnemingKey, .data$Jaar, .data$PlotNr, .data$BoomNr)

  dfsum <-
    summarize(df,
              AantalSymptomen = sum(!is.na(.data$SymptoomCode)))
  dfsum
}

################################################

#' Summarise tree data by predefined groups
#'
#' @param data tree information data
#' @param indiv_id column name where to find the tree ID
#' @param meas_id column name with the measurement ID
#' @param detail_id column name with the symptom detail ID
#' @param response  column name of the response variable
#' @param count_unique column name with the unique counts
#' @param groups0 first grouping variable
#' @param groups1 second grouping variable
#' @param groups2 third grouping variable
#' @importFrom dplyr bind_cols n_distinct
#'
#' @return dataframe with summarized tree information
#' @export
summarize_treedata <-
  function(data, indiv_id, meas_id, detail_id = NULL,
          response = NULL, count_unique = NULL,
          groups0 = NULL, groups1 = NULL, groups2 = NULL) {

  if (is.null(detail_id)) {
    data$.detail <- NA
    detail_id <- ".detail"
  }
  if (is.null(response)) {
    data$.response <- NA
    response_id <- ".response"
  }
  if (is.null(count_unique)) {
    data$.count_unique <- NA
    count_unique <- ".count_unique"
  }

  #error control on input arguments
  if (!inherits(data, "data.frame"))
    stop("data should be a data.frame or tibble")
  if (!(indiv_id %in% colnames(data)))
    stop("indiv_id should be a column name in data")
  if (!(detail_id %in% colnames(data)))
    stop("detail_id should be a column name in data")
  if (!(meas_id %in% colnames(data)))
    stop("meas_id should be a column name in data")
  if (!(meas_id %in% colnames(data)))
    stop("response should be a column name in data")
  if (!(all(groups0 %in% colnames(data))))
    stop("all vector elements in groups0 should exist as column names in data")
  if (!(all(groups1 %in% colnames(data))))
    stop("all vector elements in groups1 should exist as column names in data")
  if (!all(groups2 %in% colnames(data)))
    stop("all elements in groups2 should exist as column names in data")

  #use fixed names
  df <- bind_cols(
    data.frame(.id = data[[indiv_id]],
               .meas = data[[meas_id]],
               .detail = data[[detail_id]],
               .response = data[[response]],
               .count_unique = data[[count_unique]]),
    data[groups0],
    data[groups1],
    data[groups2])

  #elementary grouping using groups0
  df0 <- df %>%
    group_by_at(.data$groups0) %>%
    summarize(tot_n_records = n(),
              tot_n_unique = n_distinct(.data$.id))

  #grouping using groups0, groups1 en groups2
  df1 <- df %>%
    group_by_at(c(.data$groups0, .data$groups1, .data$groups2)) %>%
    summarize(n_records = n(),
              n_unique_id = n_distinct(.data$.id),
              n_unique_meas = n_distinct(.data$.meas),
              n_unique_detail = n_distinct(.data$.detail),
              n_unique_vals = n_distinct(.data$.count_unique),
              mean_response = mean(.data$.response),
              sd_response = sd(.data$.response),
              se_response = .data$sd_response / sqrt(.data$n_records),
              median_response = median(.data$.response))

  #groepeer opnieuw maar nu enkel op groups0 en groups1, om percentages te kunnen berekenen over groups2
  df2 <- df1 %>%
    group_by_at(c(groups0, groups1)) %>%
    mutate(pct_records = .data$n_records / sum(.data$n_records),
           pct_id = .data$n_unique_vals / sum(.data$n_unique_vals),
           pct_unique = .data$n_unique_vals / sum(.data$n_unique_vals),
           set = paste(c(.data$groups0, .data$groups1, .data$groups2),
                       collapse = "."))

  df2 <- df2 %>%
    left_join(df0, by = groups0) %>%
    mutate(pct_of_all_ids = .data$n_unique_id / .data$tot_n_unique * 100,
           pct_of_all_records = .data$n_records / .data$tot_n_records * 100)

  df2
}

################################################################

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

#######################################################################

#' Wilcoxon test to compare 2 groups from the tree data
#'
#' @param data data for the calculations
#' @param formula the wilcoxon formulat to use
#' @param alphas the confidence limits to calculate
#' @param paired flag to set the data paired (in conjunction with | in the formula)
#' @param ... additional parameters
#' @importFrom dplyr select mutate_if
#' @importFrom tidyr spread
#' @importFrom stats wilcox.test
#'
#' @return data.frame with wilcoxon results
#' @export
#'
wilcox_table <- function(data,
                         formula = BladverliesNetto ~ LeeftijdsklasseEur,
                         alphas = c(0.05,0.01,0.001),
                         paired = TRUE,
                         ...){
  . <- NULL #om warnings te voorkomen

  #Indien er een variabele na | staat,
  #wordt een gepaarde test uitgevoerd op deze variabele

  #geen gepaarde test
  if (length(formula[[3]]) == 1) {
    paired <- FALSE
    datatab <- select(data, !!formula[[2]], !!formula[[3]])
    datatab <-
      mutate_if(datatab, sapply(datatab, is.factor), as.character) %>%
      group_by(!!formula[[3]])

    wt <- try(wilcox.test(formula = formula,
                          data = data,
                          paired = paired,...),
              silent = TRUE)
  #gepaarde test
  } else {
    paired <- TRUE
    datatab <-
      select(data,
             !!formula[[2]],
             !!formula[[3]][[2]],
             !!formula[[3]][[3]])
    datatab <-
      mutate_if(datatab, sapply(datatab, is.factor), as.character) %>%
      group_by((!!formula[[3]][[2]]))

    datapair <- spread(datatab,
                       key = !!formula[[3]][[2]],
                       value = !!formula[[2]])
    wt <- try(wilcox.test(datapair[[2]],
                          datapair[[3]],
                          paired = paired, ...),
              silent = TRUE)
  }

  #verzamel de waarden van de wilcoxon test
  if (inherits(wt, "try-error")) {
    W <-  NA; pval <- NA; pind <- NA; paired <- paired
  } else {
    W = wt$statistic
    pval <- wt$p.value
    pind <- ifelse(pval > alphas[1],
                   "n.s.",
                   ifelse(pval > alphas[2],
                          "*",
                          ifelse(pval  > alphas[3],
                                 "**",
                                 "***")))
    paired <- paired
  }

  #statistische eigenschappen van de data (zonder testresultaten)
  sumdata <-
    datatab %>%
    summarize(N = n(),
              mean_value = mean(!!formula[[2]]),
              sd = sd(!!formula[[2]]),
              se = sd(!!formula[[2]])/sqrt(n()),
              median_value = median(!!formula[[2]]))

  #er staat nu een rij per groep, nu zetten we deze samen
  #indien er slechts 1 groep bestond, wordt alles van de tweede groep op NA gezet

  if(nrow(sumdata) == 2) {
    grp1 <- sumdata[1,1]
    grp2 <- sumdata[2,1]
    namen <- names(sumdata)[-1] #eerste naam is de groep
    kolommen <- ncol(sumdata) - 1 #eerste kolom zal wegvallen
    namen <- paste(rep(c(grp1, grp2),
                       rep(kolommen, 2)),
                   rep(namen,2),
                   sep = "_")

    tabeldata <- cbind(sumdata[1,-1], sumdata[2,-1])
    names(tabeldata) <- namen

    tabeldata %>%
      mutate(W = W,
             verschil = .[[7]] - .[[2]],
             p.value = pval,
             signif = pind,
             paired = paired)

  } else {
    grp1 <- sumdata[1,1]
    namen <- names(sumdata)[-1]
    kolommen <- ncol(sumdata) - 1
    namen <- paste(grp1, namen, sep = "_")
    tabeldata <- sumdata[1,-1]
    names(tabeldata) <- namen

    tabeldata %>%
      mutate(W = W,
             verschil = NA,
             p.value = pval,
             signif = pind,
             paired = paired)

  }
}

################################################################################
