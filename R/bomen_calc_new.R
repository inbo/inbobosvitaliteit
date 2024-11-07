#' @title Calculate tree analysis tables
#' @description Calculate tables for yearly and symptom analyses with grouping and statistics
#' @param x A data frame containing tree measurement data
#' @param group Vector or list of grouping variables (default: c("Jaar"))
#' @param group2 Additional grouping variables (optional)
#' @param respons Response variable for statistical calculations (optional)
#' @param uniquecount Variable name for unique count calculations (optional)
#' @param na.action Function to handle NA values (default: na.omit)
#' @export
bomen_calc <-
  function(x,
           group = c("Jaar"),
           group2 = NULL,
           respons = NULL,
           uniquecount = NULL,
           na.action = na.omit) {

    # Input validation
    #-------------------
    if (!is.data.frame(x)) {
      stop("Input 'x' must be a data frame")
    }

    #uniquecount and respons are column names
    #if not specified, call the column name "do_nothing" and
    #set the column equal to value 1
    if (is.null(uniquecount)) uniquecount <- "do_nothing"
    if (is.null(respons)) respons <- "do_nothing"
    x$do_nothing <- 1

    grouplist <- if (!is.list(group)) list(group) else group

    # Calculate total trees per year if year is present
    #-------------------------------------------------------
    dftotaalBomen <- if ("Jaar" %in% names(x)) {
      x %>%
        group_by(Jaar) %>%
        summarise(
          TotaalBomen = n_distinct(MetingKey),
          TotaalRecords = n(),
          .groups = "drop"
        )
    }

    #Loop door alle groepen heen
    #----------------------------
    rv <- NULL
    for (i in 1:length(grouplist)) {
      current_group <- grouplist[[i]]
      all_group <- ifelse(length(group2),
                          lapply(current_group, FUN = c, group2),
                          current_group)

      print(current_group)
      print(all_group)
      rv[[i]] <- x %>%
        group_by(across(all_of(all_group))) |>
        summarize(
          AantalBomen = n_distinct(MetingKey),
          AantalRecords = n(),
          AantalAantastingen = n_distinct(AantastingsKey, na.rm = TRUE),
          AantalUnique = ifelse(all(is.na(!!sym(uniquecount))),
                                NA,
                                n_distinct(!!sym(uniquecount), na.rm = TRUE)),
          mean_value = ifelse(all(is.na(!!sym(respons))),
                              NA,
                              mean(!!sym(respons))),
          sd = ifelse(all(is.na(!!sym(respons))),
                      NA,
                      sd(!!sym(respons))),
          se = ifelse(all(is.na(!!sym(respons))),
                      NA,
                      sd(!!sym(respons)) / sqrt(n())),
          median_value = ifelse(all(is.na(!!sym(respons))),
                                NA,
                                median(!!sym(respons))),
          .groups = "drop") %>%
        group_by(across(all_of(current_group))) %>%
        #na.action() %>%
        mutate(
          PctBomen = AantalBomen / sum(AantalBomen) * 100,
          PctRecords = AantalRecords / sum(AantalRecords) * 100,
          PctUnique = AantalUnique / sum(AantalUnique) * 100,
          Set = paste(current_group, collapse = "."))
      print(rv[[i]])
    }

    #bind alle bekomen datasets samen,
    bc <- bind_rows(rv)
    if ("Jaar" %in% names(bc)) {
      bc <-
        left_join(bc, dftotaalBomen, by = "Jaar") %>%
        mutate(
          PctOfTotaalBomen = AantalBomen / TotaalBomen * 100,
          PctOfTotaalRecords = AantalRecords / TotaalRecords * 100)
    }

    # Handle species-related columns
    bc <- bc %>%
      mutate(
        Soort = if (!"Soort" %in% names(.)) NA else Soort,
        SoortIndeling = if (!"SoortIndeling" %in% names(.)) NA else SoortIndeling,
        SoortType = if (!"SoortType" %in% names(.)) NA else SoortType
      ) %>%
      mutate(
        selectie = case_when(
          !is.na(Soort) ~ as.character(Soort),
          !is.na(SoortIndeling) ~ as.character(SoortIndeling),
          !is.na(SoortType) ~ as.character(SoortType),
          TRUE ~ "totaal"
        )
      )

    # Clean up optional columns
    if (is.null(uniquecount)) {
      bc <- select(bc, -c(AantalUnique, PctUnique))
    }
    if (is.null(respons)) {
      bc <- select(bc, -c(mean_value, median_value, sd, se))
    }
    if (all(is.na(x$AantastingsKey))) {
      bc <- select(bc, -AantalAantastingen)
    }

    bc
  }
