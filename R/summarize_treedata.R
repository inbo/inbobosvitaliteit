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
