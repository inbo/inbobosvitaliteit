#' Calculate sen slope on tree data
#'
#' @param data the data to base the calculations on
#' @param sen_boot the amount of bootstrap samples for the confidence intervals. When 0 no bootstrap is performed and the values are returned as is
#'
#' @return data.frame with sen-slope results
#' @importFrom rkt rkt
#' @importFrom lme4 lmer
#' @importFrom stats quantile
#' @export
#'
calc_sen_slope <- function(data, sen_boot = 0){
  if (sen_boot < 1) {
    test <- rkt::rkt(date = data[["Jaar"]],
                     y = data[["mean_value"]],
                     block = data[["PlotNr"]])
    lmerfit <- summary(lmer(mean_value ~ Jaar + (1|.data$PlotNr),
                            data = data))$coef[,1]
    return(
      data.frame(
        sen_int1 = median(data[["mean_value"]] - test$B * data[["Jaar"]]),
        sen_slope1 = test$B,
        tau = test$tau,
        lmer_intercept = lmerfit[1],
        lmer_slope = lmerfit[2]))
  }
  else {
    senslope <- numeric(sen_boot)
    senintercept <- numeric(sen_boot)
    allplots <- unique(data[["PlotNr"]])
    for (i in 1:sen_boot) {
      print(paste0(i/sen_boot*100,"%"))
      randplots <- sample(allplots,
                          size = length(allplots),
                          replace = TRUE)
      bd <- NULL
      for (j in 1:length(randplots)) {
        bd <- rbind(bd, (data %>%
                           filter( .data$PlotNr == randplots[j]) %>%
                           mutate(PlotNr = j)))
      }
      senslope[i] <- rkt::rkt(bd[["Jaar"]],
                              y = bd[["mean_value"]],
                              block = bd[["PlotNr"]])$B

      senintercept[i] <-
        median(bd[["mean_value"]] - senslope[i] * bd[["Jaar"]])
    }
    return(
      data.frame(
        Nboot = sen_boot,
        sen_intercept = mean(senintercept),
        sen_slope = mean(senslope),
        sen_lcl = quantile(senslope, probs = 0.025, na.rm = TRUE),
        sen_ucl = quantile(senslope, probs = 0.975, na.rm = TRUE)))
  }
}

