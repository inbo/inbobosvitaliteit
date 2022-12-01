####################################################################################################

### SEN SLOPE GEMIDDELD BLADVERLIES

###################################################################################################

e <- try({
  set.seed(sen_seed)

  #dfSenResult bevat de samenvattende tabel per jaar en per plotnummer

  #deze worden als basis gebruikt voor de  sen slope uit het rkt package
  dfSenResult <- bomen_calc(x = dfTreesTrend,
                            group = lapply(normal_groups, c, "PlotNr"),
                            respons = "BladverliesNetto")


  my_rkt <- function(data){
    model <- rkt(date = data$Jaar, y = data$gemNNV, block = data$PlotNr)
    data.frame(sen_slope = model$B, kendall_tau = model$tau, p_value = model$sl)
  }

  (sen_slopes <- dfSenResult %>%
      select(Jaar, gemNNV = mean_value, PlotNr, selectie) %>%
      nest(data = c(Jaar, gemNNV, PlotNr)) %>%
      mutate(senresult = map(.x = data, .f = my_rkt)) %>%
      select(-data) %>%
      unnest(cols = senresult)) %>%
    write.csv2(file = file.path(outdir, "trend_senslopes_rkt.csv"))



  #Bereken de sen-slope bootstrap via de functie pred_sen_slope (functies_trend.R)
e <- try({
  #recalc_sen <- TRUE
  if (recalc_sen) {
    dfSen <- NULL
    for (i in unique(dfSenResult$selectie)) {
      sendata <- dfSenResult %>%
        filter(selectie == i) %>%
        ungroup() %>%
        transmute(selectie, Jaar = Jaar - meerjaarlijks[1], PlotNr, mean_value)
      print(i)
      print(dim(sendata))
      tmp <- pred_sen_slope(sendata, sen_boot = sen_boot)
      tmp$Jaar <- tmp$Jaar + meerjaarlijks[1]
      print(head(tmp))
      dfSen[[i]] <- tmp
    }
    if(!dir.exists(file.path(outdir, "interim")))
      dir.create(file.path(outdir, "interim"))
    save(dfSen, file = file.path(outdir,
                                 "interim",
                                 paste0("dfSen_trend_nnv", sen_boot, ".Rdata")))
  } else {
    load(file =  file.path(outdir, "interim",
                           paste0("dfSen_trend_nnv", sen_boot, ".Rdata")))
  }

  ###

  ymax <- min(40,max(dfSenResult$mean_value + 2 * dfSenResult$se))
  for (i in unique(dfSenResult$selectie)) {
    plotdata <- dfSen[[i]]
    p <-
      ggplot(plotdata, aes(x = Jaar, y = mean_year,
                           ymin = mean_lcl, ymax = mean_ucl)) +
      geom_point() + geom_errorbar() +
      geom_line(aes(y = fit), color = INBOtheme::inbo_groen) +
      geom_ribbon(aes(ymin = boot_lcl, ymax = boot_ucl),
                  alpha = 0.3, fill = INBOtheme::inbo_groen) +
      xlab("Jaar") + ylab("Bladverlies (%) Sen-slope") +
      ggtitle(i) + ylim(0,ymax)
    print(p)
    file = file.path(outdir, paste0("trend_nnv_", i, ".png"))
    ggsave(plot = p, filename = file, dpi = fig_dpi, width = fig_width, height = fig_height)
  }
})
if (inherits(e, "try-error")) stop("MISLUKT: SEN GEMIDDELD BLADVERLIES")


###########

e <- try({
  #Percentage beschadigde bomen   ==> Lijkt niet OK, helling altijd 0

  #recalc_sen <- TRUE
  n_sen_boot <- sen_boot
  if (recalc_sen) {
    dfSenB <- NULL
    for (i in unique(dfTrendBeschadigd$selectie)) {
      sendataB <- dfTrendBeschadigd %>%
        filter(selectie == i) %>%
        ungroup() %>%
        transmute(selectie, Jaar = Jaar - meerjaarlijks[1],
                  PlotNr, mean_value = PctBeschadigd)
      print(i)
      print(dim(sendataB))
      tmp <- NULL
      try({
      tmp <- pred_sen_slope(sendataB, sen_boot = n_sen_boot)
      tmp$Jaar <- tmp$Jaar + meerjaarlijks[1]
      print(head(tmp))
      dfSenB[[i]] <- tmp
      })
    }
    save(dfSenB, file =  file.path(outdir, "interim", paste0("dfSenbesch_trend_nnv", n_sen_boot, ".Rdata")))
  } else {
    load(file =  file.path(outdir, "interim", paste0("dfSenbesch_trend_nnv", n_sen_boot, ".Rdata")))
  }


  ##load(file = "dfSen_trend_beschadigd200.Rdata")


  ymax <- min(100,max(dfSenResult$mean_value + 2 * dfSenResult$se))
  for (i in unique(dfSenResult$selectie)) {
    plotdata <- dfSenB[[i]]
    p <-
      ggplot(plotdata, aes(x = Jaar, y = mean_year, ymin = pmax(0,mean_lcl), ymax = pmin(100,mean_ucl))) +
      geom_point() + geom_errorbar() +
      geom_line(aes(y = fit), color = INBOtheme::inbo_groen) +
      geom_ribbon(aes(ymin = boot_lcl, ymax = boot_ucl),
                  alpha = 0.3, fill = INBOtheme::inbo_groen) +
      xlab("Jaar") + ylab("Aandeel Beschadigd (%)") + ggtitle(i) + ylim(0,ymax)
    print(p)

    file = file.path(outdir, paste0("trend_pctbeschadigd_", i, ".png"))
    ggsave(plot = p, filename = file, dpi = fig_dpi, width = fig_width, height = fig_height)
  }
})
if (inherits(e, "try-error")) stop("MISLUKT: SEN BESCHADIGD")
