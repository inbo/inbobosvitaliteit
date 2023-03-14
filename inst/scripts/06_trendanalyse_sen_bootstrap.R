

##############################
#SEN SLOPE
##############################

####################################################################################################

### SEN SLOPE GEMIDDELD BLADVERLIES

###################################################################################################
library(rkt)




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
      cat("\n\nBerekeningen sen-slope bladverlies voor: ", i, "\n-----------------------\n")
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
    file = file.path(outdir, paste0("trend_sen_nnv_", i, ".png"))
    ggsave(plot = p, filename = file, dpi = fig_dpi, width = fig_width, height = fig_height)
  }
})
if (inherits(e, "try-error")) warning("MISLUKT: SEN GEMIDDELD BLADVERLIES")


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
      cat("\n\nBerekeningen sen-slope beschadigde bomen voor: ", i, "\n-----------------------\n")
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

    file = file.path(outdir, paste0("trend_sen_pctbeschadigd_", i, ".png"))
    ggsave(plot = p, filename = file, dpi = fig_dpi, width = fig_width, height = fig_height)
  }
})
if (inherits(e, "try-error")) warning("MISLUKT: SEN BESCHADIGD")
