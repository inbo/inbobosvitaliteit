

##############
### 3 jaar ###
##############


### AFGELEIDE DATASETS
e <- try({
  #! Toevoegen prbo aan driejaarlijkse data
  dfTrees3 <-
    dfTrees3 %>%
    mutate(prbo = paste0(PlotNr, BoomNr))

  #gemeenschappelijke bomen over 3 jaar
  gemeenschappelijk3j <-

    dfTrees3Gmsch <-
    inner_join(dfTrees3,
               dfTrees3 %>% group_by(prbo) %>%
                 summarize(aantal = n()) %>%
                 filter(aantal == 3),
               by = "prbo")

  #Berekening totalen 3-jaarlijks gemeenschappelijk
  dfTotaalBomen3J <-
    bomen_calc(dfTrees3Gmsch, normal_groups) %>%
    select(selectie, Jaar, TotaalAantalBomen = AantalBomen) %>%
    left_join(dfVolgorde, by = "selectie") %>%
    arrange(volgorde)
})
if (inherits(e, "try-error")) stop("MISLUKT laden 3-jaarlijkse data")


e <- try({
cp <- list(driejaarlijks[c(2,1)], driejaarlijks[c(3,2)], driejaarlijks[c(3,1)])

rv <- NULL
for (i in 1:length(cp)) {
  rv[[i]] <-
    bind_rows(
    filter(dfTrees3Gmsch, Jaar %in% cp[[i]]) %>%
      wilcox_table(formula = BladverliesNetto ~ Jaar | prbo) %>%
      select(verschil, p.value, signif) %>%
      mutate(selectie = "totaal", lincomb = paste(cp[[i]], collapse = "-")),

    filter(dfTrees3Gmsch, Jaar %in% cp[[i]]) %>%
      split(.$SoortType) %>%
      map_dfr(wilcox_table, formula = BladverliesNetto ~ Jaar | prbo, .id = "SoortType") %>%
      select(verschil, p.value, signif, SoortType) %>%
      mutate(selectie = SoortType, SoortType = NULL, lincomb = paste(cp[[i]], collapse = "-")),

    filter(dfTrees3Gmsch, Jaar %in% cp[[i]]) %>%
      split(.$SoortIndeling) %>%
      map_dfr(wilcox_table, formula = BladverliesNetto ~ Jaar | prbo, .id = "SoortIndeling") %>%
      select(verschil, p.value, signif, SoortIndeling) %>%
      mutate(selectie = SoortIndeling, SoortIndeling = NULL, lincomb = paste(cp[[i]], collapse = "-"))
    )
}
})
if (inherits(e, "try-error")) stop("MISLUKT wilcox tables")

e <- try({
bind_rows(rv) %>%
  mutate(diffsig = paste(round(verschil, 1), signif)) %>%
  select(diffsig, lincomb, selectie) %>%
  spread(key = lincomb, value = diffsig) %>%
  left_join(dfVolgorde) %>%
  arrange(volgorde) %>%
  select(-volgorde) %>%
  write.csv2(file.path(outdir, "driejaarlijks_39_evolutiebladverlies.csv"))
})
if (inherits(e, "try-error")) stop("MISLUKT: EVOLUTIE BLADVERLIES")


######

e <- try({
ggplotschadeklassen <- function(data, title = "", fig_width = 7, fig_height = 5, fig_dpi = 300){
  p <-
    ggplot(data, aes(x = BVKlasseEur, y = PctBomen, fill = factor(Jaar))) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
    xlab("schadeklasse") + ylab("percentage bomen") +
    guides(fill = guide_legend(title = "jaar")) + ylim(0,100)+ ggtitle(title) +
    scale_x_discrete(drop = FALSE)
  print(p)
  ggsave(plot = p, filename = file.path(outdir, paste0("driejaarlijks_06_", title, ".png")),
         width = fig_width, height = fig_height, dpi = fig_dpi)
}
})
if (inherits(e, "try-error")) stop("MISLUKT: SCHADEKLASSEN")

e <- try({
plotdata <- bomen_calc(dfTrees3Gmsch, c("Jaar"), "BVKlasseEur")
plotdata2 <- expand.grid(Jaar = unique(plotdata$Jaar), BVKlasseEur = unique(plotdata$BVKlasseEur))
plotdata <- left_join(plotdata2, plotdata, by = c("Jaar", "BVKlasseEur"))
plotdata$AantalBomen[is.na(plotdata$AantalBomen)] <- 0
ggplotschadeklassen(plotdata, "totaal")

plotdata <- bomen_calc(filter(dfTrees3Gmsch, SoortType == "loofbomen"), c("Jaar"), "BVKlasseEur")
plotdata2 <- expand.grid(Jaar = unique(plotdata$Jaar), BVKlasseEur = unique(plotdata$BVKlasseEur))
plotdata <- left_join(plotdata2, plotdata, by = c("Jaar", "BVKlasseEur"))
plotdata$AantalBomen[is.na(plotdata$AantalBomen)] <- 0
ggplotschadeklassen(plotdata, "loofbomen")

plotdata <- bomen_calc(filter(dfTrees3Gmsch, SoortType == "naaldbomen"), c("Jaar"), "BVKlasseEur")
plotdata2 <- expand.grid(Jaar = unique(plotdata$Jaar), BVKlasseEur = unique(plotdata$BVKlasseEur))
plotdata <- left_join(plotdata2, plotdata, by = c("Jaar", "BVKlasseEur"))
plotdata$AantalBomen[is.na(plotdata$AantalBomen)] <- 0
ggplotschadeklassen(plotdata, "naaldbomen")

for (i in unique(dfTrees3Gmsch$SoortIndeling)) {
  plotdata <-  bomen_calc(filter(dfTrees3Gmsch, SoortIndeling == i), c("Jaar"), "BVKlasseEur")
  plotdata2 <- expand.grid(Jaar = unique(plotdata$Jaar), BVKlasseEur = unique(plotdata$BVKlasseEur))
  plotdata <- left_join(plotdata2, plotdata, by = c("Jaar", "BVKlasseEur"))
  plotdata$AantalBomen[is.na(plotdata$AantalBomen)] <- 0
  if (i == "overige lbs.") i <- "overige loofboomsoorten"
  if (i == "overige nbs.") i <- "overige naaldboomsoorten"
  ggplotschadeklassen(plotdata, i)
}
})
if (inherits(e, "try-error")) stop("MISLUKT: PLOTS SCHADEKLASSEN")


cat("ALL DRIEJAARLIJKS FINISHED\n")
