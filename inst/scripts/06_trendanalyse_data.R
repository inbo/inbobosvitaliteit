

#TRENDANALYSE## >>> Trendanalyse

library(rkt)

e <- try({
### >>> Data inhoud

#Aantal Bomen
ggplot(dfTreesTrend, aes(x = Jaar)) + geom_bar() + ylab("Aantal bomen")

ggsave(file = file.path(outdir, "trend_aantalbomen.png"), width = fig_width, height = fig_height, dpi = fig_dpi)


#Aantal Plots
group_by(dfTreesTrend, Jaar) %>%
  summarize(aantal_plots = length(unique(PlotNr)), .groups = "drop") %>%
  ggplot(aes(x = Jaar, y = aantal_plots)) +
  geom_bar(stat = "identity")

#Voorkomen ieder plot
(AantalPlotJaren <- group_by(dfTreesTrend, PlotNr, Jaar) %>%
  summarize(AantalMetingen = length(MetingKey), .groups = "drop_last") %>%
  summarize(AantalJaar = sum(AantalMetingen > 0),
            TeWeinigJaar = AantalJaar < 0.8 * length(meerjaarlijks),
            .groups = "drop")) %>%
  ggplot(aes(x = factor(PlotNr), y = AantalJaar, color = factor(TeWeinigJaar))) +
  geom_point() +
  labs(x = "PlotNr", color = "TeWeinigJaar") + theme(axis.text.x = element_text(angle = 90))

ggsave(file = file.path(outdir, "trend_aantal_jaar_gemeten_per_plot.png"))

dfTreesTrendV <- filter(dfTreesTrend, PlotNr %in% (filter(AantalPlotJaren, TeWeinigJaar == FALSE) %>% pull(PlotNr)))
})
if (inherits(e, "try-error")) stop("MISLUKT: DATA INHOUD VERKENNEN")


###################################################################################################
### DATA
###################################################################################################
e <- try({
dfTrendSummary <-
  bomen_calc(x = dfTreesTrend, group = normal_groups, respons = "BladverliesNetto") %>%
  mutate(lcl = mean_value - 1.96 * se,
         ucl = mean_value + 1.96 * se)

cat("\nRuwe data bladverlies trend alle bomen\n")
ggplot(dfTrendSummary %>% filter(Set == "Jaar"),
       aes(x = Jaar, y = mean_value, ymin = lcl, ymax = ucl)) +
  geom_line() + geom_point() + geom_errorbar()  + ylab("Bladverlies (%)")
ggsave(file = file.path(outdir, "trend_ruwedata_bladverlies_allebomen.png"))

cat("\nRuwe data bladverlies trend soortindeling\n")
ggplot(dfTrendSummary %>% filter(Set == "Jaar.SoortIndeling"),
       aes(x = Jaar, y = mean_value, color = SoortIndeling,
           ymin = lcl, ymax = ucl)) +
  geom_line() + geom_point() + geom_errorbar()  + ylab("Bladverlies (%)")
ggsave(file = file.path(outdir, "trend_ruwedata_bladverlies_Soortindeling.png"))

cat("\nRuwe data bladverlies trend soorttype\n")
ggplot(dfTrendSummary %>% filter(Set == "Jaar.SoortType"),
       aes(x = Jaar, y = mean_value, color = SoortType,
           ymin = lcl, ymax = ucl)) +
  geom_line() + geom_point() + geom_errorbar()  + ylab("Bladverlies (%)")
ggsave(file = file.path(outdir, "trend_ruwedata_bladverlies_SoortType.png"))


cat("\nRuwe data beschadigde bomen trend alle bomen\n")
dfTrendBeschadigdTot <-
  group_by(dfTreesTrend, PlotNr, Jaar) %>%
  summarize(AantalBeschadigd = sum(Beschadigd == "beschadigd"),
            AantalOnbeschadigd = sum(Beschadigd == "onbeschadigd"),
            BomenInPlot = n(),
            PctBeschadigd = AantalBeschadigd / BomenInPlot * 100,
            .groups = "drop") %>%
  mutate(selectie = "totaal")

dfTrendBeschadigdTotSmry <- dfTrendBeschadigdTot %>%
  group_by(Jaar) %>%
  summarize(mPctBeschadigd = mean(PctBeschadigd, na.rm = TRUE),
            sePctBeschadigd = sd(PctBeschadigd, na.rm = TRUE)/sqrt(n()),
            lcl = mPctBeschadigd - 1.96 * sePctBeschadigd,
            ucl = mPctBeschadigd + 1.96 * sePctBeschadigd,
            .groups = "drop")
ggplot(dfTrendBeschadigdTotSmry,
       aes(x = Jaar, y = mPctBeschadigd, ymin = lcl, ymax = ucl)) +
  geom_line() + geom_point() + geom_errorbar() +
  ylab("Beschadigde proefvlakken (%)")
ggsave(file =
         file.path(outdir,
                   "trend_ruwedata_beschadigde_proefvlakken_allebomen.png" ))

###

cat("\nRuwe data beschadigde bomen trend soortindeling\n")
dfTrendBeschadigdType <-
  group_by(dfTreesTrend, PlotNr, Jaar, SoortType) %>%
  summarize(AantalBeschadigd = sum(Beschadigd == "beschadigd"),
            AantalOnbeschadigd = sum(Beschadigd == "onbeschadigd"),
            BomenInPlot = n(),
            PctBeschadigd = AantalBeschadigd / BomenInPlot * 100,
            .groups = "drop") %>%
  mutate(selectie = SoortType)

dfTrendBeschadigdTypeSmry <- dfTrendBeschadigdType %>%
  group_by(Jaar, SoortType) %>%
  summarize(mPctBeschadigd = mean(PctBeschadigd, na.rm = TRUE),
            sePctBeschadigd = sd(PctBeschadigd, na.rm = TRUE)/sqrt(n()),
            lcl = mPctBeschadigd - 1.96 * sePctBeschadigd,
            ucl = mPctBeschadigd + 1.96 * sePctBeschadigd,
            .groups = "drop")
ggplot(dfTrendBeschadigdTypeSmry,
       aes(x = Jaar, y = mPctBeschadigd, color = SoortType,
           ymin = lcl, ymax = ucl)) +
  geom_line() + geom_point() + geom_errorbar() +
  ylab("Beschadigde proefvlakken (%)")
ggsave(file =
         file.path(outdir,
                   "trend_ruwedata_beschadigde_proefvlakken_type.png" ))


###

cat("\nRuwe data beschadigde  proefvlakken trend\n")
dfTrendBeschadigdSoort <-
  group_by(dfTreesTrend, PlotNr, Jaar, SoortIndeling) %>%
  summarize(AantalBeschadigd = sum(Beschadigd == "beschadigd"),
            AantalOnbeschadigd = sum(Beschadigd == "onbeschadigd"),
            BomenInPlot = n(),
            PctBeschadigd = AantalBeschadigd / BomenInPlot * 100,
            .groups = "drop") %>%
  mutate(selectie = SoortIndeling)

dfTrendBeschadigdSoortSmry <- dfTrendBeschadigdSoort %>%
  group_by(Jaar, SoortIndeling) %>%
  summarize(mPctBeschadigd = mean(PctBeschadigd, na.rm = TRUE),
            sePctBeschadigd = sd(PctBeschadigd, na.rm = TRUE)/sqrt(n()),
            lcl = mPctBeschadigd - 1.96 * sePctBeschadigd,
            ucl = mPctBeschadigd + 1.96 * sePctBeschadigd,
            .groups = "drop")
ggplot(dfTrendBeschadigdSoortSmry,
       aes(x = Jaar, y = mPctBeschadigd, color = SoortIndeling,
           ymin = lcl, ymax = ucl)) +
  geom_line() + geom_point() + geom_errorbar() +
  ylab("Beschadigde proefvlakken (%)")
ggsave(file =
         file.path(outdir,
                   "trend_ruwedata_beschadigde_proefvlakken_soort.png" ))


###

dfTrendBeschadigd <- bind_rows(dfTrendBeschadigdTot,
                               dfTrendBeschadigdSoort,
                               dfTrendBeschadigdType)
})
if (inherits(e, "try-error")) stop("MISLUKT: DATA VOORBEREIDING")


####################################################################
# SEN SLOPE
####################################################################
library(rkt)

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
  cat("\n")
})
if (inherits(e, "try-error")) stop("MISLUKT: SEN ALGEMEEN")


