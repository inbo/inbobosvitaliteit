

#TRENDANALYSE## >>> Trendanalyse

library(rkt)

e <- try({
### >>> Data inhoud

#Aantal Bomen
ggplot(dfTreesTrend, aes(x = Jaar)) + geom_bar() + ylab("Aantal bomen")

ggsave(file = file.path(outdir, "trend_aantalbomen.png"), width = fig_width, height = fig_height, dpi = fig_dpi)


#Aantal Plots
group_by(dfTreesTrend, Jaar) %>%
  summarize(aantal_plots = length(unique(PlotNr))) %>%
  ggplot(aes(x = Jaar, y = aantal_plots)) +
  geom_bar(stat = "identity")

#Voorkomen ieder plot
(AantalPlotJaren <- group_by(dfTreesTrend, PlotNr, Jaar) %>%
  summarize(AantalMetingen = length(MetingKey)) %>%
  summarize(AantalJaar = sum(AantalMetingen > 0),
            TeWeinigJaar = AantalJaar < 0.8 * length(meerjaarlijks))) %>%
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

ggplot(dfTrendSummary %>% filter(Set == "Jaar"),
       aes(x = Jaar, y = mean_value, ymin = lcl, ymax = ucl)) +
  geom_line() + geom_point() + geom_errorbar()  + ylab("Bladverlies (%)")
ggsave(file = file.path(outdir, "trend_ruwedata_bladverlies_allebomen.png"))

ggplot(dfTrendSummary %>% filter(Set == "Jaar.SoortIndeling"),
       aes(x = Jaar, y = mean_value, color = SoortIndeling,
           ymin = lcl, ymax = ucl)) +
  geom_line() + geom_point() + geom_errorbar()  + ylab("Bladverlies (%)")
ggsave(file = file.path(outdir, "trend_ruwedata_bladverlies_Soortindeling.png"))

ggplot(dfTrendSummary %>% filter(Set == "Jaar.SoortType"),
       aes(x = Jaar, y = mean_value, color = SoortType,
           ymin = lcl, ymax = ucl)) +
  geom_line() + geom_point() + geom_errorbar()  + ylab("Bladverlies (%)")
ggsave(file = file.path(outdir, "trend_ruwedata_bladverlies_SoortType.png"))



dfTrendBeschadigdTot <-
  group_by(dfTreesTrend, PlotNr, Jaar) %>%
  summarize(AantalBeschadigd = sum(Beschadigd == "beschadigd"),
            AantalOnbeschadigd = sum(Beschadigd == "onbeschadigd"),
            BomenInPlot = n(),
            PctBeschadigd = AantalBeschadigd / BomenInPlot * 100) %>%
  mutate(selectie = "totaal")

dfTrendBeschadigdTotSmry <- dfTrendBeschadigdTot %>%
  group_by(Jaar) %>%
  summarize(mPctBeschadigd = mean(PctBeschadigd, na.rm = TRUE),
            sePctBeschadigd = sd(PctBeschadigd, na.rm = TRUE)/sqrt(n()),
            lcl = mPctBeschadigd - 1.96 * sePctBeschadigd,
            ucl = mPctBeschadigd + 1.96 * sePctBeschadigd)
ggplot(dfTrendBeschadigdTotSmry,
       aes(x = Jaar, y = mPctBeschadigd, ymin = lcl, ymax = ucl)) +
  geom_line() + geom_point() + geom_errorbar() +
  ylab("Beschadigde proefvlakken (%)")
ggsave(file =
         file.path(outdir,
                   "trend_ruwedata_beschadigde_proefvlakken_allebomen.png" ))

###

dfTrendBeschadigdType <-
  group_by(dfTreesTrend, PlotNr, Jaar, SoortType) %>%
  summarize(AantalBeschadigd = sum(Beschadigd == "beschadigd"),
            AantalOnbeschadigd = sum(Beschadigd == "onbeschadigd"),
            BomenInPlot = n(),
            PctBeschadigd = AantalBeschadigd / BomenInPlot * 100) %>%
  mutate(selectie = SoortType)

dfTrendBeschadigdTypeSmry <- dfTrendBeschadigdType %>%
  group_by(Jaar, SoortType) %>%
  summarize(mPctBeschadigd = mean(PctBeschadigd, na.rm = TRUE),
            sePctBeschadigd = sd(PctBeschadigd, na.rm = TRUE)/sqrt(n()),
            lcl = mPctBeschadigd - 1.96 * sePctBeschadigd,
            ucl = mPctBeschadigd + 1.96 * sePctBeschadigd)
ggplot(dfTrendBeschadigdTypeSmry,
       aes(x = Jaar, y = mPctBeschadigd, color = SoortType,
           ymin = lcl, ymax = ucl)) +
  geom_line() + geom_point() + geom_errorbar() +
  ylab("Beschadigde proefvlakken (%)")
ggsave(file =
         file.path(outdir,
                   "trend_ruwedata_beschadigde_proefvlakken_type.png" ))


###

dfTrendBeschadigdSoort <-
  group_by(dfTreesTrend, PlotNr, Jaar, SoortIndeling) %>%
  summarize(AantalBeschadigd = sum(Beschadigd == "beschadigd"),
            AantalOnbeschadigd = sum(Beschadigd == "onbeschadigd"),
            BomenInPlot = n(),
            PctBeschadigd = AantalBeschadigd / BomenInPlot * 100) %>%
  mutate(selectie = SoortIndeling)

dfTrendBeschadigdSoortSmry <- dfTrendBeschadigdSoort %>%
  group_by(Jaar, SoortIndeling) %>%
  summarize(mPctBeschadigd = mean(PctBeschadigd, na.rm = TRUE),
            sePctBeschadigd = sd(PctBeschadigd, na.rm = TRUE)/sqrt(n()),
            lcl = mPctBeschadigd - 1.96 * sePctBeschadigd,
            ucl = mPctBeschadigd + 1.96 * sePctBeschadigd)
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


