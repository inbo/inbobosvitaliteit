### >>> Data
jaren <- 1987:2022  #!!!! ieder jaar wijzigen

### >>> Laden omgeving
library(tidyverse)
library(mgcv)
library(INBOtheme)
if (!('remotes' %in% rownames(installed.packages() ))) install.packages('remotes')
if (!('git2rdata' %in% rownames(installed.packages() ))) remotes::install_github('inbo/git2rdata')

### >>> Variabelen

#! Server
server   <- "inbo-sql07-prd.inbo.be"
database <- "D0004_00_Bosvitaliteit"

### >>> Query

natuurindicatoren_sql <- paste(
  "select
  p.PLOT_NUM as PlotNr
  , b.BOOM_BNR as BoomNr
  , w.WRNG_JAA as Jaar
  , s.SPEC_EUR_CDE as Soortnummer
  , m.WRME_OMT as Omtrek
  , m.WRME_LFT as Leeftijd
  , m.WRME_UCBL_CDE as BladverliesNetto

  from
  tblProefvlak p left join
  tblWaarneming w on w.WRNG_PLOT_ID = p.PLOT_ID left join
  tblWaarnemingMeting m on m.WRME_WRNG_ID = w.WRNG_ID left join
  tblBoom b on b.BOOM_ID = m.WRME_BOOM_ID left join
  tblSoort s on s.SPEC_ID = b.BOOM_SPEC_ID",
  paste0(" \nwhere w.WRNG_JAA in (", paste(jaren, collapse = ","), ")")
)

### >>> Data ophalen

conn <- DBI::dbConnect(odbc::odbc(),
                       Driver = "SQL Server",
                       Server = server,
                       Database = database,
                       Trusted_Connection = "True")

dfNatuurindicatoren <- DBI::dbGetQuery(conn, natuurindicatoren_sql)
save(dfNatuurindicatoren, file = "dfNatuurindicatoren.Rdata")

### >>> Data transformaties

dfNI <- dfNatuurindicatoren %>%
  mutate(BladverliesNetto = as.numeric(BladverliesNetto),
         Beschadigd = BladverliesNetto > 25,
         jaar = Jaar,
         Jaar = NULL) %>%
  group_by(jaar)

dfNIS <- dfNI %>%
  summarize(beschadigd = sum(Beschadigd, na.rm = TRUE),
            gezond = sum(!Beschadigd, na.rm = TRUE),
            totaal = n()) %>%
  mutate(schade_pct = beschadigd / totaal * 100)

git2rdata::write_vc(dfNIS, file = "beschadigde_bosbomen.csv",
                    sorting = c("jaar"),
                    optimize = FALSE)

############################
vlaanderen_europa_data <-
"Jaar,niveau,Aandeel
1990,Europa,19.5
1990,Vlaanderen,8.3
1991,Europa,20.7
1991,Vlaanderen,12.9
1992,Europa,23.7
1992,Vlaanderen,17.3
1993,Europa,24.2
1993,Vlaanderen,16.8
1994,Europa,24.6
1994,Vlaanderen,22.2
1995,Europa,26.9
1995,Vlaanderen,33.2
1996,Europa,25.4
1996,Vlaanderen,26.4
1997,Europa,24.2
1997,Vlaanderen,19.3
1998,Europa,23.2
1998,Vlaanderen,22.1
1999,Europa,21
1999,Vlaanderen,21.9
2000,Europa,22.2
2000,Vlaanderen,25.2
2001,Europa,22.4
2001,Vlaanderen,22.1
2002,Europa,21.3
2002,Vlaanderen,21.7
2003,Europa,22.7
2003,Vlaanderen,20
2004,Europa,23.3
2004,Vlaanderen,20.8
2005,Europa,23.2
2005,Vlaanderen,21.3
2006,Europa,21.9
2006,Vlaanderen,19.1
2007,Europa,21.8
2007,Vlaanderen,17.3
2008,Europa,21
2008,Vlaanderen,14.3
2009,Europa,20.2
2009,Vlaanderen,15.1
2010,Europa,19.5
2010,Vlaanderen,16.1
2011,Europa,20
2011,Vlaanderen,20.1
2012,Europa,22.9
2012,Vlaanderen,25
2013,Europa,20.5
2013,Vlaanderen,20.8
2014,Europa,23.8
2014,Vlaanderen,21.1
2015,Europa,23.3
2015,Vlaanderen,21.5
2016,Europa,25.2
2016,Vlaanderen,20.3
2017,Europa,25.1
2017,Vlaanderen,20.3
2018,Europa,27
2018,Vlaanderen,22.8
2019,Europa,28.4
2019,Vlaanderen,22.7
2020,Europa,28.2
2020,Vlaanderen,25.3
2021,Europa,28.6
2021,Vlaanderen,19.9
2022,Europa,NA
2022,Vlaanderen,26.6
"

vlaanderen_europa <- read.table(text = vlaanderen_europa_data,
                                sep = ",",
                                header = TRUE)


vlaanderen_europa$niveau <- factor(vlaanderen_europa$niveau)
git2rdata::write_vc(vlaanderen_europa, file = "vlaanderen_europa.csv",
                    sorting = c("Jaar", "niveau"),
                    optimize = FALSE)

#Kopieer nu de bestanden die via write_csv gegenereerd worden naar de natuurindicatoren repository. Dit gaat over:
#- beschadigde_bosbomen.yml
#- beschadigde_bosbomen.csv
#- vlaanderen_europa.csv
#- vlaanderen_europa.yml
#


####################################################################################
## ONDERSTAANDE WORDT IN DE NATUURINDICATOREN REPOSITORY ZELF UITGEVOERD
## HIER ENKEL ALS TEST
##################################################################################

## FIGUUR INSTELLINGEN
#!Figuur
theme_set(theme_inbo(10))
theme_inbo <- theme_update(axis.title.x = element_text(colour = "black"),
                           axis.title.y = element_text(colour = "black"),
                           plot.title = element_text(colour = "black"),
                           legend.key = element_rect(fill = "white"))
update_geom_defaults("point", aes(size = 2))
update_geom_defaults("line", aes(size = 0.25))
update_geom_defaults("point", aes(size = 1))
update_geom_defaults("line", aes(size = 0.25))
fig.width <- 147 / 25.4
fig.height <- 103 / 25.4


### >>> Indicatormodel (NIET MEER NODIG OM TE GEBRUIKEN, enkel ter illustratie ---> officiele berekning zit in het indicatoren repository, enkel dfNIS doorgeven)

jni <- jaren

modelIndicator <- gam(schade_pct ~ s(jaar, k=5), data = dfNIS)
newdata <- data.frame(jaar = c(jni, jni[length(jni)] + 1:4))
newdata$future <- ifelse(newdata$jaar > jni[length(jni)], "Unobserved", "Observed")
newdata$predict <- predict(modelIndicator, newdata = newdata)
plot(modelIndicator)
preds <- predict(modelIndicator, newdata, type = "link", se.fit = TRUE)
newdata$lwr <- preds$fit - 1.96 * preds$se.fit
newdata$upr <- preds$fit + 1.96 * preds$se.fit
newdata$fit <- preds$fit


### >>> Plotdata

DataFig <- data.frame(jaar = dfNIS$jaar, waarde = dfNIS$schade_pct)
DataFig <- right_join(DataFig, newdata[c("jaar", "fit", "lwr", "upr")], by = c("jaar" = "jaar"))
DataFig$spreiding <- TRUE
DataFig$type <-  "Trend"
data <- subset(DataFig, jaar <= jni[length(jni)])
data$eenheid <- "beschadigde bosbomen (%)"
IND <- "Aandeel beschadigde bosbomen "
Sub_ID <- ""


### >>> Plot

###PNG

p <- ggplot(data, aes(x = jaar, y = fit, ymin = lwr, ymax = upr, linetype = type)) + ylab("beschadigde bosbomen (%)") +
  geom_ribbon(alpha = 0.1, aes(fill = spreiding)) +
  geom_line() +
  geom_point(aes(y = waarde)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_y_continuous(paste(unique(data$eenheid))) +
  scale_linetype_discrete(guide = guide_legend(title = NULL,
                                               override.aes = list(fill = NA),
                                               label.theme = element_text(color = "black", size = 10))) +
  scale_fill_manual(na.translate = FALSE,
                    labels = c("onzekerheid\nop de trend"),
                    values = c("TRUE" = inbo_steun_blauw),
                    guide = guide_legend(title = NULL,
                                         label.theme = element_text(color = "black", size = 10)))  +
  ggtitle(paste(IND, Sub_ID, sep = "\n"))

ggsave(p, width = fig.width, height = fig.height,
       file = paste0("",
                     paste(gsub(" ", "",
                                paste(IND, Sub_ID, sep = "__")), "png", sep = ".")))

print(p)

###EPS

p <- ggplot(data, aes(x = jaar, y = fit, ymin = lwr, ymax = upr, linetype = type)) + ylab("beschadigde bosbomen (%)") +
  geom_ribbon(alpha = 1, aes(fill = spreiding)) +
  geom_line() +
  geom_point(aes(y = waarde)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_y_continuous(paste(unique(data$eenheid))) +
  scale_linetype_discrete(guide = guide_legend(title = NULL,
                                               override.aes = list(fill = NA),
                                               label.theme = element_text(color = "black", size = 10))) +
  scale_fill_manual(na.translate = FALSE, labels = c("onzekerheid\nop de trend"),
                    values = c("TRUE" = inbo_hoofd),
                    guide = guide_legend(title = NULL,
                                         label.theme = element_text(color = "black", size = 10)))  +
  ggtitle(paste(IND, Sub_ID, sep = "\n"))

print(p)

ggsave(p, width = fig.width, height = fig.height,
       file = paste0("",
                     paste(gsub(" ", "",
                                paste(IND, Sub_ID, sep = "__")), "eps", sep = ".")))

write.csv2(file = "Meetpunten indicator.csv", DataFig)

### PLOT vergelijking met Europa

p <- ggplot(vlaanderen_europa, aes(x = Jaar, y = Aandeel, color = niveau)) +
  geom_path() + geom_point()

print(p)

