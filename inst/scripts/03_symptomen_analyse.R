
### AFGELEIDE DATASETS
e <- try({
  #! Soortinfo en Treeinfo combineren
  #Let op, er komen hier dubbele bomen voor, wegens verschillende oorzaken,
  #maar er kunnen ook duplicaatrijen ontstaan door meerdere aangetaste delen,
  #meerdere symptoomspecificaties en meerdere symptoomoorzaken

  dfSA <- get_SymptomAnalysisdata(dfTrees, dfSymptoms)

  #!Levend, met symptomen
  #deze dataset heeft het nadeel dat er duplicaatrijen komen
  #als een symptoom meerdere specificaties of oorzaken of organismeoorzaken krijgt)
  dfLMS <- filter(dfSA,
                  Jaar %in% jaarkeuze,
                  !is.na(OnderdeelBoomCat),
                  !(AangetastDeelCode %in% c(0,4))) #0 = geen symptoom, #4 is dood

  #! Dode bomen
  dfDead <- filter(dfSA, Jaar %in% jaarkeuze,  AangetastDeelCode == 4)

  #! Voorbereidende totalentabel (gebruikt al een eerste keer bomen_calc)
  dfTotaalBomen <-
    bomen_calc(dfTrees, normal_groups) %>%
    select(selectie, Jaar, TotaalAantalBomen = AantalBomen) %>%
    left_join(dfVolgorde, by = "selectie") %>%
    arrange(volgorde)
})
if (inherits(e, "try-error")) stop("MISLUKT: MAKEN AFGELEIDE DATASETS")



### 2.0 Aantal dode bomen / Aantal zonder symptomen / Aantal met meerdere symptomen

#2.0a: Aantal dode bomen
e <- try({
bomen_calc(dfTrees, group = c("Jaar"), group2 = c("Dood"))

dfSA %>%
  filter(AangetastDeelCode == 4) %>%
  select(PlotNr, PlotNaam, Soort, BoomNr) %>%
  filter(!duplicated(.)) %>%
  arrange(PlotNr) %>%
  write.csv2(file.path(outdir, "jaarlijks_03_dodebomen.csv"))
})
if (inherits(e, "try-error")) warning("MISLUKT: AANTAL DODE BOMEN")

#2.0b: Aantal bomen zonder, met en meerdere (unieke) symptomen (dode bomen worden weggelaten)
e <- try({
group_by(dfSA, Jaar, PlotNr, BoomNr) %>%
  summarize(AantalSymptomen = length(unique(SymptoomCode[!(AangetastDeelCode %in% c(0,4))])),
            ZonderSymptoom = sum(AangetastDeelCode == 0),
            DodeBoom = sum(AangetastDeelCode == 4)) %>% #! Unieke Symptomen
  group_by(Jaar) %>%
  summarize(MetSymptomen = sum(AantalSymptomen > 0), #Kan ook rechtstreeks gehaald worden uit het symtoom "no symptoms ..."
            MetMeerdereSymptomen = sum(AantalSymptomen > 1),
            ZonderSymptomen = sum(AantalSymptomen == 0 & DodeBoom == 0),
            DodeBoom = sum(DodeBoom > 0))

#Zou gelijk getal moeten geven
group_by(dfSA, Jaar) %>% summarize(sum(AangetastDeelCode == 0))


persymp <- bomen_calc(dfSA, group = c("Jaar", "SoortIndeling"), group2 =  "Symptoom") %>%
  select(SoortIndeling, Symptoom, AantalBomen)
totsrt <- bomen_calc(dfSA, group = c("Jaar", "SoortIndeling")) %>%
  select(SoortIndeling, TotaalBomen = AantalBomen)
left_join(persymp, totsrt, by = c("Jaar", "SoortIndeling")) %>%
  mutate(Percentage = AantalBomen / TotaalBomen * 100) %>%
  arrange(SoortIndeling, desc(AantalBomen)) %>%
  write.csv2(file.path(outdir, "jaarlijks_24_symptoom_per_soort.csv"))
})
if (inherits(e, "try-error")) warning("MISLUKT: SYMPTOOMAANTALLEN")


### 2.1 Aantal bomen: onderdeel boom / onderdeel boom symptoom / stam_spec_symptoom_soort

e <- try({
#2.1a: Aantal bomen met symtoom op een bepaald onderdeel
dfa <- bomen_calc(dfSA, group = c("Jaar", "OnderdeelBoomCat"), group2 = c("AangetastDeel"), uniquecount = "SymptoomCode")
dfb <- bomen_calc(dfSA, c("Jaar"), "OnderdeelBoomCat", uniquecount = "SymptoomCode")
bind_rows(dfa, dfb)

#2.1b: Aantal bomen, onderdeel boom, symtoom

dfSA_discolor <- filter(dfSA, SymptoomCode %in% c(2,3)) %>%
  group_by(Jaar, BoomKey, WaarnemingKey, OnderdeelBoomCat) %>%
  summarise(MetingKey = (MetingKey[1]),
            SymptoomCode  = 9923,
            Symptoom = "yellowgreen and/or redbrown discolouration")

dfSA2 <- bind_rows(dfSA %>% select(Jaar, BoomKey, WaarnemingKey, MetingKey, OnderdeelBoomCat, SymptoomCode, Symptoom), dfSA_discolor)


bomen_calc(dfSA2, c("Jaar", "OnderdeelBoomCat", "SymptoomCode"), "Symptoom") %>%
  ungroup() %>%
  filter(SymptoomCode > 0) %>%
  group_by(OnderdeelBoomCat, Symptoom) %>%
  summarize(AantalBomen = sum(AantalBomen), PctOfTotaalBomen = sum(PctOfTotaalBomen)) %>%
  arrange(OnderdeelBoomCat, desc(AantalBomen)) %>%
  write.csv2(file.path(outdir, "jaarlijks_22_percentage_bomen_met_symptomen.csv"))

#2.1c: SymptoomSpec per Symptoom, keuze soort, loof, naald, etc. ==> Mijn aantal bomen voor subtotaal is anders, en volgens mij correcter

bomen_calc(filter(dfLMS, SymptoomSpecCode != -1),
           lapply(normal_groups, c, c("OnderdeelBoomCat", "SymptoomCode"), "SymptoomSpecificatie"))

bomen_calc(dfLMS, c("Jaar", "OnderdeelBoomCat", "SoortType", "SymptoomCode"))
})
if (inherits(e, "try-error")) warning("MISLUKT: SYMPTOMEN PER ONDERDEEL")



### 2.2 Aandeel bomen symptoom soort (2.1 in percentages?) / top 5 symptomen per soort

#2.2a Berekening van percentages, geen rekening houden met onderdeelboomcat

e <- try({
bomen_calc(dfSA, normal_groups, c("SymptoomCode", "Symptoom"))

#2.2b top 5 symptomen per soort (gewoon een sortering en filtering van 2.2a)

bomen_calc(dfLMS, normal_groups, c("SymptoomCode", "Symptoom")) %>%
  arrange(Jaar, SoortType, desc(AantalBomen))
})
if (inherits(e, "try-error")) warning("MISLUKT: SBOMEN PER SYMPTOOM")

### 2.3 Aandeel bomen oorzaak / aandeel oorzaken symptoom (hier weer de dode bomen en bomen zonder symptomen inclusief)

#2.3a
e <- try({
bomen_calc(dfSA, c("Jaar"), c("SymptoomOorzaakGroepNaam"), uniquecount = "SymptoomOorzaakCode") %>%
  select(SymptoomOorzaakGroepNaam, AantalBomen, PctOfTotaalBomen) %>%
  arrange(desc(AantalBomen)) %>%
  write.csv2(file.path(outdir, "jaarlijks_23_symptoomoorzaken.csv"))

#2.3b
bomen_calc(dfSA, normal_groups,
           c("SymptoomCode","SymptoomOorzaakCode","SymptoomOrganisme", "SymptoomExtent"))
})
if (inherits(e, "try-error")) warning("MISLUKT: SYMPTOOMOORZAAK")

### 3.1 Oorzaak dode bomen (1 boom heeft meerdere oorzaken, daarom er hier 19 ipv 18 records zijn)
e <- try({
group_by(dfDead, Jaar, SymptoomOorzaakCode, SymptoomOorzaak, SymptoomOrganisme) %>%
  summarize(Aantal = n()) %>%
  filter(Jaar == 2017)
})
if (inherits(e, "try-error")) warning("MISLUKT: OORZAAK DODE BOMEN")


### 3.4 Aandeel bomen aangetastdeel soort
e <- try({
bomen_calc(dfSA, normal_groups, c("AangetastDeelCode"))

bomen_calc(dfSA, normal_groups, c("AangetastDeel"))

bomen_calc(filter(dfSA, AangetastDeelCode %in% 31:34, SymptoomCode == 17) , normal_groups, "SymptoomSpecificatie") %>%
  left_join(dfTotaalBomen) %>%
  mutate(Pct = AantalBomen / TotaalAantalBomen * 100,
         SymptoomSpecificatie = factor(SymptoomSpecificatie, levels = c("debarking", "cracks", "other wounds"))) %>%
  select(volgorde, selectie, SymptoomSpecificatie, Pct) %>%
  spread(key = SymptoomSpecificatie, value = Pct, fill = 0.00) %>%
  arrange(volgorde) %>%
  write.csv2(file.path(outdir, "jaarlijks_30_verwonding_aan_stam.csv"))
})
if (inherits(e, "try-error")) warning("MISLUKT: AANGETAST DEEL")


### 3.5 Top 5 oorzaken per soort
e <- try({
bomen_calc(dfSA, c("Jaar", "SymptoomOorzaak")) %>%
  arrange(Jaar, desc(AantalBomen))
})
if (inherits(e, "try-error")) warning("MISLUKT: TOP 5 OORZAKEN")

### 3.6 Aandeel bomen onderdeel boom symtoom extent

e <- try({
bomen_calc(dfSA, lapply(normal_groups, c, c("OnderdeelBoomCat", "SymptoomCode")), "SymptoomExtent")
})
if (inherits(e, "try-error")) warning("MISLUKT: SYMPTOMEN EXTENT")

### 3.7 Aandeel bomen met verkleuring oorzaak organisme / bladvraat oorzaak organisme

e <- try({
right_join(
  bomen_calc(dfSA, normal_groups, "SymptoomAbnormaalVerkleurd") %>%
    filter(SymptoomAbnormaalVerkleurd == TRUE) %>%
    select(selectie, AantalBomen),
  bomen_calc(dfSA, normal_groups) %>%
    select(selectie, TotaalBomen = AantalBomen)) %>%
  mutate(Percentage = AantalBomen / TotaalBomen * 100,
         Percentage = replace(Percentage, is.na(Percentage), 0.00)) %>%
  left_join(dfVolgorde) %>%
  arrange(volgorde) %>%
  select(selectie, Percentage) %>%
  write.csv2(file.path(outdir, "jaarlijks_25_abnormale_verkleuring.csv"))
})
if (inherits(e, "try-error")) warning("MISLUKT: ABNORMAAL VERKLEURD")


#3.7a Verkleuring Oorzaak Organisme

e <- try({
bomen_calc(filter(dfSA, SymptoomCode %in% c(2,3)), normal_groups,
           c("Symptoom", "SymptoomOorzaakCode", "SymptoomOrganisme", "SymptoomExtent"))


bomen_calc(filter(dfSA, SymptoomCode %in% c(2,3), SymptoomOorzaakCode %in% 300:399, SymptoomVerkleurd == TRUE),
           normal_groups, c("SymptoomExtent")) %>%
  left_join(dfTotaalBomen) %>%
  mutate(Pct = AantalBomen / TotaalAantalBomen * 100) %>%
  select(volgorde, SymptoomExtent, selectie, Pct) %>%
  spread(key = SymptoomExtent, value = Pct, fill = 0.00) %>%
  arrange(volgorde) %>%
  write.csv2(file.path(outdir, "jaarlijks_28_verkleuring_door_schimmels.csv"))
})
if (inherits(e, "try-error")) warning("MISLUKT: VERKLEURING SCHIMMELS")

#3.7b Bladvraat oorzaak organisme

e <- try({
bomen_calc(filter(dfSA, SymptoomCode %in% c(1)), normal_groups, "SymptoomExtent") %>%
  left_join(dfTotaalBomen) %>%
  select(Jaar, selectie, SymptoomExtent, AantalBomen, TotaalAantalBomen, volgorde) %>%
  mutate(Pct = AantalBomen / TotaalAantalBomen * 100) %>%
  select(volgorde, selectie, SymptoomExtent, Pct) %>%
  spread(key = SymptoomExtent, value = Pct, fill = 0.00) %>%
  arrange(volgorde) %>%
  write.csv2(file.path(outdir, "jaarlijks_26_verdeling_insectenaantasting.csv"))

bomen_calc(filter(dfSA, SymptoomCode %in% c(1)), lapply(normal_groups, c, c("Symptoom", "SymptoomOorzaakCode", "SymptoomOrganisme")),
           "SymptoomExtent")
})
if (inherits(e, "try-error")) warning("MISLUKT: INSECTENAANTASTING VERDELING")

##---> FOUTMELDING MET DATA EIND 2022 (datasets zonder rijen)
e <- try({
  bomen_calc(filter(dfSA, OnderdeelBoomCat == "Stam",
                  SymptoomOorzaakGroep == 200),
           lapply(normal_groups, c, "OnderdeelBoomCat"), "SymptoomExtent") %>%
  left_join(dfTotaalBomen) %>%
  mutate(Pct = AantalBomen / TotaalAantalBomen * 100) %>%
  arrange(volgorde) %>%
  select(selectie, Pct) %>%
  write.csv2(file.path(outdir, "jaarlijks_27_insecten_op_stam.csv"))
})
if (inherits(e, "try-error")) warning("MISLUKT: INSECTEN OP STAM")

### 3.8 aandeel bomen met kroonsterfte oorzaak organisme / kroonsterfte aangetast deel soort extent

#3.8a
e <- try({
bomen_calc(filter(dfSA, SymptoomCode %in% c(14)), lapply(normal_groups, c, c("Symptoom", "SymptoomOorzaakCode", "SymptoomOrganisme")),
           "SymptoomExtent")

#3.8b

bomen_calc(filter(dfSA, SymptoomCode %in% c(14)), lapply(normal_groups, c, "AangetastDeelCode"), "SymptoomExtent")

#Let op, kolomvolgorde is niet volledig correct: 2-10 staat het laatste
bomen_calc(filter(dfSA, SymptoomCode %in% c(14)), lapply(normal_groups, c, "AangetastDeel"), "SymptoomExtent") %>%
  left_join(dfTotaalBomen) %>%
  ungroup() %>%
  mutate(Pct = AantalBomen / TotaalAantalBomen * 100,
         AangetastDeel = recode(AangetastDeel, "diameter >= 10 cm" = "diameter is 10+"),
         groep = interaction(AangetastDeel, SymptoomExtent, lex.order = TRUE)) %>%
  filter(SymptoomExtent != "Unknown") %>%
  select(volgorde, selectie, groep, Pct) %>%
  spread(key = groep, value = Pct, fill = 0.00) %>%
  arrange(volgorde) %>%
  write.csv2(file.path(outdir, "jaarlijks_29_kroonsterfte.csv"))
})
if (inherits(e, "try-error")) warning("MISLUKT: KROONSTERFTE")

### 3.9 aandeel bomen met insecten soort extent / bladvreters soort extent / schimmelaantasting proefvlak soort /
###     aandeel eiken met bladvraat proefvlak extent /
###     aandeel bomen sympt 10 en 11 / sympt 10 en 11 oorzaak / sympt 10 en 11 oorzaak organisme
###     aandeel eiken met teken van insecten proefvlak extent

#3.9a Insecten
e <- try({
bomen_calc(filter(dfSA, SymptoomCode %in% c(1)), lapply(normal_groups, c, c("SymptoomCode", "OnderdeelBoomCat")), group2 = "SymptoomExtent")

#3.9b bladvreters (oorzaak)

bomen_calc(filter(dfSA, SymptoomOorzaakCode %in% c(210), SymptoomCode %in% c(1, 10)),
           lapply(normal_groups, c, c("OnderdeelBoomCat")), "SymptoomExtent")

#3.9c schimmelaantasting proefvlak soort

bomen_calc(filter(dfSA, SymptoomOorzaakCode %in% c(300:399)),
           c("Jaar", "PlotNr","SPEC_DES", "SymptoomOrganisme"), "SymptoomExtent")

bomen_calc(filter(dfSA, SymptoomOorzaakCode %in% c(300:399)),
           lapply(normal_groups, c, "SymptoomExtent")) %>%
  left_join(dfTotaalBomen) %>%
  mutate(Pct = AantalBomen / TotaalAantalBomen * 100) %>%
  select(selectie, Pct, SymptoomExtent)


#3.9d aandeel eiken met bladvraat proefvlak extent

bomen_calc(filter(dfSA,
                  substring(SPEC_DES, 1,7) == "Quercus",
                  SymptoomOorzaakCode == 210,
                  SymptoomCode %in% c(1,10)),
           c("Jaar", "PlotNr", "SPEC_DES"),
           "SymptoomExtent")

#3.9e aandeel bomen met symptomen 10 en 11

bomen_calc(dfSA, c("Jaar", "SymptoomCode")) %>%
  filter(SymptoomCode %in% c(10,11))

#3.9f aandeel bomen met symptomen 10 en 11 + oorzaak

bomen_calc(filter(dfSA, SymptoomCode %in% c(10,11)), c("Jaar", "SymptoomCode"), c("SymptoomOorzaakCode", "SymptoomOorzaak"))

#3.9g aandeel bomen, met symptomen 10 en 11 + oorzaakorganisme

bomen_calc(filter(dfSA, SymptoomCode %in% c(10,11)),
           c("Jaar", "SymptoomCode"),
           c("SymptoomOorzaakCode", "SymptoomOrganisme"))

#3.9h aandeel eiken met teken van insecten per proefvlak extent (KOMT NIET OVEREEN MET VOORBEELD)

bomen_calc(filter(dfSA, SymptoomCode %in% c(10)),
           c("Jaar", "PlotNr", "SPEC_DES"),
           "SymptoomExtent")
})
if (inherits(e, "try-error")) warning("MISLUKT: SYSTOOMEXTENT PER PROEFVLAK")

### 3.10 aandeel dennen met sphaeropsis / sphaeropsis aangetaste deel symtoom

#3.10a aandeel dennen met SPhaeropsis

e <- try({
bomen_calc(filter(dfSA, SoortType == "naaldbomen"), c("Jaar", "SPEC_DES"), "SymptoomOrganisme") %>%
  filter(SymptoomOrganisme == "Sphaeropsis sapinea")

#3.10b Aandeel dennen met sphaeropsis aangetast deel symptomen (DIT LIJKT NIET TE KLOPPEN MET DE VRAAG)

bomen_calc(filter(dfSA, SoortType == "naaldbomen"),
           c("Jaar", "PlotNr", "SPEC_DES", "AangetastDeelCode", "SymptoomCode", "Symptoom"),
           respons = "BeschadigdNum") %>%
  filter(SPEC_DES == "Pinus nigra", SymptoomCode == 14) %>%
  mutate(PctBeschadigd = 100 * mean_value) %>%
  select(Jaar, PlotNr, SPEC_DES, AangetastDeelCode, Symptoom, AantalBomen, PctBeschadigd)
})
if (inherits(e, "try-error")) warning("MISLUKT: SPHAEROPSIS")

### X. aangetaste delen symptoom boom

e <- try({
bomen_calc(filter(dfSA, SoortType == "naaldbomen"), c("SoortType", "AangetastDeel"), "Symptoom")
})
if (inherits(e, "try-error")) warning("MISLUKT: AANGETASTE DELEN PER SOORT")

### andere

#Hars/Slijmuitvloei
e <- try({
bomen_calc(filter(dfSA, SymptoomCode %in% c(18,19)), normal_groups) %>%
  left_join(dfTotaalBomen) %>%
  mutate(Pct = AantalBomen / TotaalAantalBomen * 100) %>%
  select(volgorde, selectie, Pct) %>%
  arrange(volgorde) %>%
  write.csv2(file.path(outdir, "jaarlijks_31_harsslijm.csv"))
})
if (inherits(e, "try-error")) warning("MISLUKT: HARSSLIJMUITVLOEI")


#Vervorming op stam, stamvoet of geëxposeerde wortels
e <- try({
bomen_calc(filter(dfSA, SymptoomCode == 8, OnderdeelBoomCat == "Stam"), normal_groups, "SymptoomSpecificatie") %>%
  left_join(dfTotaalBomen) %>%
  mutate(Pct = AantalBomen / TotaalAantalBomen * 100,
         SymptoomSpecificatie = recode(SymptoomSpecificatie, cankers = "cancer/tumor", tumors = "cancer/tumor")) %>%
  group_by(Jaar, volgorde, selectie, SymptoomSpecificatie) %>%
  summarize(Pct = sum(Pct)) %>%
  spread(key = SymptoomSpecificatie, value = Pct, fill = 0.00) %>%
  arrange(volgorde) %>%
  write.csv2(file.path(outdir, "jaarlijks_32_vervorming_stam.csv"))
})
if (inherits(e, "try-error")) warning("MISLUKT: VERVORMING STAM")


#bomen met takbreuk
e <- try({
bomen_calc(filter(dfSA, SymptoomCode == 13, OnderdeelBoomCat == "Takken"), normal_groups, "SymptoomExtent") %>%
  left_join(dfTotaalBomen) %>%
  mutate(Pct = AantalBomen / TotaalAantalBomen * 100) %>%
  select(volgorde, selectie, Pct, SymptoomExtent) %>%
  filter(SymptoomExtent != "Unknown") %>%
  spread(key = SymptoomExtent, value = Pct, fill = 0.00) %>%
  arrange(volgorde) %>%
  write.csv2(file.path(outdir, "jaarlijks_33_takbreuk.csv"))
})
if (inherits(e, "try-error")) warning("MISLUKT: TAKBREUK")

cat("ALL SYMPTOMENCODE FINISHED\n")
