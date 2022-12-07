

#' Generate tree species metadatafile for use in reporting
#'
#' @param target directory to export the species information to
#'
#' @return a csv (;) file generated with columns SPEC_EUR_CDE (European species code), SPEC_DES (scientific name), species, species_main_cat (report categorisation general), species_sub_cat (more detailed categorisation), species_name (English name), species_order (Reporting order)
#' @export
generate_tree_species_data <- function(target = "extdata") {
  cat("SPEC_EUR_CDE;SPEC_DES;species;species_main_cat;species_sub_cat;species_name;species_order
      999;;totaal;totaal;totaal;total;0
      9998;;totaal jong;totaal jong;totaal jong;total young;1
      9999;;totaal oud;totaal oud;totaal oud;total old;2
      948;;loofbomen;loofbomen;loofbomen;deciduous trees;11
      9997;;jonge loofbomen;jonge loofbomen;loofbomen jong;deciduous young;12
      9996;;oude loofbomen;oude loofbomen;loofbomen oud;deciduous old;13
      998;;naaldbomen;naaldbomen;naaldbomen;conifers;211
      9995;;jonge naaldbomen;jonge naaldbomen;naaldbomen jong;conifers young;212
      9994;;oude naaldbomen;oude naaldbomen;naaldbomen oud;conifers old;213
      51;Quercus robur;zomereik;loofbomen;zomereik;oak;101
      20;Fagus sylvatica;beuk;loofbomen;beuk;beech;102
      53;Quercus rubra;Amerikaanse eik;loofbomen;Amerikaanse eik;northern red oak;103
      947;;overige lbs.;loofbomen;overige lbs.;other deciduous;109
      33;Populus hybrides;populier;loofbomen;overige lbs.;poplar;111
      7;Alnus glutinosa;zwarte els;loofbomen;overige lbs.;black alder;112
      15;Castanea sativa;tamme kastanje;loofbomen;overige lbs.;sweet chestnut;113
      48;Quercus petraea;wintereik;loofbomen;overige lbs.;sessile oak;114
      22;Fraxinus excelsior;es;loofbomen;overige lbs.;ash;115
      5;Acer pseudoplatanus;esdoorn;loofbomen;overige lbs.;sycamore maple;116
      10;Betula pendula;ruwe berk;loofbomen;overige lbs.;silver berch;117
      56;Robinia pseudoacacia;valse acacia;loofbomen;overige lbs.;black locust (false acacia);118
      32;Populus canescens;grauwe abeel;loofbomen;overige lbs.;grey poplar;119
      13;Carpinus betulus;haagbeuk;loofbomen;overige lbs.;hornbeam;120
      8;Alnus incana;witte els;loofbomen;overige lbs.;grey alder;121
      36;Prunus avium;zoete kers;loofbomen;overige lbs.;sweet cherry;122
      11;Betula pubescens;zachte berk;loofbomen;overige lbs.;downy (moor, white) birch;123
      72;Ulmus minor;gladde iep;loofbomen;overige lbs.;field elm;124
      35;Populus tremula;ratelpopulier;loofbomen;overige lbs.;aspen;125
      134;Pinus sylvestris;grove den;naaldbomen;grove den;Scots pine;301
      129;Pinus nigra;Corsicaanse den;naaldbomen;Corsicaanse den;black pine;302
      997;;overige nbs.;naaldbomen;overige nbs.;other conifers;309
      117;Larix kaempferi;Japanse lork;naaldbomen;overige nbs.;Japanese larch;311
      118;Picea abies;fijnspar;naaldbomen;overige nbs.;Norway spruce;312
      136;Pseudotsuga menziesii;douglas;naaldbomen;overige nbs.;douglas fir;313",
      file = file.path(target, "tree_reportmetadata.csv"), append = FALSE)
  return(invisible())
}


#####################################################

#' Generate the SQL files used to communicate with the database
#'
#' @param target target directory where the files should be placed
#'
#' @return sql files created in the target directory
#' @export
#'
generate_sql_files <- function(target = "extdata") {
  #tree_info.SQL
  cat("select
  p.PLOT_ID as PlotKey
      , b.BOOM_ID as BoomKey
      , w.WRNG_ID as WaarnemingKey
      , m.WRME_ID as MetingKey
      , p.PLOT_NUM as PlotNr
      , p.PLOT_NAM as PlotNaam
      , g.GMTE_NAM as Gemeente
      , b.BOOM_BNR as BoomNr
      , w.WRNG_DTE as Datum
      , w.WRNG_JAA as Jaar
      , s.SPEC_EUR_CDE as Soortnummer
      , m.WRME_OMT as Omtrek
      , m.WRME_LFT as Leeftijd
      , m.WRME_UCLT_CDE AS Leeftijdsklasse
      , m.WRME_UCBL_CDE as BladverliesNetto
      , m.WRME_UCNJ_CDE AS AantalNaaldjaargangen
      , m.WRME_UCZZ_CDE AS Zaadzetting
      , m.WRME_UCWS_CDE AS Waterscheuten
      , b.BOOM_JDO as SterfteJaar
      , b.BOOM_REMO_CDE VerwijderdCode
      , cdem.REMO_DES as VerwijderdReden
      , m.WRME_OPM as MetingOpmerking
      from
      tblProefvlak p
      left join tblWaarneming w on w.WRNG_PLOT_ID = p.PLOT_ID
      left join tblWaarnemingMeting m on m.WRME_WRNG_ID = w.WRNG_ID
      left join tblBoom b on b.BOOM_ID = m.WRME_BOOM_ID
      left join tblSoort s on s.SPEC_ID = b.BOOM_SPEC_ID
      left join cdeRemovalsMortality cdem on b.BOOM_REMO_CDE = cdem.REMO_CDE
      left join tblGemeente g on g.GMTE_NIS_CDE = p.PLOT_GMTE_NIS_CDE\n",
      file = file.path(target, "tree_info.SQL"),
      append = FALSE)

  #tree_symptom_info.SQL
  cat("
select
  m.WRME_ID as MetingKey
      , sym.WMSY_ATDE_CDE AangetastDeelCode
      , cad.ATDE_DES AangetastDeel
      , sym.WMSY_SYSP_CDE SymptoomSpecCode
      , css.SYSP_DES SymptoomSpecificatie
      , sym.WMSY_SYTO_CDE SymptoomCode
      , csym.SYTO_DES Symptoom
      , sym.WMSY_UCME_CDE SymptoomMeting
      , sym.WMSY_LEFT_CDE LeeftijdSchadeCode
      , clft.LEFT_DES as LeeftijdSchade
      , oorz.OORZ_CDE SymptoomOorzaakCode
      , oorz.OORZ_DES SymptoomOorzaak
      , ozor.OZOR_CDE SymptoomOrganismeCode
      , ozor.OZOR_NAM SymptoomOrganisme
      , sym.WMSY_OPM SymptoomOpmerking
      from
      tblProefvlak p
      left join tblWaarneming w on w.WRNG_PLOT_ID = p.PLOT_ID
      left join tblWaarnemingMeting m on m.WRME_WRNG_ID = w.WRNG_ID
      left join tblWaarnemingMetingSymptoom sym on sym.WMSY_WRME_ID = m.WRME_ID
      left join tblBoom b on b.BOOM_ID = m.WRME_BOOM_ID
      left join tblSoort s on s.SPEC_ID = b.BOOM_SPEC_I
      left join cdeAangetastDeel cad on cad.ATDE_CDE = sym.WMSY_ATDE_CDE
      left join cdeSymptoomSpecificatie css on css.SYSP_CDE = sym.WMSY_SYSP_CDE
      left join cdeSymptoom csym on csym.SYTO_CDE = sym.WMSY_SYTO_CDE
      left join cdeLeeftijdSchade clft on clft.LEFT_CDE = sym.WMSY_LEFT_CDE
      left join tblMetingSymptoomOorzaak syo on syo.MSYO_WMSY_ID = sym.WMSY_ID
      left join tblOorzaak oorz on oorz.OORZ_ID = syo.MSYO_OORZ_ID
      left join tblOorzaakOrganisme ozor on ozor.OZOR_ID = syo.MSYO_OZOR_ID\n",
      file = file.path(target, "tree_symptom_info.SQL"),
      append = FALSE )

  #natuurindicatoren.sql
  cat("
select
p.PLOT_NUM as PlotNr
, b.BOOM_BNR as BoomNr
, w.WRNG_JAA as Jaar
, s.SPEC_EUR_CDE as Soortnummer
, m.WRME_OMT as Omtrek
, m.WRME_LFT as Leeftijd
, m.WRME_UCBL_CDE as BladverliesNetto
from
tblProefvlak p
left join tblWaarneming w on w.WRNG_PLOT_ID = p.PLOT_ID
left join tblWaarnemingMeting m on m.WRME_WRNG_ID = w.WRNG_ID
left join tblBoom b on b.BOOM_ID = m.WRME_BOOM_ID
left join tblSoort s on s.SPEC_ID = b.BOOM_SPEC_ID",
      file = file.path(target, "natuurindicatoren.sql"),
      append = FALSE)
}

