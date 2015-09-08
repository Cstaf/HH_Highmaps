# MAin
# Main


############### Funktion för att testa vilken miljö vi sitter i ################
.is.inca <- function(){
  unname(!(Sys.info()["sysname"] == "Darwin"))
}

if (!.is.inca()){
  setwd("~/Documents/Github/HH_Highmaps")
  rm(list=ls())
  df <- read.csv2("HH.txt")
}


################################# Ladda paket ##################################
library(dplyr)
library(relsurv)
library(ggplot2)
library(gdata)
library(stringr)


############################### Ladda funktioner ###############################
if (!.is.inca()) source('~/Documents/Github/HH_Highmaps/fun_HM_Sweden.R')
# INCA
if (.is.inca()) source('D:/R-Scripts/Väst/Oc5hoer/funktioner/fun_HM_Sweden.R', encoding = "UTF-8")



############################ Laddning av parametrar ############################
if (!.is.inca()) Från <- 2010
if (!.is.inca()) Till <- 2013
if (!.is.inca()) Diagnos <- "Samtliga diagnoser"
if (!.is.inca()) Stadie <- "Samtliga stadier"
if (!.is.inca()) Stratum <- "Aggregerat"
if (!.is.inca()) Relativ <- "Relativ överlevnad"
if (!.is.inca()) CI <- "Nej"
if (!.is.inca()) Minålder <- 18
if (!.is.inca()) Maxålder <- 110
# INCA
if (.is.inca()) Från <- as.numeric(param[["Från"]])
if (.is.inca()) Till <- as.numeric(param[["Till"]])
if (.is.inca()) Diagnos <- as.character(param[["Diagnos"]])
if (.is.inca()) Stadie <- as.character(param[["Stadie"]])
if (.is.inca()) Stratum <- as.character(param[["Stratum"]])
if (.is.inca()) Relativ <- as.character(param[["Relativ"]])
if (.is.inca()) CI <- as.character(param[["CI"]])
if (.is.inca()) Minålder <- as.numeric(param[["Minålder"]])
if (.is.inca()) Maxålder <- as.numeric(param[["Maxålder"]])




########################### Förberadande bearbetning ###########################
names(df) <- tolower(names(df))
df_HH <- df %>% 
  mutate(stadie_grupp = str_trim(substring(as.character(a_stadium_beskrivning),1,3)),
         diagnos_grupp = str_trim(gsub("[[:digit:]]","", a_icd10_gruppnamn))) %>% 
  filter(region_namn != "Region Demo",
         stadie_grupp %in% c("I", "II", "III", "IV", "-"),
         diagnos_grupp != "",
         a_diadat != "",
         !is.na(a_alder),
         !is.na(a_lkf)
  )

############################ Filtrering på stadie #############################
if (!("Samtliga stadier" %in% Stadie)) df_HH <- df_HH %>% 
  filter(stadie_grupp %in% Stadie)
Stadie <- if (!("Samtliga stadier" %in% Stadie)) {
  paste(Stadie, collapse=",")
} else {
  "Samtliga stadier"
}



############################ Filtrering på diagnos #############################
if (!("Samtliga diagnoser" %in% Diagnos)) df_HH <- df_HH %>%
  filter(diagnos_grupp %in% Diagnos)
Diagnos <- if (!("Samtliga diagnoser" %in% Diagnos)) {
  paste(Diagnos, collapse=",")
} else {
  "Samtliga diagnoser"
}


######################### Skapa text för valda urvalet #########################
Titel <- paste0("Antal fall")
Urval <- paste0("(Urval: Diagnosår: ",Från,"-",Till,", Diagnos: ",Diagnos,", Stadie: ", Stadie, ", Ålder: ", Minålder, "-",Maxålder, ")")





################################################################################
#                                                                              #
#                              Create the highmap                              #
#                                                                              #
################################################################################


################################# Not on INCA ##################################
if (!.is.inca()){
  HM_Sweden(df_HH,
            "a_lkf", 
            "region_namn",
            title = Titel,
            subtitle = Urval,
            script_municipal_code = "kommuner.csv",
            script_part1_code = "del1.txt",
            script_jquery = "js/jquery.min.js",
            script_highmaps = "js/highmaps.js",
            script_drilldown = "js/drilldown.js",
            script_maps_swe = "maps/swe.js",
            script_maps_vast = "maps/vast.js",
            script_maps_norr = "maps/norr.js",
            script_maps_syd = "maps/syd.js",
            script_maps_so = "maps/so.js",
            script_maps_u = "maps/u.js",
            script_maps_sg = "maps/sg.js")
}



################################### On INCA ####################################
if (.is.inca()){
  HM_Sweden(df_HH,
            "a_lkf", 
            "region_namn",
            title = Titel,
            subtitle = Urval,
            script_municipal_code = "D:/R-Scripts/Väst/oc5hoer/highmaps/Antal fall/kommuner.csv",
            script_part1_code = "D:/R-Scripts/Väst/oc5hoer/highmaps/Antal fall/del1.txt",
            script_jquery = "/Public/Files/Huvud-_och_halscancer/highmaps/js/jquery.min.js",
            script_highmaps = "/Public/Files/Huvud-_och_halscancer/highmaps/js/highmaps.js",
            script_drilldown = "/Public/Files/Huvud-_och_halscancer/highmaps/js/drilldown.js",
            script_maps_swe = "/Public/Files/Huvud-_och_halscancer/highmaps/maps/swe.js",
            script_maps_vast = "/Public/Files/Huvud-_och_halscancer/highmaps/maps/vast.js",
            script_maps_norr = "/Public/Files/Huvud-_och_halscancer/highmaps/maps/norr.js",
            script_maps_syd = "/Public/Files/Huvud-_och_halscancer/highmaps/maps/syd.js",
            script_maps_so = "/Public/Files/Huvud-_och_halscancer/highmaps/maps/so.js",
            script_maps_u = "/Public/Files/Huvud-_och_halscancer/highmaps/maps/u.js",
            script_maps_sg = "/Public/Files/Huvud-_och_halscancer/highmaps/maps/sg.js")
}


