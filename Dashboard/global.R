# INIT----
required_packages <- c("rsconnect","shiny", "sf", "readr","ggtext", "here", "dplyr", "ggplot2", "plotly", "bslib", "shinydashboard", "fresh", "scatterpie", "leaflet", "leaflet.minicharts", "tidyr", "bs4Dash", "shinyBS", "shinyWidgets")
missing <- required_packages[!required_packages %in% installed.packages()]
if(length(missing)) install.packages(missing)
lapply(required_packages, library, character.only = TRUE)


{
  library(here)
  library(shiny)
  library(bslib)
  library(shinydashboard)
  library(sf)
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(fresh)
  library(scatterpie)
  library(leaflet)
  library(leaflet.minicharts)
  library(plotly)
  library(tidyr)
  library(bs4Dash)
  library(bslib)
  library(shinyBS)
  library(rsconnect)
  library(shinyWidgets)
  library(scales)
  library(ggtext)
}



dc_europe <- readRDS(here("dashboard", "data", "dc_europe.rds"))
europe_map <- readRDS(here("dashboard", "data", "europe_map.rds"))
regions <- readRDS(here("dashboard", "data", "regions_simplified.rds"))
data_prod <- readRDS(here("dashboard", "data", "conso_evo.rds"))
data_ara <- readRDS(here("dashboard", "data", "data_ara_epci.rds"))
eol_aura <- readRDS(here("dashboard", "data", "eol_aura.rds"))
sol_aura <- readRDS(here("dashboard", "data", "sol_aura.rds"))
nuc_aura <- readRDS(here("dashboard", "data", "nuc_aura.rds"))
hydro_aura <- readRDS(here("dashboard", "data", "hydro_aura.rds"))

data_DC_FLAPD <- st_read(here(
  "dashboard",
  "data", "data_DC_FLAPD", "DC_FLAP_D.shp"))

data_DC_FLAPD$ville_groupee <- case_when(
  data_DC_FLAPD$city %in% c("Paris", "Saint-Denis", "Courbevoie", "Ivry-sur-Seine", "Pantin", "Aubervilliers", "Montreuil", "Clichy", "Vitry-sur-Seine", "Roissy-en-France", "Nanterre", "Les Ulis", "Nozay", "Villepinte") ~ "Paris",
  data_DC_FLAPD$city %in% c("London", "Slough", "Hounslow", "Hayes", "Feltham", "Wembley", "Watford", "Southall", "Enfield", "Crawley") ~ "London",
  data_DC_FLAPD$city %in% c("Amsterdam", "Schiphol", "Hoofddorp", "Schiphol-Rijk", "Aalsmeer", "Oude Meer", "Diemen") ~ "Amsterdam",
  data_DC_FLAPD$city %in% c("Frankfurt am Main", "Frankfurt", "Eschborn", "Offenbach", "Neu-Isenburg", "Dietzenbach", "Raunheim", "Hanau", "Langen") ~ "Frankfurt",
  data_DC_FLAPD$city %in% c("Dublin", "Clonshaugh", "Blanchardstown", "Ballycoolin", "Clonee", "Clondalkin", "Mulhuddart", "Tallaght", "Ballybane") ~ "Dublin",
  TRUE ~ NA_character_
)

data_DC_FLAPD <- data_DC_FLAPD %>%
  filter(!is.na(latitude), !is.na(longitude))