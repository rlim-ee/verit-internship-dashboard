# INIT----
required_packages <- c("shinyjs", "shinycssloaders", "htmlwidgets", "DT", "networkD3", "shiny", "sf", "readr", "here", "dplyr", "ggplot2", "plotly", "bslib", "shinydashboard", "fresh", "leaflet", "tidyr", "bs4Dash", "shinyWidgets")
missing <- required_packages[!required_packages %in% installed.packages()]
if(length(missing)) install.packages(missing)
lapply(required_packages, library, character.only = TRUE)


{
  library(here)
  library(shiny)
  library(shinyjs)
  library(bslib)
  library(shinydashboard)
  library(sf)
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(fresh)
  library(leaflet)
  library(plotly)
  library(tidyr)
  library(bs4Dash)
  library(shinyWidgets)
  library(scales)
  library(DT)
  library(networkD3)
  library(htmlwidgets)
  library(shinycssloaders)
}

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}


dc_europe <- readRDS(here("dashboard", 
  "data", "dc_europe.rds"))
europe_map <- readRDS(here("dashboard", 
                           "data", "europe_map.rds"))
regions <- readRDS(here("dashboard", 
                        "data", "regions_simplified.rds"))
data_prod <- readRDS(here("dashboard", 
                          "data", "conso_evo.rds"))
data_ara <- readRDS(here("dashboard", 
                         "data", "data_ara_epci.rds"))
eol_aura <- readRDS(here("dashboard", 
                         "data", "eol_aura.rds"))
sol_aura <- readRDS(here("dashboard", 
                         "data", "sol_aura.rds"))
nuc_aura <- readRDS(here("dashboard", 
                         "data", "nuc_aura.rds"))
hydro_aura <- readRDS(here("dashboard", 
                           "data", "hydro_aura.rds"))
extraction <- readRDS(here("dashboard", 
                           "data", "extraction_simplifie.rds"))
extraction <- extraction %>%
  st_make_valid() %>%  # Corrige les géométries invalides
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"), quiet = TRUE)

data_DC_FLAPD <- readRDS(here("dashboard",
                              "data", "data_DC_FLAPD.rds"))

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

# Calculé une seule fois au lancement de l'app
semi_centroids <- extraction %>%
  filter(sc_plants > 0) %>%
  st_centroid()

semi_coords <- st_coordinates(semi_centroids)

# Coordonnées manuelles des pays impliqués (précises et ajustées)
pays_coords <- tibble::tribble(
  ~name, ~lon, ~lat,
  "Norway", 10.75, 59.91,
  "Netherlands", 5.29, 52.13,
  "Sweden", 18.07, 59.33,
  "Turkey", 35.24, 39.06,
  "Germany", 10.45, 51.16,
  "Italy", 12.57, 41.87,
  "Spain", -3.75, 40.46,
  "Poland", 19.14, 52.23,
  "Denmark", 10.0, 56.26,
  "Finland", 25.75, 61.92,
  "Japan", 138.25, 36.20,
  "China", 104.19, 35.86,
  "Republic of Korea", 127.77, 35.91,
  "Malaysia", 101.98, 4.21,
  "Taiwan", 120.96, 23.70,
  "India", 78.96, 20.59,
  "Brazil", -51.93, -14.24,
  "Thailand", 100.99, 15.87,
  "Indonesia", 113.92, -0.79,
  "South Africa", 22.94, -30.56,
  "Botswana", 25.92, -22.33,
  "Zimbabwe", 29.15, -19.01,
  "Namibia", 18.49, -22.56,
  "Gabon", 11.60, -0.80,
  "Australia", 133.78, -25.27,
  "Russian Federation", 105.32, 61.52,
  "United States of America", -98.35, 39.50,
  "Mexico", -102.55, 23.63,
  "Bhutan", 90.43, 27.51,
  "Saudi Arabia", 45.08, 23.88,
  "Kazakhstan", 66.92, 48.02,
  "Belgium", 4.47, 50.50,
  "Austria", 13.33, 47.52,
  "France", 2.21, 46.22,
  "Canada", -106.35, 56.13
)