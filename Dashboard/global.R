# INIT----
required_packages <- c("rsconnect","shiny", "sf", "readr","ggtext", "here", "dplyr", "ggplot2", "plotly", "bslib", "shinydashboard", "tmap", "fresh", "scatterpie", "leaflet", "leaflet.minicharts", "tidyr", "bs4Dash", "shinyBS", "shinyWidgets")
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
  library(tmap)
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
regions <- readRDS(here("dashboard", "data", "region_energie.rds"))
data_prod <- readRDS(here("dashboard", "data", "conso_evo.rds"))
data_ara <- readRDS(here("dashboard", "data", "data_ara_epci.rds"))
eol_aura <- readRDS(here("dashboard", "data", "eol_aura.rds"))
sol_aura <- readRDS(here("dashboard", "data", "sol_aura.rds"))
nuc_aura <- readRDS(here("dashboard", "data", "nuc_aura.rds"))
hydro_aura <- readRDS(here("dashboard", "data", "hydro_aura.rds"))