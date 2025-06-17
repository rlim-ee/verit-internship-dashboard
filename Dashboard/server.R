# INIT----
required_packages <- c("rsconnect","shiny", "sf", "readr", "here", "dplyr", "ggplot2", "plotly", "bslib", "shinydashboard", "tmap", "fresh", "scatterpie", "leaflet", "leaflet.minicharts", "tidyr", "bs4Dash", "shinyBS", "shinyWidgets","htmlwidgets")
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
  library(htmlwidgets)
}


# SERVER ----
server <- function(input, output, session) {
  
  ## Navigation Event Observers----
  
  observeEvent(input$go_dc_europe, {
    updateTabItems(session, inputId = "tabs", selected = "dc_europe_map")
  })
  
  observeEvent(input$go_flapd, {
    updateTabItems(session, inputId = "tabs", selected = "flapd")
  })
  
  observeEvent(input$go_dc_france, {
    updateTabItems(session, inputId = "tabs", selected = "dc_france")
  })
  
  observeEvent(input$retour_accueil_dc_europe, {
    updateTabItems(session, inputId = "tabs", selected = "home")
  })
  
  observeEvent(input$retour_accueil_flapd, {
    updateTabItems(session, inputId = "tabs", selected = "home")
  })
  
  observeEvent(input$retour_accueil_dc_france, {
    updateTabItems(session, inputId = "tabs", selected = "home")
  })
  
  observeEvent(input$retour_accueil_regions, {
    updateTabItems(session, inputId = "tabs", selected = "home")
  })
  
  observeEvent(input$retour_accueil_ara, {
    updateTabItems(session, inputId = "tabs", selected = "home")
  })
  
  ## 1.2 FLAP_D----
  
  # Extraire les coordonnées
  coords <- st_coordinates(data_DC_FLAPD)
  data_DC_FLAPD$longitude <- coords[, 1]
  data_DC_FLAPD$latitude <- coords[, 2]
  
  # Regrouper les villes proches
  data_DC_FLAPD <- data_DC_FLAPD %>%
    mutate(ville_groupee = case_when(
      city %in% c("Paris", "Saint-Denis", "Courbevoie", "Ivry-sur-Seine", "Pantin", "Aubervilliers", "Montreuil", "Clichy", "Vitry-sur-Seine", "Roissy-en-France", "Nanterre", "Les Ulis", "Nozay", "Villepinte", "Velizy", "Antony") ~ "Paris",
      city %in% c("London", "Slough", "Hounslow", "Hayes", "Feltham", "Wembley", "Watford", "Southall", "Enfield", "Crawley") ~ "London",
      city %in% c("Amsterdam", "Hoofddorp", "Rozenburg", "Diemen", "Schiphol", "Aalsmeer", "Oude Meer", "Schiphol-Rijk") ~ "Amsterdam",
      city %in% c("Frankfurt", "Frankfurt am Main", "Eschborn", "Offenbach", "Neu-Isenburg", "Dietzenbach", "Raunheim", "Hanau", "Langen", "Hattersheim") ~ "Frankfurt",
      city %in% c("Dublin", "Clonshaugh", "Blanchardstown", "Ballycoolin", "Clonee", "Clondalkin", "Mulhuddart", "Tallaght", "Ballybane", "Kilcarbery") ~ "Dublin",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(ville_groupee))
  
  # Valeur réactive pour la ville sélectionnée
  selected_ville <- reactiveVal("All")
  
  # Réactif : filtrage selon la sélection
  data_filtrée <- reactive({
    if (selected_ville() == "All") {
      data_DC_FLAPD
    } else {
      data_DC_FLAPD %>% filter(ville_groupee == selected_ville())
    }
  })
  
  
  # Affichage de la carte avec points + légende + zoom
  output$map <- renderLeaflet({
    df <- data_filtrée()
    req(nrow(df) > 0)
    
    is_detailed <- selected_ville() != "All"
    pal <- colorNumeric(
      palette = colorRampPalette(c("#fee0d2", "#fc9272", "#de2d26", "#a50f15"))(100),
      domain = df$capacity_e,
      na.color = "lightgray"
    )
    area_vals <- df$area_m2
    rayons <- ifelse(is.na(area_vals), 10, scales::rescale(area_vals, to = c(12, 24)))
    
    # Ajouter jitter si mêmes coordonnées
    df <- df %>%
      group_by(longitude, latitude) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      mutate(
        longitude_jitter = ifelse(n > 1, longitude + runif(n(), -0.002, 0.002), longitude),
        latitude_jitter  = ifelse(n > 1, latitude + runif(n(), -0.002, 0.002), latitude)
      )
    
    map <- leaflet(df) %>%
      addProviderTiles("CartoDB.Positron")
    
    if (is_detailed) {
      map <- map %>%
        addCircleMarkers(
          lng = ~longitude_jitter, lat = ~latitude_jitter, radius = rayons,
          fillColor = ~pal(capacity_e), color = "black", weight = 2,
          stroke = TRUE, fillOpacity = 0.9,
          popup = ~paste0(
            "<b>Nom :</b> ", name, "<br>",
            "<b>Ville :</b> ", city, "<br>",
            "<b>Groupe :</b> ", ville_groupee, "<br>",
            "<b>Adresse :</b> ", adress, "<br>",
            "<b>Surface :</b> ", area_m2, " m²<br>",
            "<b>Capacité :</b> ", capacity_e, " MW<br>",
            "<b>Entreprise :</b> ", company
          )
        )
      
      # Légende cercles concentriques
      vals_valides <- df$area_m2[!is.na(df$area_m2)]
      if (length(vals_valides) > 0) {
        seuils <- quantile(vals_valides, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
        rayons_legende <- scales::rescale(seuils, to = c(8, 20))
        
        html_legende <- paste0(
          "<div style='background: white; padding: 8px; border-radius: 5px; font-size: 13px'><b>Surface (m²)</b><br>",
          paste0(
            mapply(function(r, seuil) {
              paste0(
                "<div style='display: flex; align-items: center; margin-bottom: 4px;'>",
                "<svg width='", 2 * max(rayons_legende), "' height='", 2 * r, "' style='overflow: visible;'>",
                "<circle cx='", max(rayons_legende), "' cy='", r, "' r='", r, "' fill='lightgray' stroke='black' stroke-width='1'/></svg>",
                "<span style='margin-left: 8px;'>", format(round(seuil), big.mark = " "), " m²</span></div>"
              )
            }, r = rayons_legende, seuil = seuils), collapse = ""),
          "<div style='display: flex; align-items: center; margin-top: 6px;'>",
          "<svg width='20' height='20'><circle cx='10' cy='10' r='5' fill='lightgray' stroke='black' stroke-width='1'/></svg>",
          "<span style='margin-left: 8px;'>N/A</span></div></div>"
        )
        
        map <- map %>% addControl(html = html_legende, position = "bottomright")
      }
      
      map <- map %>%
        addLegend(position = "bottomright", pal = pal, values = df$capacity_e,
                  title = "Puissance (MW)", opacity = 0.8, na.label = "N/A")
      
    } else {
      map <- map %>%
        addMarkers(
          lng = ~longitude_jitter, lat = ~latitude_jitter,
          clusterOptions = markerClusterOptions(),
          popup = ~paste0(
            "<b>Nom :</b> ", name, "<br>",
            "<b>Ville :</b> ", city, "<br>",
            "<b>Groupe :</b> ", ville_groupee, "<br>",
            "<b>Adresse :</b> ", adress, "<br>",
            "<b>Surface :</b> ", area_m2, " m²<br>",
            "<b>Capacité :</b> ", capacity_e, " MW<br>",
            "<b>Entreprise :</b> ", company
          )
        )
    }
    
    map %>% fitBounds(min(df$longitude_jitter), min(df$latitude_jitter), max(df$longitude_jitter), max(df$latitude_jitter))
  })
  
  
  # Boutons pour chaque ville
  observeEvent(input$go_paris,     { selected_ville("Paris") })
  observeEvent(input$go_london,    { selected_ville("London") })
  observeEvent(input$go_amsterdam, { selected_ville("Amsterdam") })
  observeEvent(input$go_frankfurt, { selected_ville("Frankfurt") })
  observeEvent(input$go_dublin,    { selected_ville("Dublin") })
  
  # Bouton "Vue globale"
  observeEvent(input$reset_vue, {
    selected_ville("All")
    leafletProxy("map") %>% setView(lng = 5, lat = 51, zoom = 5)
    showNotification("Vue globale restaurée", type = "message")
  })
  
  
  outputOptions(output, "map", suspendWhenHidden = FALSE)
  
}
  

