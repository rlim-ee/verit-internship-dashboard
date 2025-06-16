# INIT----
required_packages <- c("rsconnect","shiny", "sf", "readr", "here", "dplyr", "ggplot2", "plotly", "bslib", "shinydashboard", "tmap", "fresh", "scatterpie", "leaflet", "leaflet.minicharts", "tidyr", "bs4Dash", "shinyBS", "shinyWidgets")
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
  
  # Réactif : filtrage selon la sélection
  data_filtrée <- reactive({
    req(input$selected_villes)
    df <- data_DC_FLAPD %>% filter(ville_groupee %in% input$selected_villes)
    cat("Villes sélectionnées :", paste(input$selected_villes, collapse = ", "), "\n")
    cat("Nombre de points affichés :", nrow(df), "\n")
    return(df)
  })
  
  # Affichage de la carte avec points + légende + zoom
  output$map <- renderLeaflet({
    df <- data_filtrée()
    
    # Cas 1 : une seule ville sélectionnée → affichage enrichi
    if (length(input$selected_villes) == 1) {
      
      # --- Surface : NA gardés comme NA ---
      area_vals <- df$area_m2
      vals_valides <- area_vals[!is.na(area_vals)]
      
      pal <- colorNumeric(
        palette = "YlOrRd",
        domain = df$capacity_e,
        na.color = "lightgray"
      )
      
      rayons <- ifelse(
        is.na(area_vals),
        8,
        scales::rescale(area_vals, to = c(8, 20))
      )
      
      # --- Légende surface adaptative ---
      seuils_legende <- quantile(vals_valides, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
      seuils_legende <- round(seuils_legende)
      rayons_legende <- scales::rescale(seuils_legende, to = c(8, 20))
      
      html_legende <- paste0(
        "<div style='background: white; padding: 8px; border-radius: 5px; font-size: 13px'>
        <b>Surface (m²)</b><br>",
        paste0(
          "<div style='height: ", 2 * rayons_legende, "px; margin-bottom: 4px;'>
           <svg width='", 2 * max(rayons_legende), "' height='", 2 * rayons_legende, "'>
             <circle cx='", rayons_legende, "' cy='", rayons_legende, "' r='", rayons_legende, "' 
                     fill='lightgray' stroke='black' stroke-width='1'/>
           </svg> ",
          format(seuils_legende, big.mark = " "), " m²
         </div>"
          , collapse = ""),
        "<div style='margin-top:6px'>
         <svg width='20' height='20'>
           <circle cx='10' cy='10' r='5' fill='lightgray' stroke='black' stroke-width='1'/>
         </svg> Donnée manquante
       </div>
       </div>"
      )
      
      leaflet(df) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          radius = rayons,
          fillColor = ~pal(capacity_e),
          color = "black",
          weight = 2,
          stroke = TRUE,
          fillOpacity = 0.9,
          popup = ~paste0(
            "<b>Nom :</b> ", name, "<br>",
            "<b>Ville :</b> ", city, "<br>",
            "<b>Groupe :</b> ", ville_groupee, "<br>",
            "<b>Adresse :</b> ", adress, "<br>",
            "<b>Surface :</b> ", area_m2, " m²<br>",
            "<b>Capacité :</b> ", capacity_e, " MW<br>",
            "<b>Entreprise :</b> ", company
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = df$capacity_e,
          title = "Capacité (MW)",
          opacity = 0.8,
          na.label = "Non renseignée",
          labFormat = labelFormat()
        ) %>%
        addControl(
          html = html_legende,
          position = "bottomleft"
        ) %>%
        fitBounds(
          lng1 = min(df$longitude, na.rm = TRUE),
          lat1 = min(df$latitude, na.rm = TRUE),
          lng2 = max(df$longitude, na.rm = TRUE),
          lat2 = max(df$latitude, na.rm = TRUE)
        )
      
    } else {
      # Cas 2 : plusieurs villes → clustering simplifié
      leaflet(df) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addMarkers(
          lng = ~longitude,
          lat = ~latitude,
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
        ) %>%
        fitBounds(
          lng1 = min(df$longitude, na.rm = TRUE),
          lat1 = min(df$latitude, na.rm = TRUE),
          lng2 = max(df$longitude, na.rm = TRUE),
          lat2 = max(df$latitude, na.rm = TRUE)
        )
    }
  })
  outputOptions(output, "map", suspendWhenHidden = FALSE)
  
}
  

