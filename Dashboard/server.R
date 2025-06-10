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
  observeEvent(input$go_regions, {
    updateTabItems(session, inputId = "tabs", selected = "regions")
  })
  
  observeEvent(input$go_ara, {
    updateTabItems(session, inputId = "tabs", selected = "ara")
  })
  
  observeEvent(input$go_sim1, {
    updateTabItems(session, inputId = "tabs", selected = "sim1")
  })
  
  observeEvent(input$go_sim2, {
    updateTabItems(session, inputId = "tabs", selected = "sim2")
  })
  
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
  
  
  
  
  
  
  
  ## 1.1----
  ### Map1 - Carte des data centers----
  output$map1 <- renderTmap({
    
    # Calcul de l'indicateur
    dc_europe <- dc_europe %>%
      mutate(
        dc_per_million = round((nb_dc / (pop / 1e6)), 1),
        pop = format(pop, big.mark = " ", scientific = FALSE),
        nb_dc = format(nb_dc, big.mark = " ", scientific = FALSE),
      )
    
    # Jointure
    europe_map <- europe_map %>%
      left_join(dc_europe, by = "name")
    
    # Bbox centré sur l'Europe
    bbox_europe <- st_bbox(c(
      xmin = 3000000, ymin = 2500000, 
      xmax = 6000000, ymax = 5000000),
      crs = st_crs(europe_map))
    
    
    # Carte
    tm_shape(europe_map, bbox = bbox_europe) +
      tm_polygons("dc_per_million",
                  palette = "YlOrRd",
                  title = "DC / million hab.",
                  style = "quantile",
                  colorNA = "grey80",
                  textNA = "No Data",
                  border.col = "white",
                  border.lwd = 0.5,
                  popup.vars = c(
                    "Pays" = "name",
                    "Nombre de DC" = "nb_dc",
                    "Population" = "pop",
                    "DC / million hab." = "dc_per_million"
                  )
      ) + 
      tm_bubbles("nb_dc",
                 col = "darkblue",
                 alpha = 0.6,
                 scale = 3,
                 border.col = "white",
                 border.lwd = 0.5,
                 title.col = "Nombre de DC",
                 title.size = "Nombre de DC",
                 orientation = 'v',
                 popup.vars = c(
                   "Pays" = "name",
                   "Nombre de DC" = "nb_dc",
                   "Population" = "pop",
                   "DC / million hab." = "dc_per_million"
                 )) +
      tm_compass(position = c("right", "top"), size = 0.1) +
      tm_scale_bar(position = c("left", "bottom")) +
      tm_credits("Auteur : Robert Lim\nSource : Eurostat, DataCenterMap, 2025",
                 position = c("right", "bottom"),
                 size = 0.7,
                 fontface = "italic") +
      tm_layout(title = "Répartition des data centres en Europe",
                title.size = 1.2,
                title.fontface = "bold",
                title.color = "gray20",
                title.fontfamily = "sans",
                title.bg.color = "white",
                title.bg.alpha = 0.7,
                frame = FALSE,
                legend.outside = FALSE,
                legend.show = TRUE)
  })
  
  
  ## BarPlot1 - share of dc
  # Exemple de données
  data_centres <- data.frame(
    Country = c("Allemagne", "Royaume-Uni", "France", "Pays-Bas", "Italie",
                "Espagne", "Suisse", "Pologne", "Suède", "Belgique",
                "Irlande", "Autriche", "Norvège", "Danemark", "Reste de l'Europe"),
    Share = c(18.5, 17.0, 13.0, 9.5, 7.0,
              6.0, 5.0, 4.5, 4.0, 3.8,
              3.2, 3.0, 2.8, 2.5, 10.2)
  )
  
  # Trier du plus grand au plus petit sauf "Reste de l'Europe", qu'on met en dernier
  data_centres <- data_centres %>%
    filter(Country != "Reste de l'Europe") %>%
    arrange(Share) %>%
    bind_rows(data_centres %>% filter(Country == "Reste de l'Europe")) %>%
    mutate(Country = factor(Country, levels = Country))  # conserver l'ordre
  
  
  output$barPlot <- renderPlotly({
    plot_ly(data_centres,
            x = ~Share,
            y = ~Country,
            type = 'bar',
            orientation = 'h',
            marker = list(color = ~Share,
                          colorscale = 'Rainbow'),
            hoverinfo = 'x+y') %>%
      layout(
        xaxis = list(title = "", ticksuffix = "%", range = c(0, 20)),
        yaxis = list(title = ""),
        margin = list(l = 100, r = 30, t = 30, b = 30),
        plot_bgcolor = '#f0f2f5',
        paper_bgcolor = '#f0f2f5',
        font = list(size = 14),
        annotations = list(
          list(
            x = 0,
            y = -0.2,
            xref = 'paper',
            yref = 'paper',
            showarrow = FALSE,
            text = "Source : ICIS, basé sur DataCentreMap et Statista",
            font = list(size = 12, color = 'gray')
          )
        )
      )
  })
  
  
  
  
  ### Map2 - Carte de typologie----
  output$map2 <- renderTmap({
    
    groupes_pays <- data.frame(
      color_code = c("DEU", "GBR", "FRA", "NLD", "BEL",
                     "NOR", "SWE", "DNK", "FIN", "EST", "CHE", "AUT",
                     "ITA", "ESP", "GRC", "PRT", "CYP",
                     "POL", "HUN", "HRV", "ROM", "BGR",
                     "LVA", "LTU", "CZE", "SVK", "SVN",
                     "IRL"),
      groupe = c(rep("1", 5),
                 rep("2", 7),
                 rep("3", 5),
                 rep("4", 5),
                 rep("5", 5),
                 "Irlande")
    )
    
    europe_map2 <- europe_map %>%
      left_join(groupes_pays, by = "color_code")
    
    bbox_europe <- st_bbox(c(
      xmin = 3000000, ymin = 2500000, 
      xmax = 6000000, ymax = 5000000),
      crs = st_crs(europe_map))
    
    
    custom_colors <- c(
      "1" = "#A3232B",
      "2" = "#263B46",
      "3" = "#D89839",
      "4" = "#BC632E",
      "5" = "#D7C998",
      "Irlande"  = "#04BBFF"
    )
    
    tm_shape(europe_map2, bbox = bbox_europe) +
      tm_polygons("groupe",
                  palette = custom_colors,
                  title = "Typologies des pays",
                  textNA = "Pas de données",
                  colorNA = "#E1E1E1",
                  border.col = "white",
                  popup.vars = c(
                    "Pays" = "name",
                    "Groupe " = "groupe"
                  )) +
      tm_layout(title = "Typologie des pays européens selon l'ACP",
                title.size = 1.2,
                title.fontface = "bold",
                title.color = "gray20",
                title.fontfamily = "sans",
                title.bg.color = "white",
                title.bg.alpha = 0.7,
                frame = FALSE,
                legend.outside = TRUE) +
      tm_compass(position = c("right", "top"), size = 0.1) +
      tm_scale_bar(position = c("left", "bottom")) +
      tm_credits("Auteur : Robert Lim\nSource : Eurostat, DataCenterMap, 2025",
                 position = c("right", "bottom"),
                 size = 0.7,
      )
  })
  
  
  ### suite dc europe----
  output$dc_demand_plot <- renderPlot({
    data <- data.frame(
      Année = c("2024", "2035"),
      Demande_TWh = c(96, 236)
    )
    
    ggplot(data, aes(x = Année, y = Demande_TWh, fill = Année)) +
      geom_col(width = 0.5, show.legend = FALSE) +
      geom_text(aes(label = paste0(Demande_TWh, " TWh")), vjust = -0.5, size = 5) +
      scale_fill_manual(values = c("2024" = "#3182bd", "2035" = "#de2d26")) +
      labs(title = "Croissance de la demande énergétique des data centres en Europe",
           y = "Demande en TWh",
           x = NULL) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"),
            axis.text.x = element_text(face = "bold"))
  })
  
  
  ## 2.1----
  ### Map3 - Carte Prodction d'énergie----
  output$map3 <- renderTmap({
    
    # Reprojection en WGS 84 si ce n'est pas déjà fait
    regions_wgs84 <- st_transform(regions, 4326)
    
    # Calcul des centroïdes avec coordonnées X/Y
    centroids <- st_centroid(regions_wgs84)
    centroids$X <- st_coordinates(centroids)[,1]
    centroids$Y <- st_coordinates(centroids)[,2]
    
    # Mode interactif (pour tmap dans Shiny)
    tmap_mode("view")
    
    tm_basemap("CartoDB.Positron") +
      tm_shape(regions_wgs84) +
      tm_polygons(
        fill = "white",
        alpha = 0.7,
        border.col = "black",
        border.lwd = 1,
        popup.vars = c("Région" = "NOM", "Consommation totale (TWh)" = "CONSO_TWH")
      ) +
      tm_shape(centroids) +
      tm_symbols(size = "CONSO_TWH", 
                 scale = 3, 
                 col = "#0B162C", 
                 alpha = 0.7, 
                 border.col = "black",
                 popup.vars = c("Région" = "NOM", "Consommation totale (TWh)" = "CONSO_TWH")
      ) +
      tm_view(set.view = c(2.2137, 46.7276, 5),
              view.legend.position = c("right", "bottom"),
              basemaps = c("CartoDB.Positron", "OpenStreetMap")) +
      tm_layout(title = "Consommation totale brute par région",
                title.size = 1.2,
                title.fontface = "bold",
                title.color = "gray20",
                title.fontfamily = "sans",
                title.bg.color = "white",
                title.bg.alpha = 0.7,
                frame = FALSE,
                legend.outside = TRUE) +
      tm_compass(position = c("right", "top"), size = 0.1) +
      tm_scale_bar(position = c("left", "bottom")) +
      tm_credits("Auteur : Robert Lim\nSource : RTE France, 2025",
                 position = c("right", "bottom"),
                 size = 0.7
      )
  })
  
  
  
  ### Map3bis - Production totale----
  output$map3bis <- renderTmap({
    
    # Reprojection en WGS 84 si ce n'est pas déjà fait
    regions_wgs84 <- st_transform(regions, 4326)
    
    # Calcul des centroïdes avec coordonnées X/Y
    centroids <- st_centroid(regions_wgs84)
    centroids$X <- st_coordinates(centroids)[,1]
    centroids$Y <- st_coordinates(centroids)[,2]
    
    # Mode interactif
    tmap_mode("view")
    
    tm_basemap("CartoDB.Positron") +
      tm_shape(regions_wgs84) +
      tm_polygons(
        fill = "white",
        alpha = 0.7,
        border.col = "black",
        border.lwd = 1,
        popup.vars = c("Région" = "NOM", "Production totale (TWh)" = "PR_TOT_TWH")
      ) +
      tm_shape(centroids) +
      tm_symbols(size = "PR_TOT_TWH", 
                 scale = 3, 
                 col = "#1A5D1A", 
                 alpha = 0.7, 
                 border.col = "black",
                 popup.vars = c("Région" = "NOM", "Production totale (TWh)" = "PR_TOT_TWH")
      ) +
      tm_view(set.view = c(2.2137, 46.7276, 5),
              view.legend.position = c("right", "bottom"),
              basemaps = c("CartoDB.Positron", "OpenStreetMap")) +
      tm_layout(title = "Production totale d'énergie par région",
                title.size = 1.2,
                title.fontface = "bold",
                title.color = "gray20",
                title.fontfamily = "sans",
                title.bg.color = "white",
                title.bg.alpha = 0.7,
                frame = FALSE,
                legend.outside = TRUE) +
      tm_compass(position = c("right", "top"), size = 0.1) +
      tm_scale_bar(position = c("left", "bottom")) +
      tm_credits("Auteur : Robert Lim\nSource : RTE France, 2025",
                 position = c("right", "bottom"),
                 size = 0.7
      )
  })
  
  
  
  output$map_totale <- renderTmap({
    req(input$choix_map)
    
    regions_wgs84 <- st_transform(regions, 4326)
    centroids <- st_centroid(regions_wgs84)
    centroids$X <- st_coordinates(centroids)[,1]
    centroids$Y <- st_coordinates(centroids)[,2]
    
    # Dépend de l’indicateur choisi
    if (input$choix_map == "conso") {
      var <- "CONSO_TWH"
      titre <- "Consommation totale brute par région"
      couleur <- "#6A645A"
      popup_label <- "Consommation totale (TWh)"
    } else {
      var <- "PR_TOT_TWH"
      titre <- "Production totale d’énergie par région"
      couleur <- "#E3CD8B"
      popup_label <- "Production totale (TWh)"
    }
    
    tmap_mode("view")
    
    tm_basemap("CartoDB.Positron") +
      tm_shape(regions_wgs84) +
      tm_polygons(
        fill = "white",
        alpha = 0.7,
        border.col = "black",
        border.lwd = 1,
        popup.vars = c("Région" = "NOM", popup_label = var)
      ) +
      tm_shape(centroids) +
      tm_symbols(size = var,
                 scale = 4,
                 col = couleur,
                 alpha = 0.7,
                 border.col = "black",
                 popup.vars = c("Région" = "NOM", popup_label = var)
      ) +
      tm_view(set.view = c(2.2137, 46.7276, 5),
              view.legend.position = c("right", "bottom"),
              basemaps = c("CartoDB.Positron", "OpenStreetMap")) +
      tm_layout(title = titre,
                title.size = 1.2,
                title.fontface = "bold",
                title.color = "gray20",
                title.fontfamily = "sans",
                title.bg.color = "white",
                title.bg.alpha = 0.7,
                frame = FALSE,
                legend.outside = TRUE) +
      tm_compass(position = c("right", "top"), size = 0.1) +
      tm_scale_bar(position = c("left", "bottom")) +
      tm_credits("Auteur : Robert Lim\nSource : RTE France, 2025",
                 position = c("right", "bottom"),
                 size = 0.7
      )
  })
  
  
  
  
  ### PieChart1 - Production d'énergie par filière----
  output$pie_chart <- renderPlotly({
    df_pie <- if (input$region_select == "France") {
      regions %>%
        st_drop_geometry() %>%  # enlève les polygones pour éviter les erreurs
        summarise(across(matches("^PR_(NUC|HYD|FOS|SOL|EOL|AUT)_TWH$"), sum, na.rm = TRUE)) %>%
        pivot_longer(cols = everything(), names_to = "Filiere", values_to = "Valeur")
    } else {
      regions %>%
        filter(NOM == input$region_select) %>%
        st_drop_geometry() %>%
        select(matches("^PR_(NUC|HYD|FOS|SOL|EOL|AUT)_TWH$")) %>%
        pivot_longer(cols = everything(), names_to = "Filiere", values_to = "Valeur")
    }
    
    
    # Renommer les codes PR_XXX_TWH en noms lisibles
    df_pie <- df_pie %>%
      mutate(Filiere = recode(Filiere,
                              PR_NUC_TWH = "Nucléaire",
                              PR_HYD_TWH = "Hydraulique",
                              PR_FOS_TWH = "Fossile",
                              PR_EOL_TWH = "Éolien",
                              PR_SOL_TWH = "Solaire",
                              PR_AUT_TWH = "Autre"))
    
    palette_filiere <- c(
      "Nucléaire" = "#ffe18b",
      "Hydraulique" = "#2071b2",
      "Fossile" = "#313334",
      "Éolien" = "#8dcdbf",
      "Solaire" = "#f4902e",
      "Autre" = "#14682d"
    )
    
    plot_ly(df_pie, labels = ~Filiere, values = ~Valeur, type = 'pie',
            marker = list(colors = palette_filiere[df_pie$Filiere], line = list(color = '#FFFFFF', width = 1))) %>%
      layout(title = paste("Production par filière (TWh/an) -", input$region_select))
    
  })
  
  
  ### AreaChart1 - Évolution de la production d'énergie en France----
  output$area_chart <- renderPlotly({
    # Exemple de données d'évolution (à adapter à tes données réelles)
    # data_prod : dataframe avec colonnes Annee, Filiere, Valeur
    
    df_long <- data_prod %>%
      pivot_longer(cols = c(nucleaire, hydraulique, fossile, eolien, solaire, autre),
                   names_to = "filiere",
                   values_to = "production") %>%
      mutate(filiere = recode(filiere,
                              nucleaire = "Nucléaire",
                              hydraulique = "Hydraulique",
                              fossile = "Fossile",
                              eolien = "Éolien",
                              solaire = "Solaire",
                              autre = "Autre"))
    
    
    # Couleurs manuelles par filière
    colors <- c(
      "Nucléaire" = "#ffe18b",
      "Hydraulique" = "#2071b2",
      "Fossile" = "#313334",
      "Éolien" = "#8dcdbf",
      "Solaire" = "#f4902e",
      "Autre" = "#14682d"
    )
    
    
    # Initialiser le plot
    p <- plot_ly()
    
    # Ajouter chaque filière séparément
    for (f in unique(df_long$filiere)) {
      df_filiere <- df_long %>% filter(filiere == f)
      p <- p %>% add_trace(data = df_filiere,
                           x = ~year,
                           y = ~production,
                           type = 'scatter',
                           mode = 'none',
                           stackgroup = 'one',
                           fill = 'tonexty',
                           name = f,
                           fillcolor = colors[f])
    }
    
    # Ajouter la consommation
    p <- p %>% add_trace(data = data_prod,
                         x = ~year,
                         y = ~conso,
                         type = 'scatter',
                         mode = 'lines',
                         name = 'Consommation',
                         line = list(color = 'red', width = 3, dash = 'dash'))
    
    # Mise en forme finale
    p <- p %>% layout(title = "Production d'énergie par filière et consommation",
                      xaxis = list(title = "Année"),
                      yaxis = list(title = "TWh"),
                      legend = list(title = list(text = "Filières")))
    
    p
  })
  
  ### Map6 - Production vs Consommation----
  regions_wgs84 <- st_transform(regions, 4326)
  
  map6_data <- regions_wgs84 %>%
    mutate(
      y = CONSO_TWH,
      x = PR_TOT_TWH
    ) %>%
    mutate(
      x_q = ntile(x, 3),
      y_q = ntile(y, 3),
      bivar_class = paste0(x_q, "-", y_q)
    )
  
  # Palette bivariée
  bivar_pal <- c(
    "1-1" = "#edebf4", "2-1" = "#9ccae1", "3-1" = "#4fadd0",
    "1-2" = "#e39bcc", "2-2" = "#9080bd", "3-2" = "#3d64ad",
    "1-3" = "#de4fa6", "2-3" = "#843598", "3-3" = "#2a1a8a"
  )
  
  output$map6 <- renderTmap({
    tm_shape(map6_data) +
      tm_polygons(
        col = "bivar_class",
        palette = bivar_pal,
        title = "Prod-Conso",
        border.col = "white",
        popup.vars = c(
          "Région" = "NOM",
          "Consommation (MWh/an)" = "CONSO_TWH",
          "Production (MWh/an)" = "PR_TOT_TWH"
        )
      ) +
      tm_compass(position = c("right", "top"), size = 0.1) +
      tm_scale_bar(position = c("left", "bottom")) +
      tm_credits("Auteur : Robert Lim\nSource : RTE France, 2025",
                 position = c("right", "bottom"),
                 size = 0.7,
                 fontface = "italic") +
      tm_layout(title = "Consommation vs Production : Qui alimente la France",
                title.size = 1.2,
                title.fontface = "bold",
                title.color = "gray20",
                title.fontfamily = "sans",
                title.bg.color = "white",
                title.bg.alpha = 0.7,
                frame = FALSE,
                legend.outside = FALSE,
                legend.show = TRUE)
  })
  
  
  ### RadarChart1 - Prod  vs Conso----
  output$radar_chart <- renderPlotly({
    df_radar <- regions %>%
      st_set_geometry(NULL) %>%
      select(NOM, PR_TOT_TWH, CONSO_TWH)
    
    if (!is.null(input$region_radar) && input$region_radar != "Toutes") {
      df_radar <- df_radar %>% filter(NOM == input$region_radar)
    }
    
    # Si pas de données, ne rien afficher
    if (nrow(df_radar) == 0) return(NULL)
    
    # Répéter la ligne si une seule région (fermer le polygone)
    if (nrow(df_radar) == 1) {
      df_radar <- rbind(df_radar, df_radar)
    }
    
    max_val <- max(c(df_radar$PR_TOT_TWH, df_radar$CONSO_TWH), na.rm = TRUE)
    
    plot_ly(type = 'scatterpolar', fill = 'toself') %>%
      add_trace(
        r = df_radar$PR_TOT_TWH,
        theta = df_radar$NOM,
        name = "Production (TWh)",
        line = list(color = "#4fadd0"),
        fill = 'toself',
        fillcolor = 'rgba(79, 173, 208, 0.4)'
      ) %>%
      add_trace(
        r = df_radar$CONSO_TWH,
        theta = df_radar$NOM,
        name = "Consommation (TWh)",
        line = list(color = "#de4fa6"),
        fill = 'toself',
        fillcolor = 'rgba(222, 79, 166, 0.4)'
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            title = "TWh",
            range = c(0, max_val)
          )
        ),
        showlegend = TRUE
      )
  })
  
  
  ## 2.2----
  ### Map4 - Consommation par EPCI----
  output$map4 <- renderTmap({
    
    # Reprojection en WGS 84 si ce n'est pas déjà fait
    data_ara_wgs84 <- st_transform(data_ara, 4326)
    
    # Bbox centré sur l'Europe
    bbox_ara <- st_bbox(c(
      xmin = 3.0,  # longitude minimale
      ymin = 44.2, # latitude minimale
      xmax = 6.5,  # longitude maximale
      ymax = 46.2  # latitude maximale
    ), crs = 4326)  # WGS 84
    
    
    # Données de villes avec coordonnées approximatives
    villes <- data.frame(
      nom = c("Lyon", "Grenoble", "Clermont-Ferrand"),
      lon = c(4.8357, 5.7245, 3.0822),
      lat = c(45.7640, 45.1885, 45.7772)
    )
    
    # Transformer en objet sf
    villes_sf <- st_as_sf(villes, coords = c("lon", "lat"), crs = 4326)
    
    
    # Carte
    tm_shape(data_ara_wgs84, bbox = bbox_ara) +
      tm_polygons("tot",
                  palette = "Oranges",
                  title = "MWh/an",
                  style = "jenks",
                  colorNA = "grey80",
                  textNA = "No Data",
                  border.col = "white",
                  border.lwd = 0.5,
                  popup.vars = c(
                    "EPCI" = "NOM_EPCI",
                    "Consommation (MWh/an)" = "tot",
                    "Population" = "pop"
                  )
      ) + 
      tm_shape(villes_sf) +
      tm_symbols(size = 0.3, shape = 21, col = "white", border.col = "white", alpha = 0.9) +
      tm_shape(villes_sf) +
      tm_text("nom",
              size = 1.2,
              col = "gray20",
              fontface = "bold",
              fontfamily = "sans",
              just = "center"
      ) +
      tm_compass(position = c("right", "top"), size = 0.1) +
      tm_scale_bar(position = c("left", "bottom")) +
      tm_credits("Auteur : Robert Lim\nSource : RTE France, 2025",
                 position = c("right", "bottom"),
                 size = 0.7,
                 fontface = "italic") +
      tm_layout(title = "Consommation énergétique totale annuelle (MWh/an)",
                title.size = 1.2,
                title.fontface = "bold",
                title.color = "gray20",
                title.fontfamily = "sans",
                title.bg.color = "white",
                title.bg.alpha = 0.7,
                frame = FALSE,
                legend.outside = FALSE,
                legend.show = TRUE)
  })
  
  ### Map5 - par habitant----
  output$map5 <- renderTmap({
    # Reprojection en WGS 84 si ce n'est pas déjà fait
    data_ara_wgs84 <- st_transform(data_ara, 4326)
    
    # Bbox centré sur l'Europe
    bbox_ara <- st_bbox(c(
      xmin = 3.0,  # longitude minimale
      ymin = 44.2, # latitude minimale
      xmax = 6.5,  # longitude maximale
      ymax = 46.2  # latitude maximale
    ), crs = 4326)  # WGS 84
    
    
    # Données de villes avec coordonnées approximatives
    villes <- data.frame(
      nom = c("Lyon", "Grenoble", "Clermont-Ferrand"),
      lon = c(4.8357, 5.7245, 3.0822),
      lat = c(45.7640, 45.1885, 45.7772)
    )
    
    # Transformer en objet sf
    villes_sf <- st_as_sf(villes, coords = c("lon", "lat"), crs = 4326)
    
    # Calculer
    data_ara_wgs84 <- data_ara_wgs84 %>%
      mutate(
        conso_per_pop = round((res / pop), 1)
      )
    
    # Carte
    tm_shape(data_ara_wgs84, bbox = bbox_ara) +
      tm_polygons("conso_per_pop",
                  palette = "Purples",
                  title = "MWh/an",
                  style = "jenks",
                  colorNA = "grey80",
                  textNA = "No Data",
                  border.col = "white",
                  border.lwd = 0.5,
                  popup.vars = c(
                    "EPCI" = "NOM_EPCI",
                    "Consommation (MWh/an)" = "conso_per_pop",
                    "Population" = "pop"
                  )
      ) + 
      tm_shape(villes_sf) +
      tm_symbols(size = 0.3, shape = 21, col = "white", border.col = "white", alpha = 0.9) +
      tm_shape(villes_sf) +
      tm_text("nom",
              size = 1.2,
              col = "gray20",
              fontface = "bold",
              fontfamily = "sans",
              just = "center"
      ) +
      tm_compass(position = c("right", "top"), size = 0.1) +
      tm_scale_bar(position = c("left", "bottom")) +
      tm_credits("Auteur : Robert Lim\nSource : RTE France, 2025",
                 position = c("right", "bottom"),
                 size = 0.7,
                 fontface = "italic") +
      tm_layout(title = "Consommation énergétique moyenne annuelle par habitant (MWh/an)",
                title.size = 1.2,
                title.fontface = "bold",
                title.color = "gray20",
                title.fontfamily = "sans",
                title.bg.color = "white",
                title.bg.alpha = 0.7,
                frame = FALSE,
                legend.outside = FALSE,
                legend.show = TRUE)
  })
  
  
}
