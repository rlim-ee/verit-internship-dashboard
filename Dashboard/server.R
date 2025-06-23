
# SERVER ----
server <- function(input, output, session) {
  
  ## Navigation----
  
  observeEvent(input$go_dc_europe, {
    updateTabItems(session, "tabs", "dc_europe_map")
  })
  observeEvent(input$go_flapd, {
    updateTabItems(session, "tabs", "flapd")
  })
  observeEvent(input$go_dc_france, {
    updateTabItems(session, "tabs", "dc_france")
  })
  observeEvent(input$go_regions, {
    updateTabItems(session, "tabs", "regions")
  })
  observeEvent(input$go_ara, {
    updateTabItems(session, "tabs", "ara")
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
  
  
  ## 1.1 Data centres en Europe----
  
  ### Map1 - Carte de répartition----
  
  output$map1 <- renderLeaflet({
    
    # 1. Calcul indicateur
    dc_data <- dc_europe %>%
      mutate(
        dc_per_million = round(nb_dc / (pop / 1e6), 1)
      )
    
    # 2. Jointure
    europe_data <- europe_map %>%
      left_join(dc_data, by = "name")
    
    # 3. Reprojection en WGS84
    europe_data <- st_transform(europe_data, crs = 4326)
    
    # 4. Palette
    pal <- colorQuantile("YlOrRd", domain = europe_data$dc_per_million, n = 5, na.color = "#cccccc")
    
    # 5. Popups
    popup_txt <- paste0(
      "<strong>Pays :</strong> ", europe_data$name, "<br/>",
      "<strong>Nombre de DC :</strong> ", format(europe_data$nb_dc, big.mark = " "), "<br/>",
      "<strong>Population :</strong> ", format(europe_data$pop, big.mark = " "), "<br/>",
      "<strong>DC / million hab. :</strong> ", europe_data$dc_per_million
    )
    
    # 6. Extraire centroïdes
    centroids <- st_centroid(europe_data)
    coords <- st_coordinates(centroids)
    europe_data$lon <- coords[, 1]
    europe_data$lat <- coords[, 2]
    
    # 7. Calcul rayon cercles (racine carrée atténue la taille)
    radius <- sqrt(europe_data$nb_dc) * 15000  # Ajuster la taille selon besoin
    
    # 8. Centrer la carte sur lat=54, lng=15, zoom=4 (à ajuster)
    leaflet(europe_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 15, lat = 54, zoom = 3) %>%
      addPolygons(
        fillColor = ~pal(dc_per_million),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        label = lapply(popup_txt, HTML),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#444",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addCircles(
        lng = ~lon,
        lat = ~lat,
        radius = radius,
        color = "darkblue",
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.5,
        label = ~paste("Nombre de DC :", nb_dc)
      ) %>%
      addLegend(
        pal = pal,
        values = ~dc_per_million,
        title = "DC / million hab.",
        position = "bottomright"
      )
  })
  
  
  
  
  
  ### BarPlot1 - graphique de répartition en barres----
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
                          colorscale = 'Rainbow', alpha = 0.7),
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
  
  ### Évolution de la demande entre 2000 et 2050----
  
  output$dc_demand_plot <- renderPlot({
    data <- data.frame(
      Année = factor(c("2024", "2035"), levels = c("2024", "2035")),
      Demande_TWh = c(96, 236)
    )
    
    ggplot(data, aes(x = Année, y = Demande_TWh, fill = Année)) +
      geom_col(width = 0.6, alpha = 0.75) +  # AUCUN contour
      geom_text(aes(label = paste0(Demande_TWh, " TWh")),
                vjust = -0.8,
                size = 6,
                fontface = "bold",
                color = "black",
                family = "Arial") +
      scale_fill_manual(values = c(
        "2024" = scales::alpha("#3B556D", 0.7),
        "2035" = scales::alpha("#5FC2BA", 0.7)
      )) +
      labs(
        title = "Croissance de la demande énergétique des data centres en Europe",
        y = "Demande en TWh",
        x = NULL
      ) +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18, margin = margin(b = 15)),
        axis.text.x = element_text(face = "bold", size = 14, color = "#333"),
        axis.text.y = element_text(size = 13, color = "#333"),
        axis.title.y = element_text(size = 14, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(20, 20, 40, 20)
      ) +
      expand_limits(y = max(data$Demande_TWh) + 40)
  })
  
  
  

  
  
  ## 1.2 Data centres danns les FLAP-D----
  
  
  
  
  ## 1.3 Data centres en France----
  
  
  
  
  ## 2.1 Énergie en France----
  
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
  output$map3bis <- renderLeaflet({
    regions_wgs84 <- st_transform(regions, 4326)
    centroids <- st_centroid(regions_wgs84)
    centroids$X <- st_coordinates(centroids)[,1]
    centroids$Y <- st_coordinates(centroids)[,2]
    
    # Préparer les popups
    centroids$popup <- paste0(
      "<b>", centroids$NOM, "</b><br>",
      "Production totale : <b>", centroids$PR_TOT_TWH, " TWh</b>"
    )
    
    # Rayon proportionnel
    rayon <- sqrt(centroids$PR_TOT_TWH) * 9000
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 2.2137, lat = 46.7276, zoom = 5) %>%
      
      # Polygones
      addPolygons(data = regions_wgs84,
                  fillColor = "white",
                  fillOpacity = 0.7,
                  color = "black",
                  weight = 1,
                  popup = ~paste0("<b>", NOM, "</b><br>Production totale : <b>", PR_TOT_TWH, " TWh</b>")) %>%
      
      # Cercles proportionnels
      addCircles(data = centroids,
                 lng = ~X, lat = ~Y,
                 radius = rayon,
                 fillColor = "#1A5D1A",
                 color = "black",
                 weight = 1,
                 fillOpacity = 0.7,
                 popup = ~popup) %>%
      
      # Boussole et échelle
      addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) %>%
      
      # Légende texte
      addControl(
        html = paste0("<div style='background:white;padding:8px;border-radius:6px;opacity:0.9;'>
                      <b style='font-size:15px;color:#333;'>Production totale d’énergie par région</b><br>
                      <small style='color:gray;'>Auteur : Robert Lim<br>Source : RTE France, 2025</small>
                    </div>"),
        position = "bottomright"
      )
  })
  
  
  
  
  output$map_totale <- renderLeaflet({
    req(input$choix_map)
    
    regions_wgs84 <- st_transform(regions, 4326)
    centroids <- st_centroid(regions_wgs84)
    centroids$X <- st_coordinates(centroids)[,1]
    centroids$Y <- st_coordinates(centroids)[,2]
    
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
    
    centroids$popup <- paste0(
      "<b>", centroids$NOM, "</b><br>",
      popup_label, " : <b>", centroids[[var]], " TWh</b>"
    )
    
    rayon <- sqrt(centroids[[var]]) * 9000
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 2.2137, lat = 46.7276, zoom = 5) %>%
      
      addPolygons(data = regions_wgs84,
                  fillColor = "white",
                  fillOpacity = 0.7,
                  color = "black",
                  weight = 1,
                  popup = ~paste0("<b>", NOM, "</b><br>", popup_label, " : <b>", .[[var]], " TWh</b>")) %>%
      
      addCircles(data = centroids,
                 lng = ~X, lat = ~Y,
                 radius = rayon,
                 fillColor = couleur,
                 color = "black",
                 weight = 1,
                 fillOpacity = 0.7,
                 popup = ~popup) %>%
      
      addLegend(position = "bottomleft",
                title = popup_label,
                pal = colorNumeric(palette = c("white", couleur), domain = centroids[[var]]),
                values = centroids[[var]],
                opacity = 1) %>%
      
      addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) %>%
      
      addControl(
        html = paste0("<div style='background:white;padding:8px;border-radius:6px;opacity:0.9;'>
                      <b style='font-size:15px;color:#333;'>", titre, "</b><br>
                      <small style='color:gray;'>Auteur : Robert Lim<br>Source : RTE France, 2025</small>
                    </div>"),
        position = "bottomright"
      )
  })
  
  
  
  
  
  
  ### PieChart1 - Production d'énergie par filière----
  output$pie_chart <- renderPlotly({
    df_pie <- if (input$region_select == "France") {
      regions %>%
        st_drop_geometry() %>%
        summarise(across(matches("^PR_(NUC|HYD|FOS|SOL|EOL|AUT)_TWH$"), sum, na.rm = TRUE)) %>%
        pivot_longer(cols = everything(), names_to = "Filiere", values_to = "Valeur")
    } else {
      regions %>%
        filter(NOM == input$region_select) %>%
        st_drop_geometry() %>%
        select(matches("^PR_(NUC|HYD|FOS|SOL|EOL|AUT)_TWH$")) %>%
        pivot_longer(cols = everything(), names_to = "Filiere", values_to = "Valeur")
    }
    
    df_pie <- df_pie %>%
      mutate(Filiere = recode(Filiere,
                              PR_NUC_TWH = "Nucléaire",
                              PR_HYD_TWH = "Hydraulique",
                              PR_FOS_TWH = "Fossile",
                              PR_EOL_TWH = "Éolien",
                              PR_SOL_TWH = "Solaire",
                              PR_AUT_TWH = "Autre")) %>%
      filter(Valeur > 0)
    
    palette_filiere <- c(
      "Nucléaire" = "#FFE18B",
      "Hydraulique" = "#2071B2",
      "Fossile" = "#313334",
      "Éolien" = "#8DCDBF",
      "Solaire" = "#F4902E",
      "Autre" = "#14682D"
    )
    
    plot_ly(
      df_pie,
      labels = ~Filiere,
      values = ~Valeur,
      type = 'pie',
      textinfo = 'label+percent',
      hoverinfo = 'label+value+percent',
      marker = list(
        colors = palette_filiere[df_pie$Filiere],
        line = list(color = '#FFFFFF', width = 1.5)
      )
    ) %>%
      layout(
        title = list(
          text = paste("Production par filière (TWh/an) -", input$region_select),
          font = list(size = 18, color = '#333')
        ),
        showlegend = TRUE,
        legend = list(
          orientation = "v",
          x = 1.05,
          y = 0.9,
          font = list(size = 13)
        ),
        paper_bgcolor = '#f7f7f7',
        plot_bgcolor = '#f7f7f7',
        margin = list(l = 30, r = 120, t = 60, b = 30)
      )
  })
  
  
  
  ### AreaChart1 - Évolution de la production d'énergie en France----
  output$area_chart <- renderPlotly({
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
    
    colors <- c(
      "Nucléaire" = "#FFE18B",
      "Hydraulique" = "#2071B2",
      "Fossile" = "#313334",
      "Éolien" = "#8DCDBF",
      "Solaire" = "#F4902E",
      "Autre" = "#14682D"
    )
    
    p <- plot_ly()
    
    # Tracer les filières empilées
    for (f in names(colors)) {
      df_f <- df_long %>% filter(filiere == f)
      p <- p %>%
        add_trace(
          data = df_f,
          x = ~year,
          y = ~production,
          type = 'scatter',
          mode = 'none',
          stackgroup = 'one',
          fill = 'tonexty',
          name = f,
          fillcolor = colors[f],
          hoverinfo = 'x+y',
          text = ~paste0(f, ": ", round(production, 1), " TWh"),
          hovertemplate = paste("<b>%{text}</b><br>Année: %{x}<extra></extra>")
        )
    }
    
    # Ligne de consommation
    p <- p %>%
      add_trace(
        data = data_prod,
        x = ~year,
        y = ~conso,
        type = 'scatter',
        mode = 'lines',
        name = "Consommation",
        line = list(color = "red", width = 3, dash = "dash"),
        hoverinfo = 'x+y',
        text = ~paste("Consommation:", round(conso, 1), "TWh"),
        hovertemplate = paste("<b>%{text}</b><br>Année: %{x}<extra></extra>")
      )
    
    # Mise en forme finale
    p %>%
      layout(
        title = list(
          text = "Production d'énergie par filière et consommation",
          font = list(size = 20, color = "#333")
        ),
        xaxis = list(title = "Année", tickfont = list(size = 14), titlefont = list(size = 16)),
        yaxis = list(title = "TWh", tickfont = list(size = 14), titlefont = list(size = 16)),
        legend = list(
          orientation = "v",
          x = 1.02,
          y = 0.95,
          font = list(size = 13)
        ),
        plot_bgcolor = "#f7f7f7",
        paper_bgcolor = "#f7f7f7",
        margin = list(l = 60, r = 120, t = 60, b = 50)
      )
  })
  
  
  ### Map6 - Production vs Consommation----
  # Transformation en WGS84 (4326)
  regions_wgs84 <- st_transform(regions, 4326)
  
  # Préparer les données bivariées
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
  
  # Palette bivariée manuelle
  ### Préparation des données bivariées ----
  map6_data <- regions %>%
    st_transform(4326) %>%
    mutate(
      y = CONSO_TWH,
      x = PR_TOT_TWH,
      x_q = ntile(x, 3),
      y_q = ntile(y, 3),
      bivar_class = paste0(x_q, "-", y_q)
    ) %>%
    mutate(bivar_class = as.character(bivar_class))
  
  ### Palette bivariée ----
  bivar_pal <- c(
    "1-1" = "#edebf4", "2-1" = "#9ccae1", "3-1" = "#4fadd0",
    "1-2" = "#e39bcc", "2-2" = "#9080bd", "3-2" = "#3d64ad",
    "1-3" = "#de4fa6", "2-3" = "#843598", "3-3" = "#2a1a8a"
  )
  
  pal <- colorFactor(
    palette = bivar_pal,
    domain = names(bivar_pal)
  )
  
  ### Fonction popup HTML ----
  popup_html <- function(data) {
    paste0(
      "<b>Région :</b> ", data$NOM, "<br/>",
      "<b>Consommation (TWh/an) :</b> ", round(data$CONSO_TWH, 2), "<br/>",
      "<b>Production (TWh/an) :</b> ", round(data$PR_TOT_TWH, 2)
    )
  }
  
  
  ### Rendu Leaflet ----
  output$map6 <- renderLeaflet({
    # Palette bivariée
    pal <- colorFactor(
      palette = unname(bivar_pal),
      domain = names(bivar_pal)
    )
    
    # Générer les popups à l’avance
    map6_data$popup_content <- mapply(function(nom, pr, conso) {
      paste0(
        "<b>Région :</b> ", nom, "<br/>",
        "<b>Consommation (TWh/an) :</b> ", round(conso, 2), "<br/>",
        "<b>Production (TWh/an) :</b> ", round(pr, 2)
      )
    }, map6_data$NOM, map6_data$PR_TOT_TWH, map6_data$CONSO_TWH, SIMPLIFY = TRUE)
    
    leaflet(map6_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%  # Fond clair
      addPolygons(
        fillColor = ~pal(bivar_class),
        color = "white",        # Contours blancs
        weight = 1,
        opacity = 1,
        fillOpacity = 0.85,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        popup = ~popup_content
      ) %>%
      addControl(
        html = HTML('
    <div title="Lecture : la couleur reflète une combinaison entre la consommation (→) et la production (↑) d’électricité par région.">
      <div style="width:220px; background:white; padding:10px; border-radius:6px;
                  box-shadow: 0 0 6px rgba(0,0,0,0.3); font-size:12px; line-height:1.2;">
        <div style="text-align:center; font-weight:bold; margin-bottom:6px;">
          Production ↑<br/>Consommation →
        </div>
        <div style="display: grid; grid-template-columns: repeat(3, 1fr); grid-gap:1px; margin-bottom:6px;">
          <div style="background:#edebf4; height:20px;"></div>
          <div style="background:#9ccae1; height:20px;"></div>
          <div style="background:#4fadd0; height:20px;"></div>
          <div style="background:#e39bcc; height:20px;"></div>
          <div style="background:#9080bd; height:20px;"></div>
          <div style="background:#3d64ad; height:20px;"></div>
          <div style="background:#de4fa6; height:20px;"></div>
          <div style="background:#843598; height:20px;"></div>
          <div style="background:#2a1a8a; height:20px;"></div>
        </div>
        <div style="font-size:11px; text-align:left;">
          <em>Lecture :</em> <span style="white-space:nowrap;">coin bas-gauche = faible prod. & conso,</span><br/>
          <span style="white-space:nowrap;">coin haut-droit = forte prod. & conso</span>
        </div>
      </div>
    </div>
  '),
        position = "bottomright"
      ) %>%
      addScaleBar(position = "bottomleft") %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Localiser",
        onClick = JS("function(btn, map){ map.locate({setView: true}); }")
      ))
  })
  
  
  
  
  ### Radar Chart - Production vs Consommation ----
  output$radar_chart <- renderPlotly({
    # Fonction de diagnostic et nettoyage des données
    tryCatch({
      # Étape 1: Extraction et nettoyage initial
      df_radar <- regions %>%
        st_set_geometry(NULL) %>%
        select(NOM, PR_TOT_TWH, CONSO_TWH)
      
      # Étape 2: Diagnostic des données
      cat("Diagnostic des données:\n")
      cat("Nombre de lignes:", nrow(df_radar), "\n")
      cat("Structure PR_TOT_TWH:", class(df_radar$PR_TOT_TWH), "\n")
      cat("Structure CONSO_TWH:", class(df_radar$CONSO_TWH), "\n")
      
      # Étape 3: Nettoyage radical des données
      df_radar <- df_radar %>%
        mutate(
          NOM = as.character(NOM),
          # Nettoyage agressif des valeurs numériques
          PR_TOT_TWH = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(PR_TOT_TWH)))),
          CONSO_TWH = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(CONSO_TWH))))
        ) %>%
        # Filtrage strict
        filter(
          !is.na(NOM), 
          nchar(trimws(NOM)) > 0,
          !is.na(PR_TOT_TWH), 
          !is.na(CONSO_TWH),
          is.finite(PR_TOT_TWH), 
          is.finite(CONSO_TWH),
          PR_TOT_TWH >= 0, 
          CONSO_TWH >= 0
        ) %>%
        # Suppression des doublons
        distinct(NOM, .keep_all = TRUE)
      
      # Étape 4: Filtrage par région
      if (!is.null(input$region_radar) && input$region_radar != "Toutes") {
        df_radar <- df_radar %>% filter(NOM == input$region_radar)
      }
      
      # Étape 5: Vérification finale des données
      if (nrow(df_radar) == 0) {
        return(plot_ly() %>% 
                 add_annotations(
                   text = "Aucune donnée valide disponible",
                   x = 0.5, y = 0.5,
                   xref = "paper", yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16, color = "#666666")
                 ) %>%
                 layout(
                   title = "Graphique Radar - Données Non Disponibles",
                   xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE)
                 ))
      }
      
      # Étape 6: Calcul sécurisé des limites
      all_values <- c(df_radar$PR_TOT_TWH, df_radar$CONSO_TWH)
      max_val <- if(length(all_values) > 0 && all(is.finite(all_values))) {
        max(all_values) * 1.2
      } else {
        100
      }
      
      # Étape 7: Construction du graphique de base
      p <- plot_ly(type = 'scatterpolar')
      
      # Étape 8: Ajout des traces de manière sécurisée
      # Trace Production
      p <- p %>% add_trace(
        r = df_radar$PR_TOT_TWH,
        theta = df_radar$NOM,
        name = "Production (TWh)",
        mode = 'lines+markers',
        line = list(color = '#4fadd0', width = 2),
        marker = list(color = '#4fadd0', size = 6),
        fill = 'toself',
        fillcolor = 'rgba(46, 139, 87, 0.2)',
        hovertemplate = paste0(
          "<b>%{theta}</b><br>",
          "Production: %{r:.1f} TWh<br>",
          "<extra></extra>"
        )
      )
      
      # Trace Consommation
      p <- p %>% add_trace(
        r = df_radar$CONSO_TWH,
        theta = df_radar$NOM,
        name = "Consommation (TWh)",
        mode = 'lines+markers',
        line = list(color = '#de4fa6', width = 2),
        marker = list(color = '#de4fa6', size = 6),
        fill = 'toself',
        fillcolor = 'rgba(220, 20, 60, 0.2)',
        hovertemplate = paste0(
          "<b>%{theta}</b><br>",
          "Consommation: %{r:.1f} TWh<br>",
          "<extra></extra>"
        )
      )
      
      # Étape 9: Configuration du layout
      p <- p %>% layout(
        title = list(
          text = if(!is.null(input$region_radar) && input$region_radar != "Toutes") {
            paste("Bilan Énergétique -", input$region_radar)
          } else {
            "Bilan Énergétique par Région"
          },
          font = list(size = 16)
        ),
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, max_val),
            title = "TWh"
          ),
          angularaxis = list(
            rotation = 90,
            direction = "clockwise"
          )
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = 'center',
          y = -0.1
        ),
        showlegend = TRUE
      )
      
      # Étape 10: Ajout du bilan si pertinent
      if (nrow(df_radar) > 1) {
        total_prod <- sum(df_radar$PR_TOT_TWH, na.rm = TRUE)
        total_conso <- sum(df_radar$CONSO_TWH, na.rm = TRUE)
        bilan <- total_prod - total_conso
        
        if (is.finite(bilan)) {
          bilan_text <- if (bilan > 0) {
            paste("Excédent:", round(bilan, 1), "TWh")
          } else {
            paste("Déficit:", round(abs(bilan), 1), "TWh")
          }
          
          p <- p %>% add_annotations(
            text = bilan_text,
            x = 1, y = 1,
            xref = "paper", yref = "paper",
            xanchor = "right", yanchor = "top",
            showarrow = FALSE,
            font = list(size = 11),
            bgcolor = "rgba(255, 255, 255, 0.8)",
            bordercolor = "rgba(0, 0, 0, 0.1)",
            borderwidth = 1
          )
        }
      }
      
      return(p)
      
    }, error = function(e) {
      # En cas d'erreur, retourner un graphique d'erreur informatif
      cat("Erreur dans radar_chart:", e$message, "\n")
      
      return(plot_ly() %>% 
               add_annotations(
                 text = paste("Erreur lors de la création du graphique:", e$message),
                 x = 0.5, y = 0.5,
                 xref = "paper", yref = "paper",
                 showarrow = FALSE,
                 font = list(size = 14, color = "#DC143C")
               ) %>%
               layout(
                 title = "Erreur - Graphique Radar",
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE)
               ))
    })
  })
  
  
  
  
  ## 2.2 Énergie en Auvergne-Rhone-Alpes----
  
  output$map4 <- renderLeaflet({
    # Reprojection en WGS 84
    data_ara_wgs84 <- st_transform(data_ara, 4326)
    
    # Données des villes principales
    villes <- data.frame(
      nom = c("Lyon", "Grenoble", "Clermont-Ferrand"),
      lon = c(4.8357, 5.7245, 3.0822),
      lat = c(45.7640, 45.1885, 45.7772)
    )
    
    # Palette de couleurs pour consommation totale
    pal <- colorBin("Oranges", domain = data_ara_wgs84$tot, bins = 6, na.color = "#e0e0e0")
    
    # Etiquettes pour les polygones
    labels <- sprintf(
      "<strong>%s</strong><br/>Conso totale : %s MWh/an<br/>Population : %s",
      data_ara_wgs84$NOM_EPCI,
      format(data_ara_wgs84$tot, big.mark = " "),
      format(data_ara_wgs84$pop, big.mark = " ")
    ) %>% lapply(htmltools::HTML)
    
    # Carte leaflet
    leaflet(data = data_ara_wgs84) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 4.8, lat = 45.5, zoom = 7) %>%
      addPolygons(
        fillColor = ~pal(tot),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels
      ) %>%
      addLegend(
        pal = pal,
        values = ~tot,
        opacity = 0.7,
        title = "MWh/an (total)",
        position = "bottomleft"
      ) %>%
      # Points pour les villes
      addCircleMarkers(
        data = villes,
        lng = ~lon,
        lat = ~lat,
        radius = 5,
        color = "black",
        fillColor = "black",
        fillOpacity = 0.9,
        stroke = TRUE,
        weight = 1
      ) %>%
      # Noms des villes
      addLabelOnlyMarkers(
        data = villes,
        lng = ~lon,
        lat = ~lat,
        label = ~nom,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          style = list(
            "color" = "gray20",
            "font-weight" = "bold",
            "font-size" = "13px"
          )
        )
      )
  })
  
  
  output$map5 <- renderLeaflet({
    # Reprojection en WGS 84
    data_ara_wgs84 <- st_transform(data_ara, 4326)
    
    # Calcul de la conso par habitant
    data_ara_wgs84 <- data_ara_wgs84 %>%
      mutate(
        conso_per_pop = round(res / pop, 1)
      )
    
    # Coordonnées des villes principales
    villes <- data.frame(
      nom = c("Lyon", "Grenoble", "Clermont-Ferrand"),
      lon = c(4.8357, 5.7245, 3.0822),
      lat = c(45.7640, 45.1885, 45.7772)
    )
    
    # Créer une palette de couleurs
    pal <- colorBin("Purples", domain = data_ara_wgs84$conso_per_pop, bins = 5, na.color = "#e0e0e0")
    
    # Etiquettes (popup ou labels)
    labels <- sprintf(
      "<strong>%s</strong><br/>Conso/hab : %s MWh/an<br/>Population : %s",
      data_ara_wgs84$NOM_EPCI,
      data_ara_wgs84$conso_per_pop,
      format(data_ara_wgs84$pop, big.mark = " ")
    ) %>% lapply(htmltools::HTML)
    
    # Création de la carte
    leaflet(data = data_ara_wgs84) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 4.8, lat = 45.5, zoom = 7) %>%
      addPolygons(
        fillColor = ~pal(conso_per_pop),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels
      ) %>%
      addLegend(
        pal = pal,
        values = ~conso_per_pop,
        opacity = 0.7,
        title = "MWh/an par habitant",
        position = "bottomleft"
      ) %>%
      # Villes : points
      addCircleMarkers(
        data = villes,
        lng = ~lon,
        lat = ~lat,
        radius = 5,
        color = "black",
        fillColor = "black",
        fillOpacity = 0.9,
        stroke = TRUE,
        weight = 1
      ) %>%
      # Villes : noms
      addLabelOnlyMarkers(
        data = villes,
        lng = ~lon,
        lat = ~lat,
        label = ~nom,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          style = list(
            "color" = "gray20",
            "font-weight" = "bold",
            "font-size" = "13px"
          )
        )
      )
  })
  
  
  
  
}
  

