
# 
# USER INTERFACE ----
#

ui <- bs4DashPage(
  title = "Dashboard",
  fullscreen = TRUE,
  
  header = bs4DashNavbar(
    title = tags$span("Tableau de bord", style = "font-weight: bold; color: #31708f;")
  ),
  
  ##
  ## SIDEBAR ----
  ##
  sidebar = bs4DashSidebar(
    title = "Menu",
    collapsed = TRUE,       
    compact = TRUE,
    bs4SidebarMenu(
      id = "tabs",
      bs4SidebarMenuItem("Accueil", tabName = "home", icon = icon("home")),
      
      bs4SidebarMenuItem("Extr & Prod", icon = icon("spinner"),
                         bs4SidebarMenuSubItem("Métaux", tabName = "extraction"),
                         bs4SidebarMenuSubItem("Semi-conducteur", tabName = "semi_conductors")
      ),
      
      bs4SidebarMenuItem("DC & Europe", icon = icon("earth-europe"),
                         bs4SidebarMenuSubItem("DC en Europe", tabName = "dc_europe_map"),
                         bs4SidebarMenuSubItem("FLAP-D", tabName = "flapd"),
                         bs4SidebarMenuSubItem("DC en France", tabName = "dc_france")
      ),
      
      bs4SidebarMenuItem("Bilan énergétique", icon = icon("lightbulb"),
                         bs4SidebarMenuSubItem("France", tabName = "regions"),
                         bs4SidebarMenuSubItem("AURA", tabName = "ara")
      ),
      
      bs4SidebarMenuItem("Simulation", icon = icon("cogs"),
                         bs4SidebarMenuSubItem("Prédictive", tabName = "sim1"),
                         bs4SidebarMenuSubItem("Comparative", tabName = "sim2")
      )
    )
  ),
  
  
  ##
  ## MAIN BODY ----
  ##
  body = bs4DashBody(
    
    tags$head(
      # Importer la police
      tags$link(href = "https://fonts.googleapis.com/css2?family=Poppins&display=swap", rel = "stylesheet"),
      
      ### CSS personnalisé ----
      tags$style(HTML("
    body, h1, h2, h3, h4, h5, h6, .nav-link, .brand-text {
    font-family: 'Poppins', sans-serif !important;
  }

  .main-header .navbar .navbar-brand .brand-text {
    font-weight: bold !important;
    font-size: 22px !important;
    color: #f5f6f7 !important;
  }

  .main-sidebar,
  .sidebar-dark-primary {
    background-color: #31708f !important;
    color: white !important;
  }

  .main-sidebar .nav-link {
    color: white !important;
  }

  .main-sidebar .nav-link.active,
  .main-sidebar .nav-link:hover {
    background-color: #265a6a !important;
    color: white !important;
  }

  .main-sidebar .nav-link .fa {
    color: white !important;
  }

  .content-wrapper,
  .main-footer,
  .main-header {
    background-color: white !important;
  }

  .card-header {
    background-color: #0B162C !important;
    color: #5FC2BA !important;
  }

  .card-title {
    font-size: 20px !important;
    font-weight: bold !important;
    color: white !important;
  }

  .section-header {
    background-color: #31708f !important;
    color: #ffffff !important;
    font-weight: 600 !important;
    font-size: 1.4rem !important;
    padding: 12px 20px !important;
    margin: 30px 0 20px 0 !important;
    border-radius: 8px !important;
    box-shadow: 0 2px 6px rgba(0, 0, 0, 0.1) !important;
    border-left: 5px solid #0B162C !important;
    letter-spacing: 0.5px !important;
  }

  .modern-energy-card {
    background: white;
    border-radius: 12px;
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
    transition: transform 0.3s ease, box-shadow 0.3s ease;
    border-left: 5px solid;
    padding: 20px;
    margin-bottom: 20px;
    height: 140px;
    display: flex;
    flex-direction: column;
    justify-content: space-between;
  }

  .modern-energy-card:hover {
    transform: translateY(-3px);
    box-shadow: 0 8px 25px rgba(0, 0, 0, 0.15);
  }

  .energy-card-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    margin-bottom: 15px;
  }

  .energy-icon-circle {
    width: 50px;
    height: 50px;
    border-radius: 50%;
    display: flex;
    align-items: center;
    justify-content: center;
    color: white;
    font-size: 20px;
  }

  .energy-value-large {
    font-size: 2.2rem;
    font-weight: 700;
    margin: 0;
    line-height: 1;
  }

  .energy-subtitle-text {
    font-size: 1rem;
    font-weight: 600;
    color: #64748b;
    margin: 0;
  }

  .energy-capacity-info {
    font-size: 0.75rem;
    color: #64748b;
    margin-top: 4px;
    text-align: right;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }

  .section-title {
    color: #1e293b;
    font-weight: 600;
    margin-bottom: 25px;
    padding-bottom: 10px;
    border-bottom: 3px solid #3b82f6;
    display: inline-block;
  }
  "))
    ),
    
    ###
    ### ONGLETS ----
    ###
    bs4TabItems(
      
      #### 0.0 Home Tab ----
      bs4TabItem(
        tabName = "home",
        
        fluidRow(
          column(
            width = 12,
            div(
              style = "background: #f9f9f9; padding: 20px; border-radius: 8px; 
                 box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px; text-align: center;",
              h2(icon("table"), "Matérialités du numérique - Data Centers", 
                 style = "color: #31708f; font-weight: bold; margin: 0;")
            )
          )
        ),
        
        # Encadré déroulant d'introduction avec le contexte
        bs4Card(
          title = "Contexte",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12,
          tags$p(
            "Ce tableau de bord explore les enjeux liés à l’implantation des data centers, à travers trois grandes thématiques : l’extraction des ressources (métaux, semi-conducteurs), la répartition géographique des infrastructures (Europe, FLAP-D, France), et leur bilan énergétique à différentes échelles (France, Auvergne-Rhône-Alpes)."
          ),
          tags$p(
            "Deux modules de simulation sont également proposés, basés sur une étude de cas à Eybens (Isère) : une simulation prédictive permettant de moduler le facteur de charge du data center, et une simulation comparative."
          ),
          tags$p(
            "Dans ce contexte, la société française DataOne prévoit la construction de deux data centers surpuissants en Isère. Le site de Eybens devrait atteindre 1 GW de puissance d'ici 2035, avec une consommation exclusivement hydraulique selon les ambitions affichées. Ce projet illustre les enjeux liés au développement de l'intelligence artificielle, très énergivore, et met en lumière la nécessité d'un équilibre entre innovation technologique et durabilité énergétique."
          ),
          tags$blockquote(
            "Les centres de données pourraient représenter 5,7 % de la demande totale d'électricité en Europe d'ici 2035.",
            style = "font-style: italic; color: #31708f;"
          ),
          column(
            width = 12,
            div(
              style = "text-align: right; margin-bottom: 20px;",
              tags$a(
                href = "user_guide/user_guide_general.pdf", 
                target = "_blank", 
                class = "btn btn-success",
                style = "padding: 10px 20px; border-radius: 5px;",
                icon("file-pdf"), " Guide d'utilisation"
              )
            )
          )
        ),
        
        # 1ère section
        h3("Explorer le dashboard :", class = "section-header"),
        
        ## 1ère sous-section : Extraction et Production
        h5("Extraction et Production", style = "margin-top: 20px; margin-bottom: 10px; font-weight: bold; color: #0B162C"),
        fluidRow(
          column(6,
                 div(
                   id = "card_extraction",
                   class = "modern-energy-card",
                   style = "cursor: pointer; border-left-color: #4c3921;",
                   onclick = "Shiny.setInputValue('go_extraction', Math.random())",
                   
                   div(class = "energy-card-header",
                       div(class = "energy-icon-circle",
                           style = "background: linear-gradient(135deg, #4c3921, #4c3921);",
                           icon("hammer")
                       ),
                       div(style = "text-align: right;",
                           h3("Métaux", style = "color: #4c3921;", class = "energy-value-large"),
                           p("Cliquer pour voir", class = "energy-capacity-info")
                       )
                   ),
                   div(
                     p("Extraction des métaux", class = "energy-subtitle-text")
                   )
                 )
          ),
          column(6,
                 div(
                   id = "card_semi",
                   class = "modern-energy-card",
                   style = "cursor: pointer; border-left-color: #cfd514;",
                   onclick = "Shiny.setInputValue('go_semi_conductors', Math.random())",
                   div(class = "energy-card-header",
                       div(class = "energy-icon-circle",
                           style = "background: linear-gradient(135deg, #cfd514, #cfd514);",
                           icon("microchip")
                       ),
                       div(style = "text-align: right;",
                           h3("Semi-conducteurs", style = "color: #cfd514;", class = "energy-value-large"),
                           p("Cliquer pour voir", class = "energy-capacity-info")
                       )
                   ),
                   div(
                     p("Production des semi-conducteurs", class = "energy-subtitle-text")
                   )
                 )
          )
        ),
        
        ## 2ème sous-section : Répartition des data centers
        h5("Répartition des data centers", style = "margin-top: 20px; margin-bottom: 10px; font-weight: bold; color: #0B162C"),
        fluidRow(
          column(4,
                 div(
                   id = "card_dc_europe",
                   class = "modern-energy-card",
                   style = "cursor: pointer; border-left-color: #3b82f6;",
                   onclick = "Shiny.setInputValue('go_dc_europe_map', Math.random())",
                   div(class = "energy-card-header",
                       div(class = "energy-icon-circle",
                           style = "background: linear-gradient(135deg, #3b82f6, #3b82f6);",
                           icon("globe-europe")
                       ),
                       div(style = "text-align: right;",
                           h3("Europe", style = "color: #3b82f6;", class = "energy-value-large"),
                           p("Cliquer pour voir", class = "energy-capacity-info")
                       )
                   ),
                   div(
                     p("Data centers en Europe", class = "energy-subtitle-text")
                   )
                 )
          ),
          column(4,
                 div(
                   id = "card_flapd",
                   class = "modern-energy-card",
                   style = "cursor: pointer; border-left-color: #f97316;",
                   onclick = "Shiny.setInputValue('go_flapd', Math.random())",
                   div(class = "energy-card-header",
                       div(class = "energy-icon-circle",
                           style = "background: linear-gradient(135deg, #f97316, #f97316);",
                           icon("network-wired")
                       ),
                       div(style = "text-align: right;",
                           h3("FLAP-D", style = "color: #f97316;", class = "energy-value-large"),
                           p("Cliquer pour voir", class = "energy-capacity-info")
                       )
                   ),
                   div(
                     p("Data centers dans les FLAP-D", class = "energy-subtitle-text")
                   )
                 )
          ),
          column(4,
                 div(
                   id = "card_dc_france",
                   class = "modern-energy-card",
                   style = "cursor: pointer; border-left-color: #10b981;",
                   onclick = "Shiny.setInputValue('go_dc_france', Math.random())",
                   div(class = "energy-card-header",
                       div(class = "energy-icon-circle",
                           style = "background: linear-gradient(135deg, #10b981, #10b981);",
                           icon("server")
                       ),
                       div(style = "text-align: right;",
                           h3("France", style = "color: #10b981;", class = "energy-value-large"),
                           p("Cliquer pour voir", class = "energy-capacity-info")
                       )
                   ),
                   div(
                     p("Data centers en France", class = "energy-subtitle-text")
                   )
                 )
          )
        ),
        
        ## 3ème sous-section : Bilan énergétique
        h5("Bilan énergétique", style = "margin-top: 20px; margin-bottom: 10px; font-weight: bold; color: #0B162C"),
        fluidRow(
          column(6,
                 div(
                   id = "card_energy_france",
                   class = "modern-energy-card",
                   style = "cursor: pointer; border-left-color: #6366f1;",
                   onclick = "Shiny.setInputValue('go_regions', Math.random())",
                   div(class = "energy-card-header",
                       div(class = "energy-icon-circle",
                           style = "background: linear-gradient(135deg, #6366f1, #6366f1);",
                           icon("bolt")
                       ),
                       div(style = "text-align: right;",
                           h3("France", style = "color: #6366f1;", class = "energy-value-large"),
                           p("Cliquer pour voir", class = "energy-capacity-info")
                       )
                   ),
                   div(
                     p("Énergie en France", class = "energy-subtitle-text")
                   )
                 )
          ),
          column(6,
                 div(
                   id = "card_energy_ara",
                   class = "modern-energy-card",
                   style = "cursor: pointer; border-left-color: #ec4899;",
                   onclick = "Shiny.setInputValue('go_ara', Math.random())",
                   div(class = "energy-card-header",
                       div(class = "energy-icon-circle",
                           style = "background: linear-gradient(135deg, #ec4899, #ec4899);",
                           icon("chart-area")
                       ),
                       div(style = "text-align: right;",
                           h3("Auvergne-Rhône-Alpes", style = "color: #ec4899;", class = "energy-value-large"),
                           p("Cliquer pour voir", class = "energy-capacity-info")
                       )
                   ),
                   div(
                     p("Analyse régionale", class = "energy-subtitle-text")
                   )
                 )
          )
        ),
        
        ## 4ème sous-section : Simulations
        h3("Explorer les simulations :", class = "section-header"),
        fluidRow(
          column(6,
                 div(
                   id = "card_sim1",
                   class = "modern-energy-card",
                   style = "cursor: pointer; border-left-color: #14b8a6;",
                   onclick = "Shiny.setInputValue('go_sim1', Math.random())",
                   div(class = "energy-card-header",
                       div(class = "energy-icon-circle",
                           style = "background: linear-gradient(135deg, #14b8a6, #14b8a6);",
                           icon("chart-line")
                       ),
                       div(style = "text-align: right;",
                           h3("Analyse prédictive", style = "color: #14b8a6;", class = "energy-value-large"),
                           p("Cliquer pour voir", class = "energy-capacity-info")
                       )
                   ),
                   div(
                     p("Projection énergétique", class = "energy-subtitle-text")
                   )
                 )
          ),
          column(6,
                 div(
                   id = "card_sim2",
                   class = "modern-energy-card",
                   style = "cursor: pointer; border-left-color: #06b6d4;",
                   onclick = "Shiny.setInputValue('go_sim2', Math.random())",
                   div(class = "energy-card-header",
                       div(class = "energy-icon-circle",
                           style = "background: linear-gradient(135deg, #06b6d4, #06b6d4);",
                           icon("cogs")
                       ),
                       div(style = "text-align: right;",
                           h3("Analyse comparative", style = "color: #06b6d4;", class = "energy-value-large"),
                           p("Cliquer pour voir", class = "energy-capacity-info")
                       )
                   ),
                   div(
                     p("Comparaison de consommation", class = "energy-subtitle-text")
                   )
                 )
          )
        ),
        
        # Crédits
        tags$div(
          style = "margin-top: 30px; text-align: right; font-size: 0.9em; color: #888;",
          "Auteur : Zoé Cargnelli & Robert Lim | Source : ICIS, Eurostat, DataCenterMap, RTE France | 2025"
        )
      ),
      
      ###
      ### 1. Extraction et Production ----
      ###
      ####
      #### 1.1. Extraction des métaux ----
      ####
      bs4TabItem(
        tabName = "extraction",
        
        # Bouton retour
        fluidRow(
          column(
            width = 12,
            div(
              style = "margin-bottom: 20px;",
              actionButton("retour_accueil_extraction", "Retour à l'accueil", 
                           icon = icon("arrow-left"),
                           style = "background-color: #31708f; color: white; border: none; padding: 10px 20px; border-radius: 5px;")
            )
          )
        ),
        
        # Encadré déroulant avec le contexte
        bs4Card(
          title = tagList(icon("industry"), " Contexte"),
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12,
          
          tags$p(
            HTML(
              "Cette section propose une <strong>analyse de l’extraction des terres rares</strong> et des <strong>métaux stratégiques</strong> 
      (comme le <span style='color:#e67e22;'>lithium</span>, le <span style='color:#3498db;'>silicium</span> ou le <span style='color:#95a5a6;'>cuivre</span>), 
      indispensables à la fabrication des équipements numériques et au stockage de l’énergie."
            )
          ),
          
          tags$p(
            HTML(
              "Une <span style='color:#0073e6;'>carte interactive</span> permet de visualiser, pays par pays, 
      les <strong>volumes extraits en tonnes par an</strong> pour chaque métal. 
      L’utilisateur peut ainsi identifier les principaux producteurs mondiaux 
      et observer la <span style='color:#d35400;'>concentration géographique</span> des ressources."
            )
          ),
          
          tags$p(
            HTML(
              "Un <strong>diagramme de type Sankey</strong> complète cette exploration en représentant les <span style='color:#2ecc71;'>usages finaux</span> des métaux sélectionnés 
      (batteries, microélectronique, production d’énergie, etc.). 
      Pour consulter les flux associés à un métal spécifique, <strong>cliquez sur le bouton</strong> correspondant en haut de l’onglet."
            )
          ),
          
          tags$p(
            HTML(
              "Les <strong>métaux critiques</strong> jouent un rôle central dans le fonctionnement des <span style='color:#17a2b8;'>infrastructures numériques</span>. 
      On les retrouve dans les <strong>serveurs</strong>, les systèmes de <strong>refroidissement</strong>, les <strong>CPU/GPU</strong>, 
      les <strong>semi-conducteurs</strong> et les dispositifs de <strong>stockage de données</strong>. 
      Les <em>data centers</em>, véritables nœuds physiques du numérique, dépendent fortement de ces ressources 
      pour assurer la <strong>puissance de calcul</strong>, la <strong>fiabilité</strong> et la <strong>densité énergétique</strong> de leurs équipements."
            )
          )
        ),
        
        # Titre
        fluidRow(
          column(
            width = 12,
            div(
              style = "background: #f9f9f9; padding: 20px; border-radius: 8px; 
                 box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px; text-align: center;",
              h2(icon("hammer"), "Extraction des métaux", 
                 style = "color: #31708f; font-weight: bold; margin: 0;")
            )
          )
        ),
        
        ##### 1.1.1. Carte de l'extraction des métaux ----
        fluidRow(
          column(
            width = 12,
            div(
              style = "background: #f9f9f9; padding: 20px; border-radius: 8px;
               box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px;",
              h3(icon("globe"), "Extraction des matières premières", style = "color: #31708f;"),
              
              # Boutons pour sélectionner la ressource
              radioButtons(
                inputId = "selected_metal",
                label = "Choisissez une ressource :",
                choices = c("Silicium", "Or", "Cuivre", "Lithium", "Zinc", "Aluminium", "Nickel"),
                selected = "Or",
                inline = TRUE
              ),
              
              withSpinner(leafletOutput("map_extraction", height = "600px"), type = 6, color = "#444"),
              tags$p("Carte interactive montrant l'extraction (en tonnes) par pays. Les pays sans production sont en gris.",
                     style = "margin-top: 15px; font-size: 16px; color: #555;")
            )
          )
        ),
        
        ##### 1.1.2. Diagramme Sankey ----
        fluidRow(
          column(
            width = 12,
            div(
              style = "background: #f9f9f9; padding: 20px; border-radius: 8px; 
               box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px;",
              h3(icon("project-diagram"), textOutput("titre_sankey"),
                 style = "color: #31708f; margin-bottom: 20px;"),
              
              uiOutput("sankey_ui"),
              
              tags$p(
                "Visualisation de la répartition de la demande mondiale par secteur d’usage pour chaque ressource critique.",
                style = "margin-top: 15px; font-size: 16px; color: #555;"
              )
            )
          )
        )
      ),
      
      #### 1.2. Production----
      
      bs4TabItem(
        tabName = "semi_conductors",
        
        # Bouton retour
        fluidRow(
          column(
            width = 12,
            div(
              style = "margin-bottom: 20px;",
              actionButton(
                "retour_accueil_semi_conductors", "Retour à l'accueil",
                icon = icon("arrow-left"),
                style = "background-color: #31708f; color: white; border: none; padding: 10px 20px; border-radius: 5px;"
              )
            )
          )
        ),
        
        # Encadré déroulant avec le contexte
        bs4Card(
          title = tagList(icon("microchip"), " Contexte"),
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12,
          
          tags$p(
            HTML(
              "Cet onglet explore la <strong>répartition mondiale de la production de semi-conducteurs</strong>, éléments essentiels à la fabrication des équipements électroniques. 
      Une <span style='color:#0073e6;'>carte interactive</span> permet de visualiser, par pays, la <strong>part de production</strong> et le <strong>nombre d’entreprises</strong> actives dans ce secteur stratégique."
            )
          ),
          
          tags$p(
            HTML(
              "Un <strong>tableau synthétique</strong> présente le <span style='color:#f39c12;'>top 5 des pays producteurs</span>, avec des indicateurs sur la concentration industrielle."
            )
          ),
          
          tags$p(
            HTML(
              "Un <strong>diagramme comparatif</strong> illustre la <span style='color:#28a745;'>consommation moyenne d’eau quotidienne</span> utilisée par les usines de production de semi-conducteurs, 
      exprimée en équivalent <strong>piscines olympiques</strong> 🏊. Cette visualisation met en lumière l’impact environnemental souvent méconnu de cette industrie."
            )
          ),
          
          tags$p(
            HTML(
              "<strong>Les semi-conducteurs sont au cœur des infrastructures numériques</strong> : 
      ils constituent la base des processeurs <em>(CPU, GPU)</em>, des mémoires, des interfaces de réseau et des composants de stockage. 
      Les <span style='color:#17a2b8;'>data centers</span>, en particulier ceux destinés à l’<strong>intelligence artificielle</strong>, nécessitent des volumes massifs de puces hautes performances."
            )
          ),
          
          tags$p(
            HTML(
              "Dans l’étude de cas d’<strong>Eybens (Isère)</strong>, le futur data center prévoit l’installation de <span style='color:#e83e8c;'>25 000 GPU</span>. 
      Chaque GPU contient plusieurs dizaines de semi-conducteurs (cœur graphique, mémoire HBM, contrôleurs, etc.), soit un besoin estimé à 
      <strong style='color:#dc3545;'>plus de 100 000 puces</strong> pour cette seule infrastructure. 
      Cela souligne la <strong>dépendance technologique critique</strong> de ces centres à l’égard de l’industrie des semi-conducteurs."
            )
          )
        ),
        
        # Titre
        fluidRow(
          column(
            width = 12,
            div(
              style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px; text-align: center;",
              h2(icon("microchip"), "Semi-conducteurs", 
                 style = "color: #31708f; font-weight: bold; margin: 0;")
            )
          )
        ),
        
        ##### 1.2.1. Carte de production des semi-conducteurs ----
        fluidRow(
          column(
            width = 12,
            div(
              style = "background: #f9f9f9; padding: 20px; border-radius: 8px;
                 box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px;",
              h3(icon("globe"), "Production mondiale de semi-conducteurs", style = "color: #31708f; margin-bottom: 15px;"),
              withSpinner(leafletOutput("map_semi_conductors", height = "500px"), type = 6, color = "#444"),
              tags$p("Répartition mondiale de la production de semi-conducteurs par pays.",
                     style = "margin-top: 15px; font-size: 16px; color: #555;")
            )
          )
        ),
        
        fluidRow(
          
          ##### 1.2.2. Infographie de la consommation d'eau ----
          column(
            width = 6,
            div(
              style = "background: #f9f9f9; padding: 20px; border-radius: 8px;
                 box-shadow: 0 2px 6px rgba(0,0,0,0.1); min-height: 520px; display: flex; flex-direction: column; justify-content: space-between;",
              
              div(
                style = "text-align: left;",
                h3(icon("th"), "Consommation d’eau", style = "color: #31708f;"),
                tags$p("Infographie simplifiée", style = "font-weight: bold; color: #333; margin-bottom: 3px;")
              ),
              
              div(
                style = "display: flex; justify-content: center; align-items: center;",
                plotOutput("infographie_eau", height = "300px", width = "95%")
              ),
              
              tags$div(
                style = "background: #eef7fc; padding: 12px; border-radius: 6px; margin-top: 15px;",
                tags$ul(
                  style = "padding-left: 20px; margin-bottom: 5px;",
                  tags$li(HTML("<strong>1 carré</strong> = <strong>1 million de litres</strong>")),
                  tags$li(HTML("<span style='color:#f94144; font-weight:bold;'>Rouge</span> : entreprise de semi-conducteurs")),
                  tags$li(HTML("<span style='color:#277da1; font-weight:bold;'>Bleu</span> : piscine olympique"))
                )
              )
            )
          ),
          
          ##### 1.2.3. Top 5 des pays producteurs de semi-conducteurs ----
          column(
            width = 6,
            div(
              style = "background: #f9f9f9; padding: 20px; border-radius: 8px;
                 box-shadow: 0 2px 6px rgba(0,0,0,0.1); min-height: 520px; display: flex; flex-direction: column; justify-content: space-between;",
              
              div(
                h3(icon("flag"), "Top 5 pays producteurs", style = "color: #31708f; margin-bottom: 15px;"),
                DTOutput("top5_semi_conductors")
              ),
              
              tags$div(
                style = "background: #eef7fc; padding: 12px; border-radius: 6px; margin-top: 15px;",
                tags$ul(
                  style = "padding-left: 20px; margin-bottom: 5px;",
                  tags$li(HTML("<strong>Taiwan</strong> produit à lui seul <strong>50&nbsp;%</strong> de tous les semi-conducteurs mondiaux.")),
                  tags$li(HTML("Les États-Unis, le Japon et la Corée du Sud assurent ensemble environ un tiers de la production mondiale."))
                )
              )
            )
          )
        )
      ),
      
      ###
      ### 2. Répratition des data centres ----
      ###
      ####
      #### 2.1. Data centers en Europe ----
      ####
      
      bs4TabItem(
        tabName = "dc_europe_map",
        
        # Bouton retour
        fluidRow(
          column(
            width = 12,
            div(
              style = "margin-bottom: 20px;",
              actionButton("retour_accueil_dc_europe", "Retour à l'accueil", 
                           icon = icon("arrow-left"),
                           style = "background-color: #31708f; color: white; border: none; padding: 10px 20px; border-radius: 5px;")
            )
          )
        ),
        
        # Titre
        fluidRow(
          column(
            width = 12,
            div(
              style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px; text-align: center;",
              h2(icon("earth-europe"), "Répartition des data centers en Europe", 
                 style = "color: #31708f; font-weight: bold; margin: 0;")
            )
          )
        ),
        
        fluidRow(
          
          ##### 2.1.1. Carte de répartition des DC en Europe ----
          column(
            width = 6,
            div(
              style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px;",
              
              h3(icon("map-marked-alt"), "Répartition des DC en Europe", style = "color: #31708f; margin-bottom: 15px;"),
              
              withSpinner(leafletOutput("map1", height = "450px"), type = 6, color = "#444"),
              
              tags$p(
                "Visualisation géographique de la distribution des data centres à travers l'Europe, montrant les zones de concentration principale.",
                style = "margin-top: 15px; font-size: 16px; color: #555;"
              )
            )
          ),
          
          ##### 2.1.2. Graphique en barres de la répartition des DC en Europe ----
          column(
            width = 6,
            div(
              style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px;",
              
              h3(icon("chart-bar"), "Part du nombre des DC en Europe", style = "color: #31708f; margin-bottom: 15px;"),
              
              withSpinner(plotlyOutput("barPlot", height = "450px"), type = 6, color = "#444"),
              
              tags$p(
                "Répartition proportionnelle du nombre de data centres par pays européen, illustrant la dominance de certains marchés.",
                style = "margin-top: 15px; font-size: 16px; color: #555;"
              )
            )
          )
        ),
        
        ##### 2.1.3. Chiffres clés----
        fluidRow(
          style = "margin-bottom: 30px;",
          column(
            width = 4,
            div(
              style = "background: #f9f9f9; border-radius: 8px; padding: 20px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); text-align: center;",
              
              div(
                style = "margin-bottom: 15px;",
                icon("bolt", style = "color: #FFA500; font-size: 32px; margin-bottom: 10px;")
              ),
              
              h3(icon("bolt"), "Demande énergétique 2035", style = "color: #31708f; margin-bottom: 15px; font-size: 18px;"),
              
              div(style = "font-size: 28px; font-weight: bold; color: #CC8400; margin-bottom: 10px;", "236 TWh"),
              
              tags$p(
                "La consommation énergétique des data centres devrait plus que doubler d'ici 2035, atteignant 5,7% de la demande totale d'électricité européenne.",
                style = "font-size: 14px; color: #555; line-height: 1.4;"
              )
            )
          ),
          
          column(
            width = 4,
            div(
              style = "background: #f9f9f9; border-radius: 8px; padding: 20px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); text-align: center;",
              
              div(
                style = "margin-bottom: 15px;",
                icon("chart-line", style = "color: #DC143C; font-size: 32px; margin-bottom: 10px;")
              ),
              
              h3(icon("chart-line"), "Croissance 2024-2035", style = "color: #31708f; margin-bottom: 15px; font-size: 18px;"),
              
              div(style = "font-size: 28px; font-weight: bold; color: #B01030; margin-bottom: 10px;", "+146%"),
              
              tags$p(
                "Augmentation significative de la demande énergétique entre 2024 et 2035, reflétant l'expansion rapide du secteur numérique.",
                style = "font-size: 14px; color: #555; line-height: 1.4;"
              )
            )
          ),
          
          column(
            width = 4,
            div(
              style = "background: #f9f9f9; border-radius: 8px; padding: 20px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); text-align: center;",
              
              div(
                style = "margin-bottom: 15px;",
                icon("globe-europe", style = "color: #4682B4; font-size: 32px; margin-bottom: 10px;")
              ),
              
              h3(icon("map"), "Concentration géographique", style = "color: #31708f; margin-bottom: 15px; font-size: 18px;"),
              
              div(style = "font-size: 28px; font-weight: bold; color: #336699; margin-bottom: 10px;", "79%"),
              
              tags$p(
                "10 pays concentrent la majorité de la demande énergétique des data centres en Europe, soulignant l'importance des hubs technologiques.",
                style = "font-size: 14px; color: #555; line-height: 1.4;"
              )
            )
          )
        ),
        
        ##### 2.1.4. Graphique de l'évolution de la demande énergétique des DC ----
        fluidRow(
          column(
            width = 12,
            div(
              style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1);",
              
              h3(icon("bolt"), "Évolution de la demande énergétique", style = "color: #31708f; margin-bottom: 15px;"),
              
              withSpinner(plotOutput("dc_demand_plot", height = "390px"), type = 6, color = "#444"),
              
              tags$p(
                "Selon ICIS, la demande énergétique des data centres en Europe passera de 96 TWh en 2024 à 236 TWh en 2035, représentant alors 5,7 % de la demande totale d'électricité.",
                style = "margin-top: 15px; font-size: 16px; color: #555;"
              )
            )
          )
        )
       ),
      
      ####
      #### 2.2. Data centres dans les FLAP-D ----
      ####
      
      bs4TabItem(
        tabName = "flapd",
        
        # Bouton retour
        fluidRow(
          column(
            width = 12,
            div(
              style = "margin-bottom: 20px;",
              actionButton("retour_accueil_flapd", "Retour à l'accueil", 
                           icon = icon("arrow-left"),
                           style = "background-color: #31708f; color: white; border: none; padding: 10px 20px; border-radius: 5px;")
            )
          )
        ),
        
        # Titre
        fluidRow(
          column(
            width = 12,
            div(
              style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px;",
              
              h3(icon("city"), "FLAP-D : Principaux hubs européens", style = "color: #31708f; margin-bottom: 15px;"),
              
              tags$p(
                "Les villes de Francfort, Londres, Amsterdam, Paris et Dublin — connues sous l'acronyme FLAP-D — concentrent une part importante des infrastructures de data centres en Europe. Utilisez les boutons pour explorer chaque ville.",
                style = "font-size: 16px; color: #555;"
              )
            )
          )
        ),
        
        ##### 2.2.1. Carte de la répartition des DC dans les FLAP-D ----
        fluidRow(
          column(
            width = 12,
            div(
              style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1);",
              
              # Boutons de sélection
              fluidRow(
                column(2, actionButton("go_paris", "\U0001F1EB\U0001F1F7 Paris", class = "btn btn-outline-primary btn-block")),
                column(2, actionButton("go_london", "\U0001F1EC\U0001F1E7 London", class = "btn btn-outline-primary btn-block")),
                column(2, actionButton("go_amsterdam", "\U0001F1F3\U0001F1F1 Amsterdam", class = "btn btn-outline-primary btn-block")),
                column(2, actionButton("go_frankfurt", "\U0001F1E9\U0001F1EA Frankfurt", class = "btn btn-outline-primary btn-block")),
                column(2, actionButton("go_dublin", "\U0001F1EE\U0001F1EA Dublin", class = "btn btn-outline-primary btn-block")),
                column(2, actionButton("reset_vue", "Vue globale", icon = icon("globe"), class = "btn btn-outline-dark btn-block"))
              ),
              
              br(),
              
              # Texte explicatif
              tags$p(
                "️ Utilisez les boutons pour filtrer par ville ou revenir à la vue globale. Cliquez sur les points sur la carte pour afficher des informations détaillées.",
                style = "font-size: 14px; color: #555;"
              ),
              
              withSpinner(leafletOutput("map", height = "650px"), type = 6, color = "#444")
            )
          )
        )
       ),
    
    
    ####
    #### 2.3. Data centres en France ----
    ####
    
    
    
    ###
    ### 3. Bilan énergétique ----
    ###
    ####
    #### 3.1. Énergie en France----
    ####
    
    bs4TabItem(
      tabName = "regions",
      
      # Bouton retour
      fluidRow(
        column(
          width = 12,
          div(
            style = "margin-bottom: 20px;",
            actionButton("retour_accueil_regions", "Retour à l'accueil", 
                         icon = icon("arrow-left"),
                         style = "background-color: #31708f; color: white; border: none; padding: 10px 20px; border-radius: 5px;")
          )
        )
      ),
      
      # Titre
      fluidRow(
        column(
          width = 12,
          div(
            style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px; text-align: center;",
            h2(icon("leaf"), "Analyse régionale de la production et consommation d'énergie", 
               style = "color: #31708f; font-weight: bold; margin: 0;")
          )
        )
      ),
      
      fluidRow(
        ##### 3.1.1. Carte de production et consommation énergétique par région avec des cercles proportionnels ----
        column(
          width = 6,
          div(
            style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px; height: 600px; display: flex; flex-direction: column;",
            
            h3(icon("map"), "Consommation vs Production", style = "color: #31708f; margin-bottom: 15px;"),
            
            selectInput(
              inputId = "choix_map",
              label = "Choisir l'indicateur à afficher :",
              choices = c("Consommation totale brute" = "conso", 
                          "Production totale" = "prod"),
              selected = "prod"
            ),
            
            div(
              style = "flex-grow: 1;",
              withSpinner(leafletOutput("map_totale", height = "450px"), type = 6, color = "#444")
            )
          )
        ),
        
        ##### 3.1.2. Camambert de répartition de la productin énergétique par filière ----
        column(
          width = 6,
          div(
            style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px; height: 600px; display: flex; flex-direction: column;",
            
            h3(icon("chart-pie"), "Production d'énergie par filière", style = "color: #31708f; margin-bottom: 15px;"),
            
            selectInput("region_select", "Choisir une région :", 
                        choices = c("France", regions$NOM), selected = "France"),
            
            div(
              style = "flex-grow: 1;",
              withSpinner(plotlyOutput("pie_chart", height = "450px"), type = 6, color = "#444")
            )
          )
        )
      ),
      
      ##### 3.1.3. Évolution de la production et consommation énergéttique par filière ----
      fluidRow(
        column(
          width = 12,
          div(
            style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px;",
            
            h3(icon("chart-area"), "Évolution de la production par filière", style = "color: #31708f; margin-bottom: 15px;"),
            
            withSpinner(plotlyOutput("area_chart", height = "320px"), type = 6, color = "#444")
          )
        )
      ),
      
      fluidRow(
        
        ##### 3.1.4. Carte de typologie des régions françaises en fonction du bilan énergétique ----
        column(
          width = 6,
          div(
            style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px; height: 550px; display: flex; flex-direction: column;",
            
            h3(icon("plug"), "Qui alimente la France ?", style = "color: #31708f; margin-bottom: 15px;"),
            
            div(
              style = "flex-grow: 1;",
              withSpinner(leafletOutput("map6", height = "400px"), type = 6, color = "#444")
            )
          )
        ),
        
        ##### 3.1.5. Graphique en radar de Production et Consommation énergétique par région ----
        column(
          width = 6,
          div(
            style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px; height: 550px; display: flex; flex-direction: column;",
            
            h3(icon("balance-scale"), "Graphique", style = "color: #31708f; margin-bottom: 15px;"),
            
            div(
              style = "flex-grow: 1;",
              withSpinner(plotlyOutput("radar_chart", height = "400px"), type = 6, color = "#444")
            )
          )
        )
      )
    ),
    
    ####
    #### 3.2. Énergie en Auvergne-Rhone-Alpes ----
    ####
    
    bs4TabItem(
      tabName = "ara",
      
      # Bouton
      fluidRow(
        column(
          width = 12,
          div(
            style = "margin-bottom: 20px;",
            actionButton("retour_accueil_regions", "Retour à l'accueil", 
                         icon = icon("arrow-left"),
                         style = "background-color: #31708f; color: white; border: none; padding: 10px 20px; border-radius: 5px;")
          )
        )
      ),
      
      # Titre
      fluidRow(
        column(
          width = 12,
          div(
            style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px; text-align: center;",
            h2(icon("mountain"), "Auvergne-Rhône-Alpes", 
               style = "color: #31708f; font-weight: bold; margin: 0;")
          )
        )
      ),
      
      fluidRow(
        
        ##### 3.2.1. Carte de la consommation énergétique des EPCI ----
        column(
          width = 6,
          div(
            style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px;",
            
            h3(icon("bolt"), "Consommation totale", style = "color: #31708f; margin-bottom: 15px;"),
            withSpinner(leafletOutput("map_ara_totale", height = "530px"), type = 6, color = "#444"),
            p(
              "Carte représentant la consommation énergétique totale par EPCI",
              style = "margin-top: 10px; font-size: 0.9em; color: #555;"
            )
          )
        ),
        
        ##### 3.2.2. Carte de la consommation énergétique des EPCI par habitant ----
        column(
          width = 6,
          div(
            div(
              style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px;",
              
              h3(icon("user"), "Consommation par habitant", style = "color: #31708f; margin-bottom: 15px;"),
              
            withSpinner(leafletOutput("map_ara_hab", height = "530px"), type = 6, color = "#444"),
            p(
              "Carte représentant la consommation énergétique ramenée à la population.",
              style = "margin-top: 10px; font-size: 0.9em; color: #555;"
            )
          )
        )
      )
     ),
     
     ##### 3.2.3. Installation énergétique dans la région ----
     fluidRow(
       column(
         width = 12,
         div(
           style = "background: #f9f9f9; padding: 20px; border-radius: 8px;
               box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px;",
           
           h3(icon("industry"), "Centrales hydroélectriques et nucléaires",
              style = "color: #31708f; margin-bottom: 15px;"),
           
           withSpinner(leafletOutput("map_centrales", height = "600px"), type = 6, color = "#444"),
           
           p(
             "Carte représentant les centrales nucléaires et hydroélectriques avec des cercles proportionnels à leur puissance.",
             style = "margin-top: 10px; font-size: 0.9em; color: #555;"
           )
         )
       )
     )
    ),
    
    ###
    ### 4. Simulations ----
    ###
    #### 
    #### 4.1. Simulation 1 - Analyse prédictive ----
    ####
    
    tabItem(
      tabName = "sim1",
      fluidRow(
        column(
          width = 12,
          
          # Bouton retour
          fluidRow(
            column(
              width = 6,
              div(
                style = "margin-bottom: 20px;",
                actionButton("retour_accueil_sim1", "Retour à l'accueil", 
                             icon = icon("arrow-left"),
                             style = "background-color: #31708f; color: white; border: none; padding: 10px 20px; border-radius: 5px;")
              )
            ),
            
            # Guide d'utilisation
            column(
              width = 6,
              div(
                style = "text-align: right; margin-bottom: 20px;",
                tags$a(
                  href = "user_guide/user_guide_sim1.pdf", 
                  target = "_blank", 
                  class = "btn btn-success",
                  style = "padding: 10px 20px; border-radius: 5px;",
                  icon("file-pdf"), " Guide d'utilisation"
                )
              )
            )
          ),
          
          
          ##### 4.1.1. Boîte d'explication ----
          bs4Card(
            title = "ℹ️ À propos de cette simulation",
            status = "info",
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            p("Cette simulation a pour objectif de comparer la consommation électrique projetée d'un ou plusieurs data centers (DC) avec la production totale d'énergie en France selon le rapport de RTE, sur la période 2025–2035."),
            p("Les projections de consommation sont établies à partir des estimations de puissance du data center actuellement en construction à Éybens."),
            
            tags$hr(),
            
            p(strong("📈 Hypothèses d'évolution :"), "Les prévisions suivent les étapes de développement du projet Data One :"),
            tags$ul(
              tags$li("2025 : 15 MW"),
              tags$li("2026 : 200 MW"),
              tags$li("2028 : 400 MW"),
              tags$li("2035 : 1 000 MW")
            ),
            
            p("🏗️ La simulation permet d'extrapoler jusqu'à 35 data centers, en cohérence avec les ambitions exprimées par les pouvoirs publics en matière d'infrastructures numériques, notamment dans le cadre du développement de l'intelligence artificielle."),
            
            tags$hr(),
            
            p(strong("📊 Représentation graphique :")),
            tags$ul(
              tags$li("Les points rouges indiquent la consommation cumulée des data centers ajoutée à la consommation énergétique 2024 (Consommation simulée)"),
              tags$li("La courbe verte représente la trajectoire de référence de la production énergétique nationale"),
              tags$li("La courbe bleue représente la trajectoire de référence de la consommation énergétqiue nationale"),
              tags$li("Les pointillés verts/bleus indiquent les variations min/max des différents scénarios RTE")
            ),
            
            tags$hr(),
            
            p(strong("⚡ Équivalent en unités de production :"), "La simulation permet de comparer la consommation projetée des data centers en 2035 avec la production nécessaire par filière :"),
            tags$ul(
              tags$li("Réacteurs nucléaires"),
              tags$li("Grands barrages hydrauliques"),
              tags$li("Centrales à charbon"),
              tags$li("Éoliennes"),
              tags$li("Panneaux solaires"),
              tags$li("Centrales à biomasse")
            ),
            
            tags$hr(),
            
            p(strong("💡 Conversion des unités :"), "Pour comparer les consommations projetées, il est nécessaire de convertir les unités de GW en TWh/an selon la formule :"),
            p(em("Énergie annuelle (GWh/an) = Puissance (GW) × 24 heures × 365 jours")),
            p("Exemple pour un data center d'une puissance d'1 GW et un facteur de charge de 60 % : 1 × 24 × 365 × 0,6 = 5 256 GWh/an = 5,26 TWh/an"),
            
            tags$hr(),
            
            p(strong("🎯 Objectif :"), "Cette simulation vise à éclairer les enjeux d'articulation entre les besoins énergétiques croissants des infrastructures numériques et les capacités de production énergétique du pays dans une perspective de planification énergétique à long terme.")
          )
        )
      ),
      
      ##### 4.1.2. Paramètres de simulation ----
      div(style = "margin-bottom: 30px;",
          fluidRow(
            column(6,
                   div(class = "modern-energy-card", 
                       style = "border-left-color: #8b5cf6; min-height: 360px;",
                       
                       # En-tête
                       div(class = "energy-card-header",
                           div(class = "energy-icon-circle", 
                               style = "background: linear-gradient(135deg, #8b5cf6, #7c3aed);",
                               icon("sliders-h")
                           ),
                           div(style = "text-align: right;",
                               h3("Configuration", class = "energy-value-large", style = "color: #8b5cf6;"),
                               p("Simulation interactive", class = "energy-capacity-info")
                           )
                       ),
                       
                       # Contenu
                       div(
                         p("Paramètres de simulation", class = "energy-subtitle-text"),
                         
                         # Curseur DC
                         sliderInput("nb_dc", 
                                     label = div("Nombre de Data Centers", 
                                                 style = "font-weight: 600; color: #495057; margin-bottom: 10px;"),
                                     min = 1, 
                                     max = 35, 
                                     value = 1,
                                     step = 1,
                                     ticks = FALSE),
                         
                         # Curseur facteur de charge
                         sliderInput("facteur_charge", 
                                     label = div("Facteur de charge (%)", 
                                                 style = "font-weight: 600; color: #495057; margin-bottom: 10px;"),
                                     min = 0, 
                                     max = 100, 
                                     value = 100,
                                     step = 1,
                                     ticks = FALSE)
                         
                       )
                   )
            ),
            
            ##### 4.1.3. Données de référance ----
            column(6,
                   div(class = "modern-energy-card", 
                       style = "border-left-color: #f59e0b; min-height: 360px;",
                       
                       # En-tête
                       div(class = "energy-card-header",
                           div(class = "energy-icon-circle", 
                               style = "background: linear-gradient(135deg, #f59e0b, #d97706);",
                               icon("info-circle")
                           ),
                           div(style = "text-align: right;",
                               h3("Référence", class = "energy-value-large", style = "color: #f59e0b;"),
                               p("Données de base", class = "energy-capacity-info")
                           )
                       ),
                       
                       # Contenu
                       div(
                         p("Données de référence", class = "energy-subtitle-text"),
                         div(style = "margin-top: 15px;",
                             tags$ul(
                               style = "list-style-type: none; padding-left: 0; margin-bottom: 0;",
                               
                               # Production
                               tags$li(
                                 style = "margin-bottom: 10px; display: flex; align-items: flex-start; font-size: 13px;",
                                 div(class = "energy-icon-circle", 
                                     style = "background: linear-gradient(135deg, #226D68, #226D68); width: 20px; height: 20px; margin-right: 10px; margin-top: 2px; display: flex; align-items: center; justify-content: center;",
                                     icon("industry", style = "font-size: 12px; color: white;")
                                 ),
                                 div("Production de départ (2024) : ", 
                                     tags$strong("538 TWh", style = "color: #f59e0b;")
                                 )
                               ),
                               
                               # Consommation actuelle
                               tags$li(
                                 style = "margin-bottom: 10px; display: flex; align-items: flex-start; font-size: 13px;",
                                 div(class = "energy-icon-circle", 
                                     style = "background: linear-gradient(135deg, #6c757d, #495057); width: 20px; height: 20px; margin-right: 10px; margin-top: 2px; display: flex; align-items: center; justify-content: center;",
                                     icon("home", style = "font-size: 12px; color: white;")
                                 ),
                                 div("Consommation de départ (2024) : ", 
                                     tags$strong("442 TWh", style = "color: #f59e0b;")
                                 )
                               ),
                               
                               # Points rouges DC
                               tags$li(
                                 style = "margin-bottom: 15px; display: flex; align-items: flex-start; font-size: 13px;",
                                 div(class = "energy-icon-circle", 
                                     style = "background: linear-gradient(135deg, #D46F4D, #D46F4D); width: 20px; height: 20px; margin-right: 10px; margin-top: 2px; display: flex; align-items: center; justify-content: center;",
                                     icon("diamond", style = "font-size: 12px; color: white;")
                                 ),
                                 div("Points rouge : paliers DC (2025, 2026, 2028, 2035)")
                               )
                             )
                         ),
                         
                         # Facteur de charge
                         div(style = "margin-top: 20px; text-align: center;",
                             tags$span(icon("cogs"), style = "margin-right: 6px; color: #8b5cf6;"),
                             tags$span(textOutput("facteur_charge_affiche"), 
                                       style = "font-weight: 600; color: #8b5cf6;")
                         )
                       )
                   )
            )
          )
      ),
      
      ##### 4.1.4. Tendances de consommation et production énergétique entre 2000 et 2050
      fluidRow(
        column(
          width = 12,
          div(
            h3(icon("chart-line"), "Tendances de consommation et production énergétique entre 2000 et 2050", class = "section-title"),
            withSpinner(plotOutput("energiePlot", height = "300px"), type = 6, color = "#444")
          )
        )
      ),
      
      
      ##### 4.1.5. Graphique principal ----
      fluidRow(
        column(
          width = 12,
          div(
            h3(
              icon("chart-line", style = "margin-right: 8px;"),
              "Simulation de Projection : Production vs Consommation Énergétique 2025–2035",
              class = "section-title"
            ),
            withSpinner(plotlyOutput("energy_plot"), type = 6, color = "#444"),
            p(
              "Les lignes de référence suivent les scénarios présentés dans le rapport ",
              tags$em("Futurs énergétique 2050"),
              " de RTE publié en 2022.",
              style = "margin-top: 12px; font-size: 0.9em; color: #555;"
            )
          )
        )
      ),
      
      br(), br(),
      fluidRow(
        column(12,
               h2("Équivalent en unités de production en 2035 :", class = "section-title")
        )
      ),
      
      ##### 4.1.6. Équivalent en unités de production
      # Première ligne : nucléaire, hydraulique, charbon
      fluidRow(
        column(4,
               div(class = "modern-energy-card", 
                   style = "border-left-color: #f97316;",
                   div(class = "energy-card-header",
                       div(class = "energy-icon-circle", 
                           style = "background: linear-gradient(135deg, #f97316, #ea580c);",
                           icon("atom")
                       ),
                       div(style = "text-align: right;",
                           h3(textOutput("nuke_value"), class = "energy-value-large", style = "color: #f97316;"),
                           h6(p("Réacteurs nucléaires en France - 56"), class = "energy-capacity-info")
                       )
                   ),
                   div(
                     p("Réacteurs nucléaires", class = "energy-subtitle-text")
                   )
               )
        ),
        
        column(4,
               div(class = "modern-energy-card", 
                   style = "border-left-color: #3b82f6;",
                   div(class = "energy-card-header",
                       div(class = "energy-icon-circle", 
                           style = "background: linear-gradient(135deg, #3b82f6, #1d4ed8);",
                           icon("tint")
                       ),
                       div(style = "text-align: right;",
                           h3(textOutput("hydro_value"), class = "energy-value-large", style = "color: #3b82f6;")
                       )
                   ),
                   div(
                     p("Grands barrages", class = "energy-subtitle-text")
                   )
               )
        ),
        
        column(4,
               div(class = "modern-energy-card", 
                   style = "border-left-color: #6b7280;",
                   div(class = "energy-card-header",
                       div(class = "energy-icon-circle", 
                           style = "background: linear-gradient(135deg, #6b7280, #4b5563);",
                           icon("industry")
                       ),
                       div(style = "text-align: right;",
                           h3(textOutput("coal_value"), class = "energy-value-large", style = "color: #6b7280;")
                       )
                   ),
                   div(
                     p("Centrales à charbon", class = "energy-subtitle-text")
                   )
               )
        )
      ),
      
      # Deuxième ligne : Éolien, Solaire, Biomasse
      fluidRow(
        column(4,
               div(class = "modern-energy-card", 
                   style = "border-left-color: #14b8a6;",
                   div(class = "energy-card-header",
                       div(class = "energy-icon-circle", 
                           style = "background: linear-gradient(135deg, #14b8a6, #0d9488);",
                           icon("wind")
                       ),
                       div(style = "text-align: right;",
                           h3(textOutput("wind_value"), class = "energy-value-large", style = "color: #14b8a6;"),
                           h6(textOutput("wind_surface"), class = "energy-capacity-info")
                       )
                   ),
                   div(
                     p("Éoliennes terrestres", class = "energy-subtitle-text")
                   )
               )
        ),
        
        column(4,
               div(class = "modern-energy-card", 
                   style = "border-left-color: #eab308;",
                   div(class = "energy-card-header",
                       div(class = "energy-icon-circle", 
                           style = "background: linear-gradient(135deg, #eab308, #ca8a04);",
                           icon("sun")
                       ),
                       div(style = "text-align: right;",
                           h3(textOutput("solar_value"), class = "energy-value-large", style = "color: #eab308;"),
                           h6(textOutput("solar_surface"), class = "energy-capacity-info")  # <-- ici
                       )
                   ),
                   div(
                     p("Installations photovoltaïques", class = "energy-subtitle-text")
                   )
               )
        ),
        
        column(4,
               div(class = "modern-energy-card", 
                   style = "border-left-color: #22c55e;",
                   div(class = "energy-card-header",
                       div(class = "energy-icon-circle", 
                           style = "background: linear-gradient(135deg, #22c55e, #16a34a);",
                           icon("leaf")
                       ),
                       div(style = "text-align: right;",
                           h3(textOutput("bio_value"), class = "energy-value-large", style = "color: #22c55e;")
                       )
                   ),
                   div(
                     p("Centrales à biomasse", class = "energy-subtitle-text")
                   )
               )
        )
      ),
      valueBoxOutput("wind_surface_box"),
      valueBoxOutput("solar_surface_box"),
      
      htmlOutput("surface_info")  # boîte d'info explicative
    ), 
    
    
    
    ####
    #### 4.2. Simulation 2 - Analyse comparative ----
    ####
    
    tabItem(
      tabName = "sim2",
      
      fluidRow(
        column(
          width = 12,
          
          fluidRow(
            
            # Bouton retour
            column(
              width = 6,
              div(
                style = "margin-bottom: 20px;",
                actionButton("retour_accueil_sim2", "Retour à l'accueil", 
                             icon = icon("arrow-left"),
                             style = "background-color: #31708f; color: white; border: none; padding: 10px 20px; border-radius: 5px;")
              )
            )
            
            # Guide d'utilisation
            #column(
              #width = 6,
              #div(
                #style = "text-align: right; margin-bottom: 20px;",
                #tags$a(
                  #href = "user_guide/user_guide_sim2.pdf", 
                  #target = "_blank", 
                  #class = "btn btn-success",
                  #style = "padding: 10px 20px; border-radius: 5px;",
                  #icon("file-pdf"), " Guide d'utilisation"
                #)
              #)
            #)
          ),
          
          ##### 4.2.1. Boite d'explication ----
          bs4Card(
            title = "ℹ️ À propos de cette simulation",
            status = "info",
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            p("Ce graphique permet de représenter et de comparer le nombre d'habitants équivalents pour chaque palier de consommation du data center d'Eybens entre 2025 et 2035. Et ce, en prenant des exemples de profils de consommation par personne à travers le monde et en France."),
            p("Les barres représentent le nombre d'habitants équivalents selon la consommation moyenne."),
            p("Cochez les profils pour adapter la simulation."),
            tags$hr(),
            p(strong("🔍 Estimation initiale :"), "La consommation du DC est basée sur le data center actuellement en construction à Éybens."),
            p(strong("📈 Évolution prévue :"), "Les projections suivent les plans de développement de Data One :"),
            tags$ul(
              tags$li("2025 : 15 MW"),
              tags$li("2026 : 200 MW"),
              tags$li("2028 : 400 MW"),
              tags$li("2035 : 1 000 MW")
            ),
            tags$hr(),
            p(strong("💡 Conversion des unités :"), "Pour comparer les consommations projetées de Data One aux consommations annuelles moyennes d'individus, il est nécessaire de convertir l'unité des projections de Data One (exprimées en GW) afin d'obtenir des valeurs en GWh/an. Pour ce faire, on applique la formule suivante :"),
            p(em("Énergie annuelle (en GWh/an) = Puissance (GW) × nombre d'heures d'utilisation par jour × nombre de jours d'utilisation par an")),
            p("Par exemple, calculons la conversion de la projection de 2035 pour 1 GW :"),
            p(em("Énergie annuelle (GWh) = 1 × 24 × 365 = 8 760 GWh/an")),
            tags$ul(
              tags$li("Ou encore 8 760 000 000 kWh/an"),
              tags$li("Soit 8 760 000 MWh/an"),
              tags$li("Ou l'équivalent de 8,76 TWh/an")
            ),
            p("On peut donc diviser les différentes consommations annuelles projetées par la consommation moyenne souhaitée pour obtenir le nombre d'individus équivalents.")
          )
        )
      ),
      
      ##### 4.2.2. Graphique 1 - Comparaison avec consommation par pays ----
      fluidRow(
        bs4Card(
          title = "Simulation : Comparaison avec la consommation par habitant à travers le monde",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          fluidRow(
            column(
              width = 4,
              uiOutput("checkbox_group_conso")
            ),
            column(
              width = 8,
              plotlyOutput("barplot")
            )
          ),
          p(),
          p(strong("💡 Aide d'interprétation pour l'échelle mondiale :"),"Pour un data center d’une puissance de 1 GW, cela correspond à la consommation énergétique annuelle de 3 275 991 personnes, basée sur la moyenne mondiale de 2,674 MWh par personne et par an."),
          footer = "Sources : Ministère de la Transistion Écologique et de la Cohésion des Territoires : Chiffres clés de l'énergie, 2024"
        )
      ),
      
      ##### 4.2.3. Encarts info pour les habitants équivalents pour le Mali, le Qatar et la France ----
      fluidRow(
        column(4,
               div(class = "modern-energy-card", 
                   style = "border-left-color: #22c55e;",
                   div(class = "energy-card-header",
                       div(class = "energy-icon-circle", 
                           style = "background: linear-gradient(135deg, #22c55e, #15803d);",
                           icon("leaf")
                       ),
                       div(style = "text-align: right;",
                           h3(textOutput("qatar_1gw"), class = "energy-value-large", style = "color: #22c55e;"),
                           p(textOutput("qatar_pop"), class = "energy-subtitle-text", style = "margin-top: -10px; font-size: 0.9em; color: #4b5563;")
                       )
                   ),
                   div(
                     p("Habitants équivalents – Qatar", class = "energy-subtitle-text")
                   )
               )
        ),
        column(4,
               div(class = "modern-energy-card", 
                   style = "border-left-color: #eab308;",
                   div(class = "energy-card-header",
                       div(class = "energy-icon-circle", 
                           style = "background: linear-gradient(135deg, #eab308, #ca8a04);",
                           icon("leaf")
                       ),
                       div(style = "text-align: right;",
                           h3(textOutput("france_1gw"), class = "energy-value-large", style = "color: #eab308;"),
                           p(textOutput("france_pop"), class = "energy-subtitle-text", style = "margin-top: -10px; font-size: 0.9em; color: #4b5563;")
                       )
                   ),
                   div(
                     p("Habitants équivalents – France", class = "energy-subtitle-text")
                   )
               )
        ),
        column(4,
               div(class = "modern-energy-card", 
                   style = "border-left-color: #f43f5e;",
                   div(class = "energy-card-header",
                       div(class = "energy-icon-circle", 
                           style = "background: linear-gradient(135deg, #f43f5e, #be123c);",
                           icon("leaf")
                       ),
                       div(style = "text-align: right;",
                           h3(textOutput("mali_1gw"), class = "energy-value-large", style =  "color: #f43f5e;"),
                           p(textOutput("mali_pop"), class = "energy-subtitle-text", style = "margin-top: -10px; font-size: 0.9em; color: #4b5563;")
                       )
                   ),
                   div(
                     p("Habitants équivalents – Mali", class = "energy-subtitle-text")
                   )
               )
        )
      ),
      
      ##### 4.2.4. Graphique 2 - Simulation personnalisée ----
      fluidRow(
        bs4Card(
          title = "Simulation personnalisée : Comparer jusqu'à 8 consommations de votre choix",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          sidebarLayout(
            sidebarPanel(
              lapply(1:8, function(i) {
                cond <- if (i == 1) {
                  "true"
                } else {
                  prev_nom <- paste0("input.nom_perso_", i - 1)
                  prev_val <- paste0("input.val_perso_", i - 1)
                  paste0(prev_nom, " !== '' && ", prev_val, " > 0")
                }
                
                conditionalPanel(
                  condition = cond,
                  fluidRow(
                    column(6, textInput(paste0("nom_perso_", i), paste0("Entité ", i), value = paste("Perso", i))),
                    column(3, numericInput(paste0("val_perso_", i), "Valeur", value = NA, min = 0, step = 0.01)),
                    column(3, selectInput(paste0("unit_perso_", i), "Unité", choices = c("kWh/an", "MWh/an", "GWh/an"), selected = "MWh/an"))
                  )
                )
              })
            ),
            mainPanel(
              plotlyOutput("barplot_personalisee")
            )
          )
        )
      )
    )
   )
  )
 )
  
# END ----
