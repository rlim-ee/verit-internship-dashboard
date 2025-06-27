# USER INTERFACE ----
ui <- bs4DashPage(
  title = "Dashboard",
  fullscreen = TRUE,
  
  header = bs4DashNavbar(
    title = span("Tableau de bord", style = "font-weight: bold; color: #31708f;")
  ),
  
  ## Sidebar ----
  sidebar = bs4DashSidebar(
    title = "Menu",
    bs4SidebarMenu(
      id = "tabs",
      bs4SidebarMenuItem("Accueil", tabName = "home", icon = icon("home")),
      
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
  
  ## Main Body ----
  body = bs4DashBody(
    
    tags$head(
      # Import de la police Poppins depuis Google Fonts
      tags$link(href = "https://fonts.googleapis.com/css2?family=Poppins&display=swap", rel = "stylesheet"),
      
      # CSS personnalisé
      tags$style(HTML("
    body, h1, h2, h3, h4, h5, h6, .small-box, .nav-link, .brand-text {
      font-family: 'Poppins', sans-serif !important;
    }

    .main-header .navbar .navbar-brand .brand-text {
      font-weight: bold !important;
      font-size: 22px !important;
      color: #f5f6f7 !important;
    }

    .main-sidebar {
      background-color: #31708f !important;
      color: white !important;
    }

    .sidebar-dark-primary {
      background-color: #31708f !important;
      color: white !important;
    }

    .main-sidebar .nav-link {
      color: white !important;
    }

    .main-sidebar .nav-link.active {
      background-color: #265a6a !important;
      color: white !important;
    }

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

    .value-box-custom-teal {
      background: linear-gradient(135deg, #5FC2BA, #4FADAA) !important;
      color: white !important;
      border-radius: 15px !important;
      box-shadow: 0 4px 15px rgba(95, 194, 186, 0.3) !important;
    }

    .value-box-custom-navy {
      background: linear-gradient(135deg, #0B162C, #1A2B4C) !important;
      color: white !important;
      border-radius: 15px !important;
      box-shadow: 0 4px 15px rgba(11, 22, 44, 0.3) !important;
    }

    .value-box-custom-orange {
      background: linear-gradient(135deg, #FF6B35, #F7931E) !important;
      color: white !important;
      border-radius: 15px !important;
      box-shadow: 0 4px 15px rgba(255, 107, 53, 0.3) !important;
    }

    .value-box-custom-purple {
      background: linear-gradient(135deg, #6C5CE7, #A29BFE) !important;
      color: white !important;
      border-radius: 15px !important;
      box-shadow: 0 4px 15px rgba(108, 92, 231, 0.3) !important;
    }

    .value-box-custom-green {
      background: linear-gradient(135deg, #00B894, #00CEC9) !important;
      color: white !important;
      border-radius: 15px !important;
      box-shadow: 0 4px 15px rgba(0, 184, 148, 0.3) !important;
    }

    .small-box h3, .small-box h4 {
      font-weight: bold !important;
      font-size: 1.8rem !important;
      color: white !important;
      text-shadow: 1px 1px 2px rgba(0,0,0,0.3) !important;
    }

    .small-box p {
      font-weight: 600 !important;
      font-size: 1.1rem !important;
      color: white !important;
      text-shadow: 1px 1px 2px rgba(0,0,0,0.3) !important;
    }

    .small-box .icon {
      font-size: 4rem !important;
      opacity: 0.8 !important;
    }

    .small-box a {
      color: white !important;
      text-decoration: none !important;
      font-weight: bold !important;
      transition: all 0.3s ease !important;
    }

    .small-box a:hover {
      color: #f8f9fa !important;
      text-decoration: underline !important;
      transform: scale(1.05) !important;
    }

    .small-box {
      transition: transform 0.3s ease, box-shadow 0.3s ease !important;
    }

    .small-box:hover {
      transform: translateY(-5px) !important;
      box-shadow: 0 6px 20px rgba(0, 0, 0, 0.2) !important;
    }

    @media (max-width: 768px) {
      .small-box h3 {
        font-size: 1.4rem !important;
      }
      .small-box p {
        font-size: 1rem !important;
      }
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

    .progress-container {
      width: 100%;
      height: 6px;
      background-color: #e2e8f0;
      border-radius: 3px;
      overflow: hidden;
      margin-top: 10px;
    }

    .progress-bar-animated {
      height: 100%;
      border-radius: 3px;
      transition: width 1.2s ease-out;
      animation: shimmer 2s infinite;
    }

    @keyframes shimmer {
      0% { opacity: 0.8; }
      50% { opacity: 1; }
      100% { opacity: 0.8; }
    }

    .section-title {
      color: #1e293b;
      font-weight: 600;
      margin-bottom: 25px;
      padding-bottom: 10px;
      border-bottom: 3px solid #3b82f6;
      display: inline-block;
    }

    .energy-extra-info {
      margin-top: 15px;
      padding-top: 15px;
      border-top: 1px solid rgba(255, 255, 255, 0.1);
      flex-grow: 1;
    }

    .info-row {
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-bottom: 8px;
      font-size: 0.85em;
    }

    .info-row:last-child {
      margin-bottom: 0;
    }

    .info-label {
      color: #64748b;
      font-weight: 500;
    }

    .info-value {
      color: #1e293b;
      font-weight: 600;
      text-align: right;
    }

    @keyframes slideInUp {
      from {
        opacity: 0;
        transform: translateY(30px);
      }
      to {
        opacity: 1;
        transform: translateY(0);
      }
    }

    .row {
      margin-left: -15px;
      margin-right: -15px;
    }

    .row:after {
      content: '';
      display: table;
      clear: both;
    }
  "))
    ),
    
    bs4TabItems(
      
      ### Home Tab ----
      bs4TabItem(
        tabName = "home",
        
        # Project Description
        bs4Card(
          title = "Contexte",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12,
          tags$p("Les centres de données, infrastructures clés du numérique, connaissent une forte croissance en Europe avec une capacité énergétique estimée à 9,2 GW pouvant atteindre 26,6 GW d'ici 2035. Cette expansion soulève des préoccupations environnementales majeures, notamment l'accaparement de l'énergie renouvelable au détriment d'autres usages. La France, parmi les pays les plus dotés en data centers, mise sur son autonomie énergétique, en particulier grâce à la région Auvergne-Rhône-Alpes, grande productrice d'hydroélectricité."),
          tags$p("Dans ce contexte, la société française DataOne prévoit la construction de deux data centers surpuissants en Isère. Le site de Eybens devrait atteindre 1 GW de puissance d'ici 2035, avec une consommation exclusivement hydraulique selon les ambitions affichées. Ce projet illustre les enjeux liés au développement de l'intelligence artificielle, très énergivore, et met en lumière la nécessité d'un équilibre entre innovation technologique et durabilité énergétique."),
          tags$blockquote(
            "Les centres de données pourraient représenter 5,7 % de la demande totale d'électricité en Europe d'ici 2035.",
            style = "font-style: italic; color: #31708f;"
          )
        ),
        
        h3("Explorer le dashboard :", style = "margin-top: 20px; margin-bottom: 10px; font-weight: bold; color: #0B162C"),
        
        # Section: Répartition des data centers
        h5("Répartition des data centers", class = "section-header"),
        
        fluidRow(
          column(4,
                 div(class = "modern-energy-card", 
                     style = "border-left-color: #3b82f6;",  # bleu
                     div(class = "energy-card-header",
                         div(class = "energy-icon-circle", 
                             style = "background: linear-gradient(135deg, #3b82f6, #1e3a8a);",
                             icon("globe-europe")
                         ),
                         div(style = "text-align: right;",
                             h3(actionLink("go_dc_europe", "Europe", 
                                           style = "color: #3b82f6; text-decoration: none;"), 
                                class = "energy-value-large"),
                             p("Cliquer pour voir", class = "energy-capacity-info")
                         )
                     ),
                     div(
                       p("Data centers en Europe", class = "energy-subtitle-text")
                     )
                 )
          ),
          
          column(4,
                 div(class = "modern-energy-card", 
                     style = "border-left-color: #f97316;",  # orange
                     div(class = "energy-card-header",
                         div(class = "energy-icon-circle", 
                             style = "background: linear-gradient(135deg, #f97316, #c2410c);",
                             icon("network-wired")
                         ),
                         div(style = "text-align: right;",
                             h3(actionLink("go_flapd", "FLAP-D", 
                                           style = "color: #f97316; text-decoration: none;"), 
                                class = "energy-value-large"),
                             p("Cliquer pour voir", class = "energy-capacity-info")
                         )
                     ),
                     div(
                       p("Data centers dans les FLAP-D", class = "energy-subtitle-text")
                     )
                 )
          ),
          
          column(4,
                 div(class = "modern-energy-card", 
                     style = "border-left-color: #10b981;",  # vert
                     div(class = "energy-card-header",
                         div(class = "energy-icon-circle", 
                             style = "background: linear-gradient(135deg, #10b981, #065f46);",
                             icon("server")
                         ),
                         div(style = "text-align: right;",
                             h3(actionLink("go_dc_france", "France", 
                                           style = "color: #10b981; text-decoration: none;"), 
                                class = "energy-value-large"),
                             p("Cliquer pour voir", class = "energy-capacity-info")
                         )
                     ),
                     div(
                       p("Data centers en France", class = "energy-subtitle-text")
                     )
                 )
          )
        ),
        
        # Section: Bilan énergétique
        h5("Bilan énergétique", class = "section-header"),
        
        fluidRow(
          column(6,
                 div(class = "modern-energy-card", 
                     style = "border-left-color: #6366f1;",  # violet
                     div(class = "energy-card-header",
                         div(class = "energy-icon-circle", 
                             style = "background: linear-gradient(135deg, #6366f1, #4f46e5);",
                             icon("bolt")
                         ),
                         div(style = "text-align: right;",
                             h3(actionLink("go_regions", "France", 
                                           style = "color: #6366f1; text-decoration: none;"), 
                                class = "energy-value-large"),
                             p("Cliquer pour voir", class = "energy-capacity-info")
                         )
                     ),
                     div(
                       p("Énergie en France", class = "energy-subtitle-text")
                     )
                 )
          ),
          
          column(6,
                 div(class = "modern-energy-card", 
                     style = "border-left-color: #ec4899;",  # rose
                     div(class = "energy-card-header",
                         div(class = "energy-icon-circle", 
                             style = "background: linear-gradient(135deg, #ec4899, #be185d);",
                             icon("chart-area")
                         ),
                         div(style = "text-align: right;",
                             h3(actionLink("go_ara", "Auvergne-Rhône-Alpes", 
                                           style = "color: #ec4899; text-decoration: none;"), 
                                class = "energy-value-large"),
                             p("Cliquer pour voir", class = "energy-capacity-info")
                         )
                     ),
                     div(
                       p("Analyse régionale", class = "energy-subtitle-text")
                     )
                 )
          )
        ),
        
        h3("Explorer les simulations :", style = "margin-top: 20px; margin-bottom: 10px; font-weight: bold; color: #0B162C"),        

        
          fluidRow(
          column(6,
                 div(class = "modern-energy-card", 
                     style = "border-left-color: #14b8a6;",
                     div(class = "energy-card-header",
                         div(class = "energy-icon-circle", 
                             style = "background: linear-gradient(135deg, #14b8a6, #0f766e);",
                             icon("chart-line")
                         ),
                         div(style = "text-align: right;",
                             h3(actionLink("go_sim1", "Analyse prédictive", 
                                           style = "color: #14b8a6; text-decoration: none;"), 
                                class = "energy-value-large"),
                             p("Cliquer pour voir", class = "energy-capacity-info")
                         )
                     ),
                     div(
                       p("Projection énergétique", class = "energy-subtitle-text")
                     )
                 )
          ),
          
          column(6,
                 div(class = "modern-energy-card", 
                     style = "border-left-color: #06b6d4;",
                     div(class = "energy-card-header",
                         div(class = "energy-icon-circle", 
                             style = "background: linear-gradient(135deg, #06b6d4, #0891b2);",
                             icon("cogs")
                         ),
                         div(style = "text-align: right;",
                             h3(actionLink("go_sim2", "Analyse comparative", 
                                           style = "color: #06b6d4; text-decoration: none;"), 
                                class = "energy-value-large"),
                             p("Cliquer pour voir", class = "energy-capacity-info")
                         )
                     ),
                     div(
                       p("Comparaison de consommation", class = "energy-subtitle-text")
                     )
                 )
          )
        ),
        
        # Credits
        tags$div(
          style = "margin-top: 30px; text-align: right; font-size: 0.9em; color: #888;",
          "Auteur : Zoé Cargnelli & Robert Lim | Source : ICIS, Eurostat, DataCenterMap, RTE France | 2025"
        )
      ),
      
      ### 1.1 Data centres en Europe----
      bs4TabItem(
        tabName = "dc_europe_map",
        
        # Bouton retour avec style
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
        
        #### 1.1.1 Carte de répartition des DC en Europe + graphique----
        fluidRow(
          column(
            width = 6,
            div(
              style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px;",
              
              h3(icon("map-marked-alt"), "Répartition des DC en Europe", style = "color: #31708f; margin-bottom: 15px;"),
              
              leafletOutput("map1", height = "450px"),
              
              tags$p(
                "Visualisation géographique de la distribution des data centres à travers l'Europe, montrant les zones de concentration principale.",
                style = "margin-top: 15px; font-size: 16px; color: #555;"
              )
            )
          ),
          column(
            width = 6,
            div(
              style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px;",
              
              h3(icon("chart-bar"), "Part du nombre des DC en Europe", style = "color: #31708f; margin-bottom: 15px;"),
              
              plotlyOutput("barPlot", height = "450px"),
              
              tags$p(
                "Répartition proportionnelle du nombre de data centres par pays européen, illustrant la dominance de certains marchés.",
                style = "margin-top: 15px; font-size: 16px; color: #555;"
              )
            )
          )
        ),
        
        #### 1.1.2 Chiffres clés----
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
        
        #### 1.1.3 Graphique de l'évolution de la demande----
        fluidRow(
          column(
            width = 12,
            div(
              style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1);",
              
              h3(icon("bolt"), "Évolution de la demande énergétique", style = "color: #31708f; margin-bottom: 15px;"),
              
              plotOutput("dc_demand_plot", height = "390px"),
              
              tags$p(
                "Selon ICIS, la demande énergétique des data centres en Europe passera de 96 TWh en 2024 à 236 TWh en 2035, représentant alors 5,7 % de la demande totale d'électricité.",
                style = "margin-top: 15px; font-size: 16px; color: #555;"
              )
            )
          )
        )
       ),
      
      ### 1.2 Data centres dans les FLAP-D----
      bs4TabItem(
        tabName = "flapd",
        
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
        
        #### Titre et description ----
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
        
        #### Carte interactive + boutons de sélection ----
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
              
              leafletOutput("map", height = "650px")
            )
          )
        )
       ),
    
    
    
    ### 1.3 Data centres en France----
    
    
    
    
    ### 2.1 Énergie en France----
    bs4TabItem(
      tabName = "regions",
      
      # Bouton retour avec style
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
      
      # Titre principal avec style
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
      
      # Première ligne : carte + camembert
      fluidRow(
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
              leafletOutput("map_totale", height = "450px")
            )
          )
        ),
        column(
          width = 6,
          div(
            style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px; height: 600px; display: flex; flex-direction: column;",
            
            h3(icon("chart-pie"), "Production d'énergie par filière", style = "color: #31708f; margin-bottom: 15px;"),
            
            selectInput("region_select", "Choisir une région :", 
                        choices = c("France", regions$NOM), selected = "France"),
            
            div(
              style = "flex-grow: 1;",
              plotlyOutput("pie_chart", height = "450px")
            )
          )
        )
      ),
      
      # Deuxième ligne : évolution par filière
      fluidRow(
        column(
          width = 12,
          div(
            style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px;",
            
            h3(icon("chart-area"), "Évolution de la production par filière", style = "color: #31708f; margin-bottom: 15px;"),
            
            plotlyOutput("area_chart", height = "320px")
          )
        )
      ),
      
      # Troisième ligne : carte flux + radar
      fluidRow(
        column(
          width = 6,
          div(
            style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px; height: 550px; display: flex; flex-direction: column;",
            
            h3(icon("plug"), "Qui alimente la France ?", style = "color: #31708f; margin-bottom: 15px;"),
            
            div(
              style = "flex-grow: 1;",
              leafletOutput("map6", height = "400px")
            )
          )
        ),
        column(
          width = 6,
          div(
            style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px; height: 550px; display: flex; flex-direction: column;",
            
            h3(icon("balance-scale"), "Graphique", style = "color: #31708f; margin-bottom: 15px;"),
            
            div(
              style = "flex-grow: 1;",
              plotlyOutput("radar_chart", height = "400px")
            )
          )
        )
      )
    ),
    
    
    
    
    
    
    ### 2.2 Énergie en Auvergne-Rhone-Alpes----
    bs4TabItem(
      tabName = "ara",
      
      # Bouton retour avec style
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
      
      fluidRow(
        column(
          width = 6,
          div(
            style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px;",
            
            h3(icon("bolt"), "Consommation totale", style = "color: #31708f; margin-bottom: 15px;"),
            leafletOutput("map_ara_totale", height = "530px"),
            p(
              "Carte représentant la consommation énergétique totale par EPCI",
              style = "margin-top: 10px; font-size: 0.9em; color: #555;"
            )
          )
        ),
        column(
          width = 6,
          div(
            div(
              style = "background: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-bottom: 20px;",
              
              h3(icon("user"), "Consommation par habitant", style = "color: #31708f; margin-bottom: 15px;"),
              
            leafletOutput("map_ara_hab", height = "530px"),
            p(
              "Carte représentant la consommation énergétique ramenée à la population.",
              style = "margin-top: 10px; font-size: 0.9em; color: #555;"
            )
          )
        )
      )
     )
    ),
    
    ### Installation énergétique dans la région----
    
    ### 3.1 Sim1----
    tabItem(
      tabName = "sim1",
      fluidRow(
        column(
          width = 12,
          
          # Bouton retour à l'accueil
          fluidRow(
            column(
              width = 12,
              div(
                style = "margin-bottom: 20px;",
                actionButton("retour_accueil_sim1", "Retour à l'accueil", 
                             icon = icon("arrow-left"),
                             style = "background-color: #31708f; color: white; border: none; padding: 10px 20px; border-radius: 5px;")
              )
            )
          ),
          
          
          #### Boîte d'explication----
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
              tags$li("2025 : 15 MW (~0,13 TWh/an)"),
              tags$li("2026 : 200 MW (~1,75 TWh/an)"),
              tags$li("2028 : 400 MW (~3,50 TWh/an)"),
              tags$li("2035 : 1 000 MW (~8,76 TWh/an)")
            ),
            
            p("🏗️ La simulation permet d'extrapoler jusqu'à 35 data centers, en cohérence avec les ambitions exprimées par les pouvoirs publics en matière d'infrastructures numériques, notamment dans le cadre du développement de l'intelligence artificielle."),
            
            tags$hr(),
            
            p(strong("📊 Représentation graphique :")),
            tags$ul(
              tags$li("Les points rouges indiquent la consommation cumulée des data centers ajoutée à la consommation énergétique actuelle"),
              tags$li("La courbe verte représente la trajectoire de référence de la production énergétique nationale"),
              tags$li("Les pointillés encadrant cette courbe correspondent aux intervalles de variation (minima et maxima) issus des différents scénarios prospectifs élaborés par RTE"),
              tags$li("Le pointillé noir représente la trajectoire de référence de la production énergétique nationale")
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
            
            p(strong("💡 Conversion des unités :"), "Pour comparer les consommations projetées, il est nécessaire de convertir les unités de GW en GWh/an selon la formule :"),
            p(em("Énergie annuelle (GWh/an) = Puissance (GW) × 24 heures × 365 jours")),
            p("Exemple pour 2035 : 1 GW = 1 × 24 × 365 = 8 760 GWh/an = 8,76 TWh/an"),
            
            tags$hr(),
            
            p(strong("🎯 Objectif :"), "Cette simulation vise à éclairer les enjeux d'articulation entre les besoins énergétiques croissants des infrastructures numériques et les capacités de production énergétique du pays dans une perspective de planification énergétique à long terme.")
          )
        )
      ),
      
      #### Paramètres de simulation----
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
                                     ticks = FALSE),
                         
                       )
                   )
            ),
            
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
                                 div("Production de départ 2025 : ", 
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
                                 div("Consommation actuelle : ", 
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
                         
                         # Facteur de charge bien placé
                         div(style = "margin-top: 20px; text-align: center;",
                             span(icon("cogs"), style = "margin-right: 6px; color: #8b5cf6;"),
                             span(textOutput("facteur_charge_affiche"), 
                                  style = "font-weight: 600; color: #8b5cf6;")
                         )
                       )
                   )
            )
          )
      ),
      
      fluidRow(
        column(
          width = 12,
          div(
            h3(icon("chart-line"), "Tendances de consommation et production énergétique entre 2000 et 2050", class = "section-title"),
            plotOutput("energiePlot", height = "300px")
          )
        )
      ),
      
      
      #### Graphique principal----
      fluidRow(
        column(
          width = 12,
          div(
            h3(
              icon("chart-line", style = "margin-right: 8px;"),
              "Simulation de Projection : Production vs Consommation Énergétique 2025–2035",
              class = "section-title"
            ),
            plotlyOutput("energy_plot", height = "600px"),
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
      
      # Première ligne : Nucléaire, Hydro, Charbon
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
    
    
    
    
    ### 3.2 Sim2 ----
    tabItem(
      tabName = "sim2",
      
      #### Boite d'explication----
      fluidRow(
        column(
          width = 12,
          
          
          fluidRow(
            column(
              width = 12,
              div(
                style = "margin-bottom: 20px;",
                actionButton("retour_accueil_sim2", "Retour à l'accueil", 
                             icon = icon("arrow-left"),
                             style = "background-color: #31708f; color: white; border: none; padding: 10px 20px; border-radius: 5px;")
              )
            )
          ),
          
          
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
      
      #### Graphique 1 - Comparaison avec consommation par pays----
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
      
      #### Encarts info pour les habitants équivalents pour le Mali, le Qatar et la France
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
      
      #### Graphique 2 - Simulation personnalisée----
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
  
