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

# Charger les donnnées
dc_europe <- read_csv(here(
  "dashboard",
  "data", "dc_europe.csv"))
europe_map <- st_read(here(
  "dashboard",
  "data", "europe_map", "europe_map.shp"))
regions <- st_read(here(
  "dashboard",
  "data","region_energie", "DONNEES", "region_energie.shp"))
data_prod <- read_csv2(here(
  "dashboard",
  "data", "conso_evo.csv"))
data_ara <- st_read(here(
  "dashboard",
  "data", "data_ara_epci", "DONNEES", "data_ara.shp"))




# USER INTERFACE ----
ui <- bs4DashPage(
  title = "Dashboard",
  fullscreen = TRUE,
  
  header = bs4DashNavbar(title = "Dashboard"),
  
  ## Sidebar ----
  sidebar = bs4DashSidebar(
    skin = "#31708f",
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
      )
    )
  ),
  
  ## Main Body ----
  body = bs4DashBody(
    
    # Custom CSS Styling
    # Custom CSS Styling avec Google Fonts et animations
    tags$head(
      # Google Fonts : Poppins
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Poppins&display=swap",
        rel = "stylesheet"
      ),
      
      # CSS personnalisé
      tags$style(HTML("
  /* Police globale */
  body, h1, h2, h3, h4, h5, h6, .small-box, .nav-link, .brand-text {
    font-family: 'Poppins', sans-serif !important;
  }

  /* En-tête - Marque de la barre de navigation */
  .main-header .navbar .navbar-brand .brand-text {
    font-weight: bold !important;
    font-size: 22px !important;
    color: #f5f6f7 !important;
  }

  /* Barre latérale */
  .main-sidebar {
    background-color: #31708f !important;
  }

  .main-sidebar .nav-link,
  .main-sidebar .brand-link {
    color: #f5f6f7 !important;
  }

  .main-sidebar .nav-link.active {
    color: #5FC2BA !important;
    background-color: #0B162C !important;
  }

  .main-sidebar .nav-link:hover {
    background-color: #0B162C !important;
    color: #5FC2BA !important;
  }

  /* Fond des contenus */
  .content-wrapper,
  .main-footer,
  .main-header {
    background-color: white !important;
  }

  /* En-têtes des cartes */
  .card-header {
    background-color: #0B162C !important;
    color: #5FC2BA !important;
  }

  .card-title {
    font-size: 20px !important;
    font-weight: bold !important;
    color: white !important;
  }

  /* Titres de section */
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


  /* Value boxes personnalisées */
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

  /* Texte des value boxes */
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

  /* Lien dans les value boxes */
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

  /* Animation au survol des small-box */
  .small-box {
    transition: transform 0.3s ease, box-shadow 0.3s ease !important;
  }

  .small-box:hover {
    transform: translateY(-5px) !important;
    box-shadow: 0 6px 20px rgba(0, 0, 0, 0.2) !important;
  }

  /* Responsive pour les petits écrans */
  @media (max-width: 768px) {
    .small-box h3 {
      font-size: 1.4rem !important;
    }
    .small-box p {
      font-size: 1rem !important;
    }
  }

  /* Carte moderne d'énergie */
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
    font-size: 0.75rem;       /* un peu plus petit */
    color: #64748b;           /* un gris discret */
    margin-top: 4px;
    text-align: right;        /* aligné à droite comme la valeur principale */
    white-space: nowrap;      /* évite les retours à la ligne */
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

  /* Informations supplémentaires */
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

  /* Animation d’apparition */
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

  /* Correctifs de mise en page */
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
      tabItem(
        tabName = "home",
        
        # Project Description
        bs4Card(
          title = "Contexte",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          width = 12,
          tags$p("Les centres de données, infrastructures clés du numérique, connaissent une forte croissance en Europe avec une capacité énergétique estimée à 9,2 GW pouvant atteindre 26,6 GW d’ici 2035. Cette expansion soulève des préoccupations environnementales majeures, notamment l’accaparement de l’énergie renouvelable au détriment d’autres usages. La France, parmi les pays les plus dotés en data centers, mise sur son autonomie énergétique, en particulier grâce à la région Auvergne-Rhône-Alpes, grande productrice d’hydroélectricité."),
          tags$p("Dans ce contexte, la société française DataOne prévoit la construction de deux data centers surpuissants en Isère. Le site de Eybens devrait atteindre 1 GW de puissance d’ici 2035, avec une consommation exclusivement hydraulique selon les ambitions affichées. Ce projet illustre les enjeux liés au développement de l’intelligence artificielle, très énergivore, et met en lumière la nécessité d’un équilibre entre innovation technologique et durabilité énergétique."),
          tags$blockquote(
            "Les centres de données pourraient représenter 5,7 % de la demande totale d'électricité en Europe d'ici 2035.",
            style = "font-style: italic; color: #555;"
          )
        ),
        
        
        h3("Explorer le dashboard :"),
        
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
          
        
        # Credits
        tags$div(
          style = "margin-top: 30px; text-align: right; font-size: 0.9em; color: #888;",
          "Auteur : Zoé Cargnelli & Robert Lim | Source : ICIS, Eurostat, DataCenterMap, RTE France | 2025"
        )
      ),
    
    ### 1.1 Data centres en Europe----
    
    tabItem(
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
            
            h3(icon("trending-up"), "Croissance 2024-2035", style = "color: #31708f; margin-bottom: 15px; font-size: 18px;"),
            
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
    
    
    
    ### 1.2 Data centres danns les FLAP-D----
    
    
    
    
    ### 1.3 Data centres en France----
    
    
    
    
    ### 2.1 Énergie en France----
    tabItem(
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
      
      # Première ligne : carte + camembert avec style uniforme
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
    )
    
    
    
    
    
    
    ### 2.2 Énergie en Auvergne-Rhone-Alpes
    
  
    )
  )
)
      
    
  
