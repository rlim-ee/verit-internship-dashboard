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




# Mode interactif pour tmap
tmap_mode("view")




# USER INTERFACE ----
ui <- bs4DashPage(
  title = "Dashboard",
  fullscreen = TRUE,
  
  header = bs4DashNavbar(title = "Dashboard"),
  
  ## Sidebar ----
  sidebar = bs4DashSidebar(
    skin = "#5FC2BA",
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
                         bs4SidebarMenuSubItem("Auvergne-Rhône-Alpes", tabName = "ara")
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
    /* Global Font */
    body, h1, h2, h3, h4, h5, h6, .small-box, .nav-link, .brand-text {
      font-family: 'Poppins', sans-serif !important;
    }

    /* Header Navbar Brand */
    .main-header .navbar .navbar-brand .brand-text {
      font-weight: bold !important;
      font-size: 22px !important;
      color: #0B162C !important;
    }

    /* Sidebar */
    .main-sidebar {
      background-color: #5FC2BA !important;
    }

    .main-sidebar .nav-link,
    .main-sidebar .brand-link {
      color: #0B162C !important;
    }

    .main-sidebar .nav-link.active {
      color: #5FC2BA !important;
      background-color: #0B162C !important;
    }

    .main-sidebar .nav-link:hover {
      background-color: #0B162C !important;
      color: #5FC2BA !important;
    }

    /* Content Backgrounds */
    .content-wrapper,
    .main-footer,
    .main-header {
      background-color: white !important;
    }

    /* Card Titles */
    .card-header {
      background-color: #0B162C !important;
      color: #5FC2BA !important;
    }

    .card-title {
      font-size: 20px !important;
      font-weight: bold !important;
      color: white !important;
    }

    /* Section Headers */
    .section-header {
      color: #0B162C !important;
      font-weight: bold !important;
      font-size: 1.4rem !important;
      border-left: 6px solid #0B162C !important;
      background: linear-gradient(90deg, #5FC2BA 0%, rgba(95, 194, 186, 0.2) 100%) !important;
      padding: 10px 15px !important;
      margin: 30px 0 15px 0 !important;
      border-radius: 6px !important;
    }

    /* Custom Value Boxes */
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

    /* Value Box Text */
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

    /* Value Box Link */
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

    /* Hover animation for small-box */
    .small-box {
      transition: transform 0.3s ease, box-shadow 0.3s ease !important;
    }

    .small-box:hover {
      transform: translateY(-5px) !important;
      box-shadow: 0 6px 20px rgba(0, 0, 0, 0.2) !important;
    }

    /* Responsive for small screens */
    @media (max-width: 768px) {
      .small-box h3 {
        font-size: 1.4rem !important;
      }
      .small-box p {
        font-size: 1rem !important;
      }
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
        
        
        h4("Explorer le dashboard :", style = "margin-top: 20px; margin-bottom: 10px; font-weight: bold; color: #0B162C"),
        
        # Navigation Value Boxes - DC & Europe Section
        h5("DC & Europe", class = "section-header"),
        fluidRow(
          bs4ValueBox(
            value = actionLink("go_dc_europe", "Cliquez pour voir"),
            subtitle = "DC en Europe",
            icon = icon("earth-europe"),
            color = "teal",
            width = 4
          ),
          
          bs4ValueBox(
            value = actionLink("go_flapd", "Cliquez pour voir"),
            subtitle = "FLAP-D",
            icon = icon("network-wired"),
            color = "gray",
            width = 4
          ),
          
          bs4ValueBox(
            value = actionLink("go_dc_france", "Cliquez pour voir"),
            subtitle = "DC en France",
            icon = icon("flag"),
            color = "orange",
            width = 4
          )
        ),
        
        # Navigation Value Boxes - Energy Balance Section
        h5("Bilan énergétique", class = "section-header"),
        fluidRow(
          bs4ValueBox(
            value = actionLink("go_regions", "Cliquez pour voir"),
            subtitle = "Analyse nationale",
            icon = icon("lightbulb"),
            color = "success",
            width = 6
          ),
          
          bs4ValueBox(
            value = actionLink("go_ara", "Cliquez pour voir"),
            subtitle = "Analyse régionale",
            icon = icon("mountain"),
            color = "purple",
            width = 6
          )
        ),
        
        # Credits
        tags$div(
          style = "margin-top: 30px; text-align: right; font-size: 0.9em; color: #888;",
          "Auteur : Zoé Cargnelli & Robert Lim | Source : ICIS, Eurostat, DataCenterMap, RTE France | 2025"
        )
      ),
      
      ### Tab 1.1: European Data Centers ----
      tabItem(
        tabName = "dc_europe_map",
        
        fluidRow(
          column(
            width = 12,
            actionButton("retour_accueil_dc_europe", "Retour à l'accueil", icon = icon("arrow-left")),
            br(), br()
          )
        ),
        
        # Maps Row
        fluidRow(
          bs4Card(
            title = "Répartition des DC en Europe",
            width = 6,
            status = "teal",
            solidHeader = TRUE,
            tmapOutput("map1", height = "450px")
          ),
          bs4Card(
            title = "Part du nombre des DC en Europe",
            width = 6,
            status = "teal",
            solidHeader = TRUE,
            plotlyOutput("barPlot", height = "450px")
          )
        ),
        
        # Key Metrics Row
        fluidRow(
          bs4ValueBox(
            value = "236 TWh",
            subtitle = "Demande estimée en 2035",
            icon = icon("bolt"),
            color = "warning",
            width = 4
          ),
          bs4ValueBox(
            value = "+146%",
            subtitle = "Hausse depuis 2024",
            icon = icon("chart-line"),
            color = "danger",
            width = 4
          ),
          bs4ValueBox(
            value = "79%",
            subtitle = "Concentration sur 10 pays",
            icon = icon("globe-europe"),
            color = "info",
            width = 4
          )
        ),
        
        # Energy Demand Evolution
        fluidRow(
          bs4Card(
            title = "Évolution de la demande énergétique",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            icon = icon("bolt"),
            plotOutput("dc_demand_plot", height = "390px"),
            
            tags$p(
              "Selon ICIS, la demande énergétique des data centres en Europe passera de 96 TWh en 2024 à 236 TWh en 2035, représentant alors 5,7 % de la demande totale d'électricité.",
              style = "margin-top: 15px; font-size: 15px;"
            ),
            
            footer = "Source : ICIS, Projection 2024-2035"
          )
        )
      ),
      
      ### Tab 1.2: FLAP-D ----
      tabItem(
        tabName = "flapd",
        
        fluidRow(
          column(
            width = 12,
            actionButton("retour_accueil_flapd", "Retour à l'accueil", icon = icon("arrow-left")),
            br(), br()
          )
        ),
        
        
        h2("FLAP-D"),
        p("Contenu à venir.")
      ),
      
      ### Tab 1.3: Data Centers in France ----
      tabItem(
        tabName = "dc_france",
        
        fluidRow(
          column(
            width = 12,
            actionButton("retour_accueil_dc_france", "Retour à l'accueil", icon = icon("arrow-left")),
            br(), br()
          )
        ),
        
        
        h2("DC en France"),
        p("Contenu à venir.")
      ),
      
      ### Tab 2.1: France Energy Analysis ----
      tabItem(
        tabName = "regions",
        
        fluidRow(
          column(
            width = 12,
            actionButton("retour_accueil_regions", "Retour à l'accueil", icon = icon("arrow-left")),
            br(), br()
          )
        ),
        
        fluidRow(
          
          # 1ère carte avec menu déroulant
          bs4Card(
            title = "Carte interactive : Consommation ou Production totale",
            width = 6,
            status = "teal",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            selectInput(
              inputId = "choix_map",
              label = "Choisir l’indicateur à afficher :",
              choices = c("Consommation totale brute" = "conso", 
                          "Production totale" = "prod"),
              selected = "prod"
            ),
            
            tmapOutput("map_totale", height = 530)
          ),
          
          # 2ème carte : camembert de production par filière
          bs4Card(
            title = "Production d'énergie par filière",
            width = 6,
            status = "teal",
            solidHeader = TRUE,
            
            selectInput("region_select", "Choisir une région :", 
                        choices = c("France", regions$NOM), selected = "France"),
            
            plotlyOutput("pie_chart", height = 530)
          )
        ),
        
        # Time Series Row
        fluidRow(
          bs4Card(
            title = "Évolution de la production par filière",
            width = 12,
            status = "teal",
            solidHeader = TRUE,
            plotlyOutput("area_chart", height = 300)
          )
        ),
        
        # Analysis Row
        fluidRow(
          bs4Card(
            title = "Qui alimente la France ?",
            width = 6,
            status = "teal",
            solidHeader = TRUE,
            tmapOutput("map6", height = 450)
          ),
          bs4Card(
            title = "Production vs Consommation par région",
            width = 6,
            status = "teal",
            solidHeader = TRUE,
            plotlyOutput("radar_chart", height = 450)
          )
        )
      ),
      
      ### Tab 2.2: Auvergne-Rhône-Alpes ----
      tabItem(
        tabName = "ara",
        
        fluidRow(
          column(
            width = 12,
            actionButton("retour_accueil_ara", "Retour à l'accueil", icon = icon("arrow-left")),
            br(), br()
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Consommation totale",
            width = 6,
            status = "teal",
            solidHeader = TRUE,
            tmapOutput("map4", height = 530)
          ),
          bs4Card(
            title = "Consommation par habitant",
            width = 6,
            status = "teal",
            solidHeader = TRUE,
            tmapOutput("map5", height = 530)
          )
        )
      )
    )
  )
)