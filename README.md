# Tableau de bord interactif – Matérialités du numérique, Data Centers

Ce tableau de bord Shiny développé avec `bs4Dash` permet d’explorer l’univers des **data centers**, des **énergies** et des **ressources stratégiques**, à travers des cartes interactives, des graphiques dynamiques, et des analyses spatiales sur plusieurs échelles (Europe, France, monde).

## Structure de l’application

### 0.0 Home Tab

### 1. Extraction et Production
#### 1.1 Extraction des métaux
- 1.1.1 Carte de l'extraction des métaux
- 1.1.2 Diagramme Sankey
#### 1.2 Production
- 1.2.1 Carte de production des semi-conducteurs
- 1.2.2 Infographie de la consommation d'eau
- 1.2.3 Top 5 des pays producteurs de semi-conducteurs

### 2. Répartition des data centres
#### 2.1 Data centers en Europe
- 2.1.1 Carte de répartition des DC en Europe
- 2.1.2 Graphique en barres de la répartition des DC en Europe
- 2.1.3 Chiffres clés
- 2.1.4 Graphique de l'évolution de la demande énergétique des DC
#### 2.2 Data centres dans les FLAP-D
- 2.2.1 Carte de la répartition des DC dans les FLAP-D
#### 2.3 Data centres en France

### 3. Bilan énergétique
#### 3.1 Énergie en France
- 3.1.1 Carte de production et consommation énergétique par région avec des cercles proportionnels
- 3.1.2 Camembert de répartition de la production énergétique par filière
- 3.1.3 Évolution de la production et consommation énergétique par filière
- 3.1.4 Carte de typologie des régions françaises en fonction du bilan énergétique
- 3.1.5 Graphique en radar de Production et Consommation énergétique par région
#### 3.2 Énergie en Auvergne-Rhône-Alpes
- 3.2.1 Carte de la consommation énergétique des EPCI
- 3.2.2 Carte de la consommation énergétique des EPCI par habitant
- 3.2.3 Installation énergétique dans la région

### 4. Simulations
#### 4.1 Simulation 1 - Analyse prédictive
- 4.1.1 Boîte d'explication
- 4.1.2 Paramètres de simulation
- 4.1.3 Données de référence
- 4.1.4 Graphique principal
#### 4.2 Simulation 2 - Analyse comparative
- 4.2.1 Boite d'explication
- 4.2.2 Graphique 1 - Comparaison avec consommation par pays
- 4.2.3 Encarts info pour les habitants équivalents pour le Mali, le Qatar et la France
- 4.2.4 Graphique 2 - Simulation personnalisée

## Arborescence des fichiers

```
dashboard/
├── app.R
├── global.R           # Chargement des packages et données
├── ui.R               # Interface utilisateur
├── server.R           # Logique serveur
├── data/              # Tous les fichiers .rds
│   ├── dc_europe.rds
│   ├── europe_map.rds
│   ├── data_DC_FLAPD.rds
│   ├── data_ara_epci.rds
│   ├── eol_aura.rds, nuc_aura.rds, hydro_aura.rds, sol_aura.rds
│   └── extraction_simplifie.rds
└── www/
    ├── custom.css     # styles CSS personnalisés
    └── user_guide/    # guides d'utilisation
```

## Lancement de l’application

### Prérequis

Installer les packages nécessaires (automatisé dans `global.R`) :
```r
core_pkgs <- c("shiny", "shinyjs", "shinyWidgets", "bs4Dash", "bslib", "shinydashboard", "fresh")
geo_pkgs  <- c("sf", "leaflet")
viz_pkgs  <- c("ggplot2", "plotly", "networkD3")
data_pkgs <- c("readr", "here", "dplyr", "tidyr", "scales")
utils_pkgs <- c("DT", "htmlwidgets", "shinycssloaders")

required_packages <- c(core_pkgs, geo_pkgs, viz_pkgs, data_pkgs, utils_pkgs)
```

### Lancer l'app

Dans R ou RStudio :
```r
shiny::runApp("dashboard")
```

## Technologies utilisées

| Librairie       | Rôle                                 |
|-----------------|--------------------------------------|
| `bs4Dash`       | Mise en page moderne type "admin"    |
| `leaflet`       | Cartes interactives                  |
| `plotly`        | Graphiques interactifs               |
| `sf`            | Données spatiales                    |
| `shinyWidgets`  | Contrôles enrichis (sliders, switches) |
| `dplyr` / `tidyr` | Traitement de données             |
| `DT`            | Tableaux interactifs                 |
| `shinycssloaders` | Loaders pendant le calcul          |
| `shinyjs`       | Fonctions JS utiles (masquer, désactiver…) |
| `fresh`         | Thème personnalisé Bootstrap         |

## Données utilisées

Les données sont stockées localement sous forme `.rds` :
- Data centers européens et FLAP-D (données synthétiques ou retraitées)
- Géométries spatiales simplifiées (`sf`)
- Données énergétiques régionales
- Ressources mondiales (terres rares, semi-conducteurs)

**Aucune donnée personnelle n’est traitée ou exposée.**

## Contacts

- **Auteurs** : Robert Lim et Zoé Cargnelli 
- **Encadrants** : Aurélie Zara et Mario Cortes-Cornax
- **Contacts** : robert.lim@etu.univ-grenoble-alpes.fr / zoe.cargnelli@etu.univ-grenoble-alpes.fr

**Stage M1 Géomatique et Analyse Spatiale** – Projet VerIT, Université Grenoble Alpes, Grenoble INP, CMQ IED
