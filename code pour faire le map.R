# Charger les bibliothèques nécessaires
library(shiny)
library(leaflet)

# Charger le dataset
quali_nitrate <- read.csv("qualite-des-cours-deau-vis-a-vis-des-nitrates-en-bretagne.csv", stringsAsFactors = TRUE)

# Identifier les régions, départements et années
regions <- unique(quali_nitrate$libelle_region)
departements <- split(quali_nitrate$libelle_departement, quali_nitrate$libelle_region)
annees <- unique(quali_nitrate$annee)

# Définir l'interface utilisateur
ui <- fluidPage(
  titlePanel("Carte des Prélèvements par Région, Département et Année"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("region", "Sélectionnez une Région :", 
                     choices = regions, 
                     selected = "Bretagne", 
                     options = list(maxItems = 1)),  # Sélection par défaut
      uiOutput("departementUI"),  # UI pour le choix des départements
      selectizeInput("annee", "Sélectionnez une Année :", 
                     choices = annees, 
                     selected = NULL, 
                     options = list(maxItems = 1, 
                                    placeholder = "Choisissez une année..."))  # Ajout d'un placeholder
    ),
    
    mainPanel(
      leafletOutput("map", height = 600)  # Afficher la carte
    )
  )
)

# Définir la logique du serveur
server <- function(input, output, session) {
  
  # UI dynamique pour les départements
  output$departementUI <- renderUI({
    req(input$region)  # Assurer que la région est sélectionnée
    
    selectizeInput("departement", "Sélectionnez un Département :", 
                   choices = departements[[input$region]], 
                   selected = "Finistère",  # Sélection par défaut
                   options = list(maxItems = 1, 
                                  placeholder = "Choisissez un département..."))  # Ajout d'un placeholder
  })
  
  # Initialisation de la carte
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Ajouter la couche de tuiles
      setView(lng = -3.5, lat = 48.5, zoom = 7)  # Centrer la carte sur la Bretagne
  })
  
  # Observer les changements de sélection et mettre à jour la carte
  observe({
    req(input$departement, input$annee)  # S'assurer qu'un département et une année sont sélectionnés
    
    # Filtrer pour le département et l'année sélectionnés
    data <- subset(quali_nitrate, libelle_departement == input$departement & annee == input$annee)
    
    leafletProxy("map") %>%
      clearMarkers()  # Effacer les anciens marqueurs

    # Vérifier si des données sont disponibles
    if (nrow(data) > 0) {
      leafletProxy("map") %>%  # Utiliser leafletProxy pour ajouter des marqueurs
        addCircleMarkers(
          data = data,  # Utiliser les données filtrées
          lng = ~longitude, 
          lat = ~latitude, 
          color = "blue", 
          fillOpacity = 0.7, 
          radius = 10,
          label = HTML(paste(
            "<div style='font-size: 12px;'>",  # Style optionnel pour le texte
            "<strong>Station:</strong> ", data$libelle_station, "<br>",
            "<strong>Cours d'eau:</strong> ", data$nom_cours_eau, "<br>",
            "<strong>Latitude:</strong> ", data$latitude, "<br>",  # Ajout de la latitude
            "<strong>Longitude:</strong> ", data$longitude, "<br>",  # Ajout de la longitude
            "<strong>Concentration Moyenne:</strong> ", data$concentration_moy, "<br>",
            "<strong>Nombre de Prélèvements:</strong> ", data$nombre_prelevements,
            "</div>"
          )),
          labelOptions = labelOptions(
            noHide = FALSE, 
            direction = 'top', 
            offset = c(0, -10), 
            textOnly = TRUE,
            style = list("color" = "black", "background-color" = "white", "border" = "solid 1px")
          )
        )
    } else {
      leafletProxy("map") %>%
        clearMarkers() %>%
        addMarkers(lng = -2.5, lat = 48.5, popup = "Aucun prélèvement disponible pour ce département et cette année.")
    }
  })
}

# Exécuter l'application Shiny
shinyApp(ui = ui, server = server)
