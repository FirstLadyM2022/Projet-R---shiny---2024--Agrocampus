# Charger les bibliothèques nécessaires
library(shiny)
library(leaflet)

# Charger le dataset
data <- read.csv("qualite-des-cours-deau-vis-a-vis-des-nitrates-en-bretagne.csv", stringsAsFactors = TRUE)

# Identifier les régions, départements et années
regions <- unique(data$libelle_region)
departements <- split(data$libelle_departement, data$libelle_region)
annees <- sort(unique(data$annee))  # Trier les années pour le curseur

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
      sliderInput("annee", "Sélectionnez une Année :", 
                  min = min(annees), 
                  max = max(annees), 
                  value = max(annees),  # Valeur par défaut : année maximale
                  step = 1,
                  ticks = FALSE),  # Ne pas afficher les ticks
      radioButtons("metrique", "Choisissez la Métrique :", 
                   choices = c("Concentration Moyenne" = "concentration_moy", 
                               "Q90" = "valeur_q90"),  # Options disponibles avec la bonne variable
                   selected = "concentration_moy", 
                   inline = TRUE)  # Afficher les boutons radio en ligne
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
    data <- subset(data, libelle_departement == input$departement & annee == input$annee)
    
    leafletProxy("map") %>%
      clearMarkers() %>%  # Effacer les anciens marqueurs
      clearControls()  # Effacer la légende précédente
    
    # Vérifier si des données sont disponibles
    if (nrow(data) > 0) {
      # Créer une palette de couleurs basée sur la métrique choisie
      pal <- colorNumeric(palette = "YlOrRd", domain = data[[input$metrique]])
      
      leafletProxy("map") %>%  # Utiliser leafletProxy pour ajouter des marqueurs
        addCircleMarkers(
          data = data,  # Utiliser les données filtrées
          lng = ~longitude, 
          lat = ~latitude, 
          color = pal(data[[input$metrique]]),  # Appliquer la couleur selon la métrique choisie
          fillOpacity = 0.7, 
          radius = 10,
          label = paste(
            "Station:", data$libelle_station, "\n",
            "Concentration Moyenne:", data$concentration_moy, "mg/L", "\n",
            "Q90:", data$valeur_q90, "mg/L", "\n",  # Utiliser valeur_q90 pour le label
            "Nombre de Prélèvements:", data$nombre_prelevements
          ),
          labelOptions = labelOptions(
            noHide = FALSE,  # Afficher le label uniquement au survol
            direction = 'top', 
            offset = c(0, -10), 
            textOnly = TRUE,  # Utilisation de texte brut
            style = list("color" = "black", "background-color" = "white", "border" = "solid 1px")  # Style des labels
          ),
          popup = paste(
            "Station:", data$libelle_station, "<br>",
            "Concentration Moyenne:", data$concentration_moy, "mg/L", "<br>",
            "Q90:", data$valeur_q90, "mg/L", "<br>",
            "Nombre de Prélèvements:", data$nombre_prelevements
          )
        )
      
      # Ajouter une légende pour la palette de couleurs
      leafletProxy("map") %>%
        addLegend("bottomright", 
                  pal = pal, 
                  values = data[[input$metrique]],  # Utiliser la métrique choisie pour la légende
                  title = input$metrique,  # Titre dynamique basé sur la métrique choisie
                  opacity = 0.7)
    } else {
      leafletProxy("map") %>%
        clearMarkers() %>%
        addMarkers(lng = -2.5, lat = 48.5, popup = "Aucun prélèvement disponible pour ce département et cette année.")
    }
  })
}

# Exécuter l'application Shiny
shinyApp(ui = ui, server = server)
