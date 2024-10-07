# app.R

library(shiny)
library(ggplot2)
library(dplyr)

# Charger les données
data <- read.csv("qualite-des-cours-deau-vis-a-vis-des-nitrates-en-bretagne.csv")

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Suivi de l'Évolution des Stations de Prélèvement par Région et Département"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Choisissez une région :", choices = unique(data$libelle_region)),
      uiOutput("departement_ui"),
      helpText("Affichage du nombre de stations de prélèvement par année pour la région et le département sélectionnés.")
    ),
    mainPanel(
      plotOutput("stationPlot")
    )
  )
)

# Serveur de l'application
server <- function(input, output, session) {
  
  # Met à jour la liste des départements en fonction de la région sélectionnée
  output$departement_ui <- renderUI({
    req(input$region) # Vérifie que la région est sélectionnée
    departements <- data %>%
      filter(libelle_region == input$region) %>%
      pull(libelle_departement) %>%
      unique() %>%
      sort()
    selectInput("departement", "Choisissez un département :", choices = departements)
  })
  
  # Créer le graphique en fonction de la région et du département sélectionnés
  output$stationPlot <- renderPlot({
    req(input$region, input$departement) # Vérifie que la région et le département sont sélectionnés
    
    # Filtrer les données pour la région et le département sélectionnés
    data_filtered <- data %>%
      filter(libelle_region == input$region, libelle_departement == input$departement) %>%
      group_by(annee) %>%
      summarise(nb_stations = n_distinct(code_station))
    
    # Création du graphique
    ggplot(data_filtered, aes(x = annee, y = nb_stations)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(
        title = paste("Évolution des Stations de Prélèvement pour:", input$region, "-", input$departement),
        x = "Année",
        y = "Nombre de Stations Uniques"
      ) +
      theme_minimal()
  })
}

# Exécuter l'application Shiny
shinyApp(ui = ui, server = server)
