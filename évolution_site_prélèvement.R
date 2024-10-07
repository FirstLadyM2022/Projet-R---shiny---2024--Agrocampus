# app.R

library(shiny)
library(ggplot2)
library(dplyr)

# Charger les données
data <- read.csv("qualite-des-cours-deau-vis-a-vis-des-nitrates-en-bretagne.csv")

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Variation des Stations de Prélèvement au Fil des Ans"),
  sidebarLayout(
    sidebarPanel(
      helpText("Affichage du nombre de stations de prélèvement par année")
    ),
    mainPanel(
      plotOutput("stationPlot")
    )
  )
)

# Serveur de l'application
server <- function(input, output) {
  output$stationPlot <- renderPlot({
    
    # Préparation des données
    stations_par_annee <- data %>%
      group_by(annee) %>%
      summarise(nb_stations = n_distinct(code_station))
    
    # Création du graphique
    ggplot(stations_par_annee, aes(x = annee, y = nb_stations)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(
        title = "Variation du Nombre de Stations de Prélèvement au Fil des Ans",
        x = "Année",
        y = "Nombre de Stations Uniques"
      ) +
      theme_minimal()
  })
}

# Exécuter l'application Shiny
shinyApp(ui = ui, server = server)
