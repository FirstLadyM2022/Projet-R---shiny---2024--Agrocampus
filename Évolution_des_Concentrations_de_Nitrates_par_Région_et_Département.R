# Charger les bibliothèques nécessaires
library(shiny)
library(ggplot2)
library(dplyr)

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Évolution des Concentrations de Nitrates par Région et Département"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Sélectionnez une région", 
                  choices = c("Bretagne", "Normandie", "Pays de la Loire"),
                  selected = "Bretagne"),
      selectInput("departement", "Sélectionnez un département", 
                  choices = NULL)  # Les choix seront mis à jour dynamiquement
    ),
    
    mainPanel(
      plotOutput("nitratePlot")
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Charger les données une fois à l'initialisation
  df <- reactive({
    read.csv("qualite-des-cours-deau-vis-a-vis-des-nitrates-en-bretagne.csv")
  })
  
  # Mettre à jour les départements en fonction de la région sélectionnée
  observe({
    req(input$region)
    
    # Filtrer les départements selon la région sélectionnée
    departements <- df() %>%
      filter(libelle_region == input$region) %>%
      pull(libelle_departement) %>%  
      unique()  # Garder seulement les départements uniques
    
    updateSelectInput(session, "departement", choices = departements)
  })
  
  # Charger et filtrer les données en fonction de la région et du département sélectionnés
  data <- reactive({
    req(input$departement)  # S'assurer qu'un département est sélectionné
    
    df() %>%
      filter(libelle_region == input$region, libelle_departement == input$departement) %>%
      group_by(annee) %>%
      summarise(
        concentration_moyenne = mean(concentration_moy, na.rm = TRUE),
        concentration_minimale = mean(concentration_min, na.rm = TRUE),
        concentration_maximale = mean(concentration_max, na.rm = TRUE)
      )
  })
  
  # Créer le graphique
  output$nitratePlot <- renderPlot({
    req(data())
    ggplot(data(), aes(x = annee)) +
      geom_line(aes(y = concentration_moyenne, color = "Concentration Moyenne")) +
      geom_line(aes(y = concentration_minimale, color = "Concentration Minimale")) +
      geom_line(aes(y = concentration_maximale, color = "Concentration Maximale")) +
      labs(
        title = paste("Évolution des Concentrations de Nitrates en", input$departement, "(", input$region, ")"),
        x = "Année",
        y = "Concentration de Nitrates (mg/L)"
      ) +
      scale_color_manual(
        values = c("Concentration Moyenne" = "blue", 
                   "Concentration Minimale" = "green", 
                   "Concentration Maximale" = "red"),
        name = "Type de Concentration"
      ) +
      theme_minimal()
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
