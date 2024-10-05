# Charger les bibliothèques nécessaires
library(shiny)
library(ggplot2)
library(dplyr)

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Comparaison des Évolutions des Concentrations de Nitrates entre Régions"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("regions", "Sélectionnez les régions", 
                  choices = c("Bretagne", "Normandie", "Pays de la Loire"),
                  selected = "Bretagne",
                  multiple = TRUE)  # Permet la sélection multiple
    ),
    
    mainPanel(
      plotOutput("nitratePlot")
    )
  )
)

# Serveur
server <- function(input, output) {
  
  # Charger les données une fois à l'initialisation
  df <- reactive({
    read.csv("qualite-des-cours-deau-vis-a-vis-des-nitrates-en-bretagne.csv")
  })
  
  # Charger et filtrer les données en fonction des régions sélectionnées
  data <- reactive({
    req(input$regions)  # S'assurer qu'au moins une région est sélectionnée
    
    df() %>%
      filter(libelle_region %in% input$regions) %>%
      group_by(annee, libelle_region) %>%
      summarise(
        concentration_moyenne = mean(concentration_moy, na.rm = TRUE),
        concentration_minimale = mean(concentration_min, na.rm = TRUE),
        concentration_maximale = mean(concentration_max, na.rm = TRUE)
      )
  })
  
  # Créer le graphique
  output$nitratePlot <- renderPlot({
    req(data())
    ggplot(data(), aes(x = annee, color = libelle_region)) +
      geom_line(aes(y = concentration_moyenne, linetype = "Concentration Moyenne")) +
      geom_line(aes(y = concentration_minimale, linetype = "Concentration Minimale")) +
      geom_line(aes(y = concentration_maximale, linetype = "Concentration Maximale")) +
      labs(
        title = "Comparaison des Concentrations de Nitrates",
        x = "Année",
        y = "Concentration de Nitrates (mg/L)",
        color = "Région",
        linetype = "Type de Concentration"
      ) +
      scale_color_manual(values = c("Bretagne" = "blue", 
                                    "Normandie" = "orange", 
                                    "Pays de la Loire" = "red")) +
      theme_minimal()
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
