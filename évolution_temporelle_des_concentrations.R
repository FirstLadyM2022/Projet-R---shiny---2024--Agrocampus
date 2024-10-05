# Charger les bibliothèques nécessaires
library(shiny)
library(ggplot2)
library(dplyr)

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Évolution des Concentrations de Nitrates dans les Cours d'Eau par Région"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Sélectionnez une région", 
                  choices = c("Bretagne", "Normandie", "Pays de la Loire"),
                  selected = "Bretagne")
    ),
    
    mainPanel(
      plotOutput("nitratePlot")
    )
  )
)

# Serveur
server <- function(input, output) {
  
  # Charger et filtrer les données en fonction de la région sélectionnée
  data <- reactive({
    # Charger le fichier CSV directement depuis un chemin fixe
    df <- read.csv("qualite-des-cours-deau-vis-a-vis-des-nitrates-en-bretagne.csv")
    
    # Vérification des colonnes et renommer si nécessaire
    df <- df %>%
      rename(
        annee = annee, 
        libelle_region = libelle_region,  # Utiliser le nom de la colonne correcte
        concentration_moy = concentration_moy,
        concentration_min = concentration_min, 
        concentration_max = concentration_max
      )
    
    # Filtrer les données selon la région sélectionnée
    df_filtered <- df %>%
      filter(libelle_region == input$region) %>%
      group_by(annee) %>%
      summarise(
        concentration_moyenne = mean(concentration_moy, na.rm = TRUE),
        concentration_minimale = mean(concentration_min, na.rm = TRUE),
        concentration_maximale = mean(concentration_max, na.rm = TRUE)
      )
    
    return(df_filtered)
  })
  
  # Créer le graphique
  output$nitratePlot <- renderPlot({
    req(data())
    ggplot(data(), aes(x = annee)) +
      geom_line(aes(y = concentration_moyenne, color = "Concentration Moyenne")) +
      geom_line(aes(y = concentration_minimale, color = "Concentration Minimale")) +
      geom_line(aes(y = concentration_maximale, color = "Concentration Maximale")) +
      labs(
        title = paste("Évolution des Concentrations de Nitrates en", input$region),
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
