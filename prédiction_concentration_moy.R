# Charger les bibliothèques nécessaires
library(shiny)
library(forecast)
library(dplyr)
library(ggplot2)

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Prévision de la Concentration Moyenne de Nitrates"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("years", "Nombre d'années à prévoir", value = 5, min = 1, max = 20)
    ),
    
    mainPanel(
      plotOutput("forecastPlot"),
      verbatimTextOutput("modelSummary")
    )
  )
)

# Serveur
server <- function(input, output) {
  
  # Charger et préparer les données
  data <- reactive({
    df <- read.csv("qualite-des-cours-deau-vis-a-vis-des-nitrates-en-bretagne.csv")
    
    # Agrégation des données par année pour obtenir la concentration moyenne annuelle
    df %>%
      group_by(annee) %>%
      summarise(concentration_moyenne = mean(concentration_moy, na.rm = TRUE))
  })
  
  # Ajuster le modèle ARIMA et faire la prévision
  forecast_data <- reactive({
    req(data())
    ts_data <- ts(data()$concentration_moyenne, start = min(data()$annee), frequency = 1)
    
    # Ajuster le modèle ARIMA
    model <- auto.arima(ts_data)
    
    # Faire une prévision pour le nombre d'années sélectionné
    forecast(model, h = input$years)
  })
  
  # Résumé du modèle
  output$modelSummary <- renderPrint({
    req(forecast_data())
    summary(forecast_data()$model)
  })
  
  # Visualiser la prévision
  output$forecastPlot <- renderPlot({
    req(forecast_data())
    forecast_df <- forecast_data()
    
    autoplot(forecast_df) +
      ggtitle("Prévision de la Concentration Moyenne de Nitrates") +
      xlab("Année") +
      ylab("Concentration Moyenne de Nitrates (mg/L)") +
      theme_minimal()
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
