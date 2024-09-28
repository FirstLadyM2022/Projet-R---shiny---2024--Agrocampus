# install.packages("shiny")
# install.packages("leaflet")


# Load required libraries
library(shiny)
library(leaflet)


# Dataset
quali_nitrate <- read.csv("/Users/massambadiop/Documents/INSTITUT AGRO/M2 Sciences des données/Analyse de données massives sur R/Projet Shiny/Projet-R---shiny---2024--Agrocampus-main/qualite-des-cours-deau-vis-a-vis-des-nitrates-en-bretagne.csv",stringsAsFactors  = T)
df_filtre <- subset(quali_nitrate, nom_sage == "Couesnon") # Sélection d'une localité (Couesnon)


# Define UI
ui <- fluidPage(
  tags$h1("ShiNitrate Project", 
          style = "color: #FF5733; text-align: center; font-weight: bold; 
                     font-size: 32px; margin-top: 20px;"),
  
  tags$h3("Get insights about nitrates!", 
          style = "color: #555; text-align: center; font-style: italic; 
                     margin-top: -10px; margin-bottom: 30px;"),
  
 
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      h3("Stations de Couesnon"),
      p("Cette carte est interactive. Vous pouvez faire un zoom avant/arrière, faire glisser la carte et cliquer sur les marqueurs.")
    ),
    
    # Main panel for displaying the map
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Create the Leaflet map
  output$map <- renderLeaflet({
    leaflet(df_filtre) %>%
      addTiles() %>%  # Ajouter la couche de tuiles
      addCircles(lng = ~longitude, lat = ~latitude, color = "blue", fillOpacity = 0.7, radius = 200) %>% 
      addPopups(lng = ~longitude, lat = ~latitude, popup = ~nom_cours_eau)
  })
}

# Run the Shiny app
shinyApp(ui, server)
