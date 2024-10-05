#

# Load required libraries
library(shiny)
library(leaflet)


# Charger les bibliothèques nécessaires
library(shiny)
library(leaflet)

# Charger le dataset
data <- read.csv("qualite-des-cours-deau-vis-a-vis-des-nitrates-en-bretagne.csv", stringsAsFactors = TRUE)

# Identifier les régions, départements et années
regions <- unique(data$libelle_region)
departements <- split(data$libelle_departement, data$libelle_region)
annees <- sort(unique(data$annee))  # Trier les années pour le curseur




# Load necessary libraries
library(shiny)
library(ggfortify)  # For autoplot
library(plotly)     # For ggplotly

# Define the UI
ui <- fluidPage(
  titlePanel("ShinyTrate Project"), 
  navbarPage("Interactive Time Series App",
                 
                 
                 # Page 1
             tabPanel("Evolutif de la concentration en nitrates",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("column1", "Select Column for Page 1:", choices = NULL)
                        ),
                        mainPanel(
                          plotlyOutput("plot1"),
                          hr(),  # A horizontal line for separation
                          plotlyOutput("nitratePlot_dep")  # Adding the plot from Page 4 here
                        )
                      )
             ),
                 
                 # Page 3
                 tabPanel("Evolution de la concentration en nitrates dans le temps",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("department", 
                                          "Select Departement:", 
                                          choices = unique(nitrate_trend_dept$libelle_departement), 
                                          selected = unique(nitrate_trend_dept$libelle_departement)[1])
                            ),
                            
                            mainPanel(
                              plotOutput("nitratePlot")
                            )
                          )
                 ),
                 
                
             
                 )
)


# Define the server logic
server <- function(input, output, session) {
  
  data <- read.csv("/Users/massambadiop/Documents/INSTITUT AGRO/M2 Sciences des données/Analyse de données massives sur R/Projet-R---shiny---2024--Agrocampus-1/qualite-des-cours-deau-vis-a-vis-des-nitrates-en-bretagne.csv",stringsAsFactors  = T, header = T)
  data.ts <- ts(data, start = 1995, end = 2023, frequency = 1)
  
  # Update choices for column selection in all pages
  observe({
    updateSelectInput(session, "column1", choices = colnames(data[,20:22]))

  })
  
  
  
  # Render Plot for Page 1
  output$plot1 <- renderPlotly({
    req(input$column1)
    plot <- autoplot(data.ts[, input$column1], xlab = "Année", ylab = input$column1, main = paste("Evolution", input$column1))
    ggplotly(plot)
  })
  # Render Plot for Page 1
  output$nitratePlot_dep <- renderPlotly({
    ggplotly(ggplot(nitrate_trend_dept, aes(x = annee, y = mean_concentration, color = libelle_departement)) +
      geom_line() + 
      geom_point(size=0.7) +
      labs(title = "Evolution de la Concentration moyenne en nitrates par Department",
           x = "Année",
           y = "Concentration moyenne en nitrates (mg/L)",
           color = "Departements") +
      theme_minimal() +
      theme(legend.position = "right") +
      theme(
        plot.title = element_text(size = 16),  # Title size
        axis.title.x = element_text(size = 14),  # X-axis title size
        axis.title.y = element_text(size = 14),  # Y-axis title size
        legend.text = element_text(size = 12)  # Legend text size
      )
    )
  })
  
  
  # Render Plot for Page 3
  output$nitratePlot <- renderPlot({
    # Filter data for the selected department
    dept_data <- nitrate_trend_dept %>% 
      filter(libelle_departement == input$department)
    
    # Plot nitrate concentration trends for the selected department
    ggplot(dept_data, aes(x = annee, y = mean_concentration)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(title = paste("Evolution de la Concentration en nitrate en", input$department),
           x = "Year",
           y = "Average Nitrate Concentration (mg/L)") +
      theme_gray()
  })
  
  

}
# Run the Shiny app
shinyApp(ui = ui, server = server)
