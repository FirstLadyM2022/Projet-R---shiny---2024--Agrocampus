# install.packages("shiny")
# install.packages("leaflet")


# Load required libraries
library(shiny)
library(leaflet)


# Dataset



# Load necessary libraries
library(shiny)
library(ggfortify)  # For autoplot
library(plotly)     # For ggplotly

# Define the UI
ui <- navbarPage("Interactive Time Series App",
                 
                 
                 # Page 2
                 tabPanel("Evolutif de la concentration en nitrates",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("column2", "Select Column for Page 2:", choices = NULL)
                            ),
                            mainPanel(
                              plotlyOutput("plot2")
                            )
                          )
                 ),
                 
                 # Page 3
                 tabPanel("Page 3",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("column3", "Select Column for Page 3:", choices = NULL)
                            ),
                            mainPanel(
                              plotlyOutput("plot3")
                            )
                          )
                 ),
                 
                 # Page 4
                 tabPanel("Page 4",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("column4", "Select Column for Page 4:", choices = NULL)
                            ),
                            mainPanel(
                              plotlyOutput("plot4")
                            )
                          )
                 )
)

# Define the server logic
server <- function(input, output, session) {
  
  data <- read.csv("/Users/massambadiop/Documents/INSTITUT AGRO/M2 Sciences des données/Analyse de données massives sur R/Projet-R---shiny---2024--Agrocampus-1/qualite-des-cours-deau-vis-a-vis-des-nitrates-en-bretagne.csv",stringsAsFactors  = T, header = T)
  data.ts <- ts(data, start = 1995, end = 2023, frequency = 1)
  
  # Update choices for column selection in all pages
  observe({
    updateSelectInput(session, "column1", choices = colnames(data))
    updateSelectInput(session, "column2", choices = colnames(data))
    updateSelectInput(session, "column3", choices = colnames(data))
    updateSelectInput(session, "column4", choices = colnames(data))
  })
  

  
  # Render Plot for Page 2
  output$plot2 <- renderPlotly({
    req(input$column2)
    plot <- autoplot(data.ts[, input$column2], xlab = "Year", ylab = input$column2, main = paste("Time Series for", input$column2))
    ggplotly(plot)
  })
  
  # Render Plot for Page 3
  output$plot3 <- renderPlotly({
    req(input$column3)
    plot <- autoplot(data.ts[, input$column3], xlab = "Year", ylab = input$column3, main = paste("Time Series for", input$column3))
    ggplotly(plot)
  })
  
  # Render Plot for Page 4
  output$plot4 <- renderPlotly({
    req(input$column4)
    plot <- autoplot(data.ts[, input$column4], xlab = "Year", ylab = input$column4, main = paste("Time Series for", input$column4))
    ggplotly(plot)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
