# Installer les packages si nécessaire
# install.packages("shiny")
# install.packages("leaflet")
# install.packages("ggfortify")
# install.packages("bslib")
#install.packages("shinythemes")

# Charger les bibliothèques nécessaires
library(shiny)
library(shinythemes)
library(bslib)
library(leaflet)
library(ggfortify)  # Pour autoplot (séries temporelles)
library(plotly)     # Pour ggplotly (graphique interactif)

# Charger les données
data_path <- "qualite-des-cours-deau-vis-a-vis-des-nitrates-en-bretagne.csv"

if (file.exists(data_path)) {
  data <- read.csv(data_path, stringsAsFactors = TRUE)
  
  # Assurer que seules les colonnes numériques sont converties en séries temporelles
  numeric_cols <- sapply(data, is.numeric)
  data.ts <- ts(data[, numeric_cols], start = 1995, end = 2023, frequency = 1)
} else {
  stop("Le fichier de données est introuvable. Vérifiez le chemin.")
}

# Identifier les régions, départements et années
regions <- unique(data$libelle_region)
departements <- split(data$libelle_departement, data$libelle_region)
annees <- sort(unique(data$annee))  # Trier les années


data_gr <- data %>% 
  select(annee, libelle_departement, concentration_moy) %>% 
  filter(!is.na(concentration_moy))

# Group data by year and department to calculate the average nitrate concentration
nitrate_trend_dept <- data_gr %>% 
  group_by(annee, libelle_departement) %>% 
  summarise(mean_concentration = mean(concentration_moy))
# Définir l'interface utilisateur (UI)
ui <- fluidPage(theme = shinytheme("cerulean"),
  titlePanel(h1("ShinyTrate Project")), 
  navbarPage("Application Interactive des Séries Temporelles",
             
             # Page 1 - Évolution de la concentration en nitrates
             tabPanel("Evolutif de la concentration en nitrates",
                      sidebarLayout(
                        sidebarPanel(
                         
                          selectInput("region", "Sélectionnez une région", 
                                      choices = c("Bretagne", "Normandie", "Pays de la Loire"),
                                      selected = "Bretagne")
                        ),
                        mainPanel(
                          plotlyOutput("nitratePlot3"),  # Graphe 1
                          hr(),  # Ligne horizontale
                          plotlyOutput("nitratePlot_dep") # Graphe 2
                          

                        )
                      )
             ),
             tabPanel("Evolution de la concentration en nitrates par Département",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("department", 
                                      "Select Departement:", 
                                      choices = unique(nitrate_trend_dept$libelle_departement), 
                                      selected = unique(nitrate_trend_dept$libelle_departement)[1])
                        ),
                        
                        mainPanel(
                          plotlyOutput("nitratePlot")
                        )
                      )
             ),
             
             # Page 3 - Carte des prélèvements par région, département et année
             tabPanel("Cartographie",
                      sidebarLayout(
                        sidebarPanel(
                          selectizeInput("region", "Sélectionnez une Région :", 
                                         choices = regions, 
                                         selected = "Bretagne", 
                                         options = list(maxItems = 1)),  # Sélection par défaut
                          uiOutput("departementUI"),  # UI dynamique pour le choix des départements
                          sliderInput("annee", "Sélectionnez une Année :", 
                                      min = min(annees), 
                                      max = max(annees), 
                                      value = max(annees),  # Valeur par défaut : année maximale
                                      step = 1,
                                      ticks = FALSE),  # Ne pas afficher les ticks
                          radioButtons("metrique", "Choisissez la Métrique :", 
                                       choices = c("Concentration Moyenne" = "concentration_moy", 
                                                   "Q90" = "valeur_q90"),  # Options de métrique
                                       selected = "concentration_moy", 
                                       inline = TRUE)  # Afficher les boutons radio en ligne
                        ),
                        mainPanel(
                          leafletOutput("map", height = 600)  # Carte interactive
                        )
                      )
             )
  )
  
)

# Définir la logique serveur
server <- function(input, output, session) {
  
  # Mettre à jour les choix de colonne pour chaque page
  observe({
    # Assurez-vous que les colonnes sélectionnées existent dans le data.frame
    updateSelectInput(session, "column1", choices = colnames(data[,20:26]))
  })
  
  ### Render Plot for Page 1
  # Graphe 1
  dataMir <- reactive({
    
    
    # Vérification des colonnes et renommer si nécessaire
    df <- data %>%
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
  output$nitratePlot3 <- renderPlotly({
    req(dataMir())
   plotT <-  ggplot(dataMir(), aes(x = annee), cex=2) +
      geom_line(aes(y = concentration_moyenne, color = "Concentration Moyenne")) +
      geom_line(aes(y = concentration_minimale, color = "Concentration Minimale")) +
      geom_line(aes(y = concentration_maximale, color = "Concentration Maximale")) +
      geom_point(aes(y = concentration_moyenne, color = "Concentration Moyenne"), size = 1) +
      geom_point(aes(y = concentration_minimale, color = "Concentration Minimale"), size = 1) +
      geom_point(aes(y = concentration_maximale, color = "Concentration Maximale"), size = 1) +
      labs(
        title = paste("Évolution des Concentrations de Nitrates en", input$region),
        x = "Année",
        y = "Concentration de Nitrates (mg/L)",
      ) +
      scale_color_manual(
        values = c("Concentration Moyenne" = "blue", 
                   "Concentration Minimale" = "green", 
                   "Concentration Maximale" = "red"),
        name = "Type de Concentration"
      ) +
      theme_gray()
    ggplotly(plotT)
  })
  
  # Graphe 2
  output$nitratePlot_dep <- renderPlotly({
    # S'assurer que nitrate_trend_dept est défini

    
    ggplotly(ggplot(nitrate_trend_dept, aes(x = annee, y = mean_concentration, color = libelle_departement)) +
               geom_line(size=0.9) + 
               geom_point(size=1) +
               labs(title = "Evolution de la Concentration moyenne en nitrates par Département",
                    x = NULL,
                    y = "Concentration moyenne en nitrates (mg/L)",
                    color = "Départements") +
               theme_minimal() +
               theme(legend.position = "right",
                     plot.title = element_text(size = 16),
                     axis.title.x = element_text(size = 14),
                     axis.title.y = element_text(size = 14),
                     legend.text = element_text(size = 12))
    )
  })
  observe({
    # Assurez-vous que les colonnes sélectionnées existent dans le data.frame
    updateSelectInput(session, "column1", choices = colnames(data[,20:22]))
  })
  # Graphe 3
  output$nitratePlot <- renderPlotly({
    req(input$department, nitrate_trend_dept)
    
    dept_data <- nitrate_trend_dept %>% 
      filter(libelle_departement == input$department)
    
    plotD <- ggplot(dept_data, aes(x = annee, y = mean_concentration)) +
               geom_line(color = 'royalblue4', size=2) +
               geom_point(color = "blue", size=2) +
               labs(title = paste("Evolution de la Concentration en nitrate en", input$department),
                    x = "Year",
                    y = "Average Nitrate Concentration (mg/L)", cex=2) +
               theme_linedraw()+
      theme(plot.title = element_text(size = 16),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
    ggplotly(plotD)
  })
  
  dataMir <- reactive({
 
    
    # Vérification des colonnes et renommer si nécessaire
    df <- data %>%
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
  output$nitratePlot3 <- renderPlotly({
    req(dataMir())
    ggplot(dataMir(), aes(x = annee)) +
      geom_line(aes(y = concentration_moyenne, color = "Concentration Moyenne"), linewidth=1) +
      geom_line(aes(y = concentration_minimale, color = "Concentration Minimale"), linewidth=1) +
      geom_line(aes(y = concentration_maximale, color = "Concentration Maximale"), linewidth=1) +
      labs(
        title = paste("Évolution des Concentrations de Nitrates en", input$region),
        x = "Année",
        y = "Concentration de Nitrates (mg/L)", linewidth=1
      ) +
      scale_color_manual(
        values = c("Concentration Moyenne" = "blue", 
                   "Concentration Minimale" = "green", 
                   "Concentration Maximale" = "red"),
        name = "Type de Concentration"
      ) +
      theme_minimal()
  })
  
  # UI dynamique pour le choix des départements
  output$departementUI <- renderUI({
    req(input$region)  # Assurer que la région est sélectionnée
    selectizeInput("departement", "Sélectionnez un Département :", 
                   choices = departements[[input$region]], 
                   selected = "Finistère",  # Sélection par défaut
                   options = list(maxItems = 1, placeholder = "Choisissez un département..."))
  })
  
  # Initialisation de la carte
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Ajouter les tuiles de base
      setView(lng = -3.5, lat = 48.5, zoom = 7)  # Centrer sur la Bretagne
  })
  
  # Mise à jour de la carte en fonction des sélections
  observe({
    req(input$departement, input$annee)  # Assurer que le département et l'année sont sélectionnés
    
    # Filtrer les données pour le département et l'année sélectionnés
    data_filtered <- subset(data, libelle_departement == input$departement & annee == input$annee)
    
    leafletProxy("map") %>%
      clearMarkers() %>%  # Effacer les marqueurs précédents
      clearControls()  # Effacer les légendes précédentes
    
    if (nrow(data_filtered) > 0) {
      # Palette de couleurs en fonction de la métrique choisie
      pal <- colorNumeric(palette = "YlOrRd", domain = data_filtered[[input$metrique]])
      
      # Ajouter les marqueurs pour chaque station
      leafletProxy("map") %>%
        addCircleMarkers(
          data = data_filtered,
          lng = ~longitude, lat = ~latitude,
          color = pal(data_filtered[[input$metrique]]),
          fillOpacity = 0.7, radius = 10,
          label = paste(
            "Station:", data_filtered$libelle_station, "<br>",
            "Concentration Moyenne:", data_filtered$concentration_moy, "mg/L", "<br>",
            "Q90:", data_filtered$valeur_q90, "mg/L", "<br>",
            "Nombre de Prélèvements:", data_filtered$nombre_prelevements
          ),
          labelOptions = labelOptions(
            direction = 'top', textOnly = TRUE,
            style = list("color" = "black", "background-color" = "white")
          ),
          popup = paste(
            "Station:", data_filtered$libelle_station, "<br>",
            "Concentration Moyenne:", data_filtered$concentration_moy, "mg/L", "<br>",
            "Q90:", data_filtered$valeur_q90, "mg/L", "<br>",
            "Nombre de Prélèvements:", data_filtered$nombre_prelevements
          )
        )
      
      # Ajouter une légende pour la métrique choisie
      leafletProxy("map") %>%
        addLegend("bottomright", pal = pal, values = data_filtered[[input$metrique]],
                  title = input$metrique, opacity = 0.7)
      
    } else {
      # Message lorsque les données sont manquantes
      leafletProxy("map") %>%
        addMarkers(lng = -2.5, lat = 48.5, popup = "Aucun prélèvement disponible pour ce département et cette année.")
    }
  })
}

# Exécuter l'application Shiny
shinyApp(ui = ui, server = server)
