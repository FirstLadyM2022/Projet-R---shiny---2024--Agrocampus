# Installer les packages si nécessaire
# install.packages("shiny")
# install.packages("leaflet")
# install.packages("ggfortify")
# install.packages("bslib")
# install.packages("shinythemes") 

# Charger les bibliothèques nécessaires
library(shiny)
library(shinythemes)
library(bslib)
library(leaflet)
library(ggfortify)  # Pour autoplot (séries temporelles)
library(plotly)     # Pour ggplotly (graphique interactif)
library(dplyr)      # Pour la manipulation de données
library(ggplot2)    # Pour la visualisation
library(DT)         # Pour les tables de données interactives

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
  summarise(mean_concentration = mean(concentration_moy, na.rm = TRUE), .groups = 'drop')

# Définir l'interface utilisateur (UI)
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                
                
                titlePanel(h1("ShinyTrate Project")),
                wellPanel(h3("Description"),
                          p("La présence excessive de nitrates et de nitrites dans l'eau peut avoir des effets néfastes sur la santé humaine et la vie aquatique. Des niveaux élevés de nitrates dans l'eau potable ont été associés à divers problèmes de santé, notamment le cancer, le syndrome du bébé bleu et la méthémoglobinémie chez les nourrissons. De plus, la pollution par les nitrates dans les eaux de surface et les eaux souterraines peut favoriser la prolifération d'algues nuisibles, entraînant une réduction des niveaux d'oxygène dans l'eau, la mort des poissons et des dommages aux écosystèmes aquatiques."),
                          br(),
                          p("Cette application Shiny permet de visualiser la qualité des cours d'eau en Bretagne en fonction des concentrations de nitrates, avec des filtres par région, département et année. Les utilisateurs peuvent visualiser l'évolution de la concentration moyenne de nitrates ou la valeur Q90 au fil des ans, à l'aide d'une carte interactive colorée. Cet outil offre une analyse précise pour les gestionnaires environnementaux et les décideurs, facilitant la surveillance des cours d'eau et l'identification des tendances de pollution à long terme.")
                ),
              
                navbarPage("Application Interactive",
                           
                           # Page 1 - Table de données
                           
                           tabPanel("Table des Données",
                                    sidebarLayout(
                                      sidebarPanel(
                                        # filtres ici si nécessaire
                                        helpText("Tableau donnée"),
                                        width = 2 
                                      ),
                                      mainPanel(
                                        DT::dataTableOutput("dataTable"),  # Affichage de la table
                                        width = 10
                                      )
                                    )
                           ),
                           
                           
                           # Onglet pour l'analyse uni-variée et bi-variée
                           tabPanel("Analyse Uni et Bi-Variée",
                                    sidebarLayout(
                                      sidebarPanel(
                                        h4("Paramètres d'Analyse"),
                                        selectInput("region_input", "Sélectionnez une région", 
                                                    choices = unique(data$libelle_region), 
                                                    selected = unique(data$libelle_region)[1]),
                                        width = 3  # Réduire la largeur de la sidebar
                                      ),
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel("Analyse Uni-Variée",
                                                   plotOutput("hist_concentration"), # Distribution univariée
                                                   verbatimTextOutput("summary_stats")  # Résumé statistique
                                          ),
                                          tabPanel("Analyse Bi-Variée",
                                                   plotOutput("boxplot_concentration_region"), # Boxplot par région
                                                   plotlyOutput("scatterplot_concentration_prelev")  # Scatter plot concentration vs. prélèvements
                                          )
                                        ),
                                        width = 9  # Augmenter la largeur de la partie principale
                                      )
                                    )
                           ),
                           
                           # Nouvel onglet avec un deuxième sidebarLayout
                           tabPanel("Analyse Avancée",
                                    sidebarLayout(
                                      sidebarPanel(
                                        h4("Paramètres de l'Analyse Avancée"),
                                        # Limiter les choix des variables
                                        selectInput("variable_x", "Sélectionnez la variable X", 
                                                    choices = c("libelle_region", 
                                                                "annee", 
                                                                "libelle_departement", 
                                                                "libelle_commune", 
                                                                "nom_epci", 
                                                                "nom_sage", 
                                                                "libelle_station", 
                                                                "code_parametre"), 
                                                    selected = "libelle_region"),  # Sélection par défaut
                                        
                                        
                                        selectInput("variable_y", "Sélectionnez la variable Y", 
                                                    choices = c("concentration_min", 
                                                                "concentration_moy", 
                                                                "concentration_max", 
                                                                "nombre_prelevements", 
                                                                "nombre_prelevements_bon_etat", 
                                                                "classe_q90", 
                                                                "valeur_q90" ), 
                                                    selected = "concentration_moy"),  # Vous pouvez changer la valeur par défaut si nécessaire
                                        
                                        width = 2  # Réduire la largeur de la sidebar
                                      ),
                                      mainPanel(
                                        plotOutput("advanced_scatter"),  # Scatter plot avancé
                                        width = 10  # Ajuster la largeur de la partie principale
                                      )
                                    )
                           ),
                

                           
                           
                           # Page 1 - Évolution de la concentration en nitrates
                           tabPanel("Évolution de la concentration en nitrates",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("region", "Sélectionnez une région", 
                                                    choices = c("Bretagne", "Normandie", "Pays de la Loire"),
                                                    selected = "Bretagne"),
                                        width = 2  # Ajustement de la largeur de la sidebar
                                      ),
                                      mainPanel(
                                        plotOutput("nitratePlot3"),  # Graphe 1
                                        width = 10  # Ajustement de la largeur de la partie principale
                                      )
                                    )
                           ),
                           
                           # Page 2 - Évolution de la concentration en nitrates par Département
                           tabPanel("Évolution de la concentration en nitrates par Département",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("department", 
                                                    "Sélectionnez un Département:", 
                                                    choices = unique(nitrate_trend_dept$libelle_departement), 
                                                    selected = unique(nitrate_trend_dept$libelle_departement)[1]),
                                        width = 2  # Ajustement de la largeur de la sidebar
                                      ),
                                      mainPanel(
                                        plotlyOutput("nitratePlot_dep"),  # Graphe 2
                                        width = 10  # Ajustement de la largeur de la partie principale
                                      )
                                    )
                           ),
                           # Page 2 - Evolution de la concentration en nitrates par Département
                           tabPanel("Evolution de la concentration en nitrates par Département",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("department", 
                                                    "Select Departement:", 
                                                    choices = unique(nitrate_trend_dept$libelle_departement), 
                                                    selected = unique(nitrate_trend_dept$libelle_departement)[1]),
                                        width = 2
                                      ),
                                      
                                      mainPanel(
                                        plotOutput("nitratePlot"),
                                        width = 10
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
                                                                 "Class90" = "classe_q90"),  # Options de métrique
                                                     selected = "concentration_moy", 
                                                     inline = TRUE),  # Afficher les boutons radio en ligne
                                        width = 2
                                      ),
                                      mainPanel(
                                        leafletOutput("map", height = 600),  # Carte interactive
                                        width = 10
                                      )
                                    )
                           )
                           ,
                          
                           # Page 4 - Comparaison des Évolutions des Concentrations de Nitrates entre Régions
                           tabPanel("Comparaison des Évolutions",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("regions", "Sélectionnez les régions", 
                                                    choices = c("Bretagne", "Normandie", "Pays de la Loire"),
                                                    selected = "Bretagne",
                                                    multiple = TRUE),  # Permet la sélection multiple
                                        width = 2
                                      ),
                                      mainPanel(
                                        plotOutput("comparisonPlot"),
                                        width = 10
                                      )
                                    )
                           )
                )
)

# Définir la logique serveur
server <- function(input, output, session) {
  ####################1
  # Table interactive des données page 1
  output$dataTable <- DT::renderDataTable({
    datatable(
      data,
      options = list(
        pageLength = 10,                      # Nombre de lignes par page
        scrollX = TRUE,                       # Activer le défilement horizontal
        autoWidth = TRUE,                     # Ajustement automatique de la largeur
        columnDefs = list(list(width = '100px', targets = "_all"))  # Largeur fixe pour toutes les colonnes
      ),
      class = 'cell-border stripe'            # Style de tableau
    )
  })
  
  #########################################
  
  # Calcul des statistiques descriptives uni-variées
  output$summary_stats <- renderPrint({
    summary_stats <- data %>%
      summarise(
        concentration_moy_mean = mean(concentration_moy, na.rm = TRUE),
        concentration_moy_median = median(concentration_moy, na.rm = TRUE),
        concentration_moy_sd = sd(concentration_moy, na.rm = TRUE)
      )
    print(summary_stats)
  })
  
  # Histogramme pour la distribution de la concentration moyenne
  output$hist_concentration <- renderPlot({
    ggplot(data, aes(x = concentration_moy)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
      labs(title = "Distribution de la concentration moyenne en nitrates",
           x = "Concentration Moyenne (mg/L)", y = "Fréquence") +
      theme_minimal()
  })
  
  # Boxplot de la concentration par région
  output$boxplot_concentration_region <- renderPlot({
    ggplot(data %>% filter(libelle_region == input$region_input), 
           aes(x = libelle_region, y = concentration_moy, fill = libelle_region)) +
      geom_boxplot() +
      labs(title = paste("Distribution de la concentration moyenne en", input$region_input),
           x = "Région", y = "Concentration Moyenne (mg/L)") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Scatter plot : Relation entre concentration moyenne et nombre de prélèvements
  output$scatterplot_concentration_prelev <- renderPlotly({
    ggplot(data, aes(x = nombre_prelevements, y = concentration_moy)) +
      geom_point(alpha = 0.5, color = "dodgerblue") +
      geom_smooth(method = "lm", color = "red") +
      labs(title = "Relation entre concentration moyenne et nombre de prélèvements",
           x = "Nombre de prélèvements", y = "Concentration Moyenne (mg/L)") +
      theme_minimal()
  })
  
  # Scatter plot avancé basé sur les sélections
  output$advanced_scatter <- renderPlot({
    ggplot(data, aes_string(x = input$variable_x, y = input$variable_y)) +
      geom_point(alpha = 0.5, color = "green") +
      geom_smooth(method = "lm", color = "red") +
      labs(title = paste("Relation entre", input$variable_x, "et", input$variable_y),
           x = input$variable_x, y = input$variable_y) +
      theme_minimal()
  })
  
  
################
  
  # Mettre à jour les choix de colonne pour chaque page
  observe({
    updateSelectInput(session, "column1", choices = colnames(data[,20:26]))
  })
  
  ### Render Plot for Page 1
  # Graphe 1
  dataMir <- reactive({
    df <- data %>%
      rename(
        annee = annee, 
        libelle_region = libelle_region,  
        concentration_moy = concentration_moy,
        concentration_min = concentration_min, 
        concentration_max = concentration_max
      )
    
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
  #################plot 3 = "Évolution des Concentrations de Nitrates en"
  # Créer le graphique
  output$nitratePlot3 <- renderPlot({
    req(dataMir())
    ggplot(dataMir(), aes(x = annee)) +
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
  
  
  ########################### plot 2 = "Evolution de la Concentration moyenne en nitrates par Département"
  # Graphe 2
  output$nitratePlot_dep <- renderPlotly({
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
  
  # Graphe 3
  output$nitratePlot <- renderPlot({
    req(input$department, nitrate_trend_dept)
    
    dept_data <- nitrate_trend_dept %>% 
      filter(libelle_departement == input$department)
    
    ggplot(dept_data, aes(x = annee, y = mean_concentration)) +
      geom_line(color = 'royalblue4', size=2) +
      geom_point(color = "blue", size=2) +
      labs(title = paste("Evolution de la Concentration en nitrate en", input$department),
           x = "Année",
           y = "Concentration Moyenne en Nitrate (mg/L)") +
      theme_linedraw()
  })
  
  # UI dynamique pour le choix des départements
  output$departementUI <- renderUI({
    req(input$region)  # Assurer que la région est sélectionnée
    selectizeInput("departement", "Sélectionnez un Département :", 
                   choices = departements[[input$region]], 
                   selected = departements[[input$region]][1],  # Sélection par défaut
                   options = list(maxItems = 1))  # Limiter à une sélection
  })
  
  # Logique de la carte
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Ajouter les tuiles de base
      setView(lng = -3.5, lat = 48.5, zoom = 7)  # Centrer sur la Bretagne
  })
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
            "Class90:", data_filtered$classe_q90,
            "Nombre de Prélèvements:", data_filtered$nombre_prelevements
          ),
          labelOptions = labelOptions(
            direction = 'top', textOnly = TRUE,
            style = list("color" = "black", "background-color" = "white")
          ),
          popup = paste(
            "Station:", data_filtered$libelle_station, "<br>",
            "Concentration Moyenne:", data_filtered$concentration_moy, "mg/L", "<br>",
            "Class90:", data_filtered$classe_q90,
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
  
  
  # Logique pour le graphique de comparaison des évolutions des concentrations de nitrates entre régions
  output$comparisonPlot <- renderPlot({
    req(input$regions)
    
    comparison_data <- data %>% 
      filter(libelle_region %in% input$regions) %>% 
      group_by(annee, libelle_region) %>% 
      summarise(mean_concentration = mean(concentration_moy, na.rm = TRUE), .groups = 'drop')
    
    ggplot(comparison_data, aes(x = annee, y = mean_concentration, color = libelle_region)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(title = "Comparaison des Évolutions des Concentrations de Nitrates entre Régions",
           x = "Année",
           y = "Concentration Moyenne en Nitrates (mg/L)") +
      theme_minimal() +
      theme(legend.position = "right",
            plot.title = element_text(size = 16),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            legend.text = element_text(size = 12))
  })
}

# Lancer l'application
shinyApp(ui, server)
