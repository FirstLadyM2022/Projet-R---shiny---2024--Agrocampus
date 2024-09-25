# Chargement des bibliothèques
library(leaflet)

# Exemple de dataframe avec des coordonnées
df <- data.frame(
  name = c("Point 1", "Point 2", "Point 3"),
  longitude = c(8.5500, 8.5100, 8.5600),
  latitude = c(47.3700, 47.3800, 47.3600)
)

# Création de la carte
m <- leaflet(df) %>%
  addTiles() %>%  # Ajouter la couche de tuiles
  addCircles(lng = ~longitude, lat = ~latitude, color = "blue", fillOpacity = 0.7, radius = 200) %>% 
  addPopups(lng = ~longitude, lat = ~latitude, popup = ~name)

# Afficher la carte
m

