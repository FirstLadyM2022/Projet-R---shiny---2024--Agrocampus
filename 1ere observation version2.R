# chargement les packages nécessaires
library(readr)
library(dplyr)
library(tidyverse)
library(skimr)
library(summarytools)
library(visdat)
library(funModeling)
library(dlookr)
library(ggplot2)
library(maps)
library(leaflet)
#remove.packages()
#remove.packages(visdat)
install.packages("visdat")



########### chargement de la data:
quali_nitrate <- read_csv("C:/Users/mirei/Desktop/projet1 2024/DATA SET choisi/qualite-des-cours-deau-vis-a-vis-des-nitrates-en-bretagne.csv")
View(quali_nitrate)

#observation de la data
dim(quali_nitrate)
names(quali_nitrate)
descr(quali_nitrate)
str(quali_nitrate)
summary(quali_nitrate)
glimpse(quali_nitrate)
skim(quali_nitrate)
dfSummary(quali_nitrate) # sortie resumé dta magnifiaque!
dfSummary(quali_nitrate) %>% view()

#visualisation des données
vis_dat(quali_nitrate)
vis_miss(quali_nitrate)
plot_num(quali_nitrate)

# analyse des corrélation 
# dta with only num var car avec les var quali en lancant la vis_cor pour avoir 
# la matrix ca mets des messages d'erreur
df_num <- quali_nitrate %>% select_if(is.numeric) 
str(df_num)

#matrix
vis_cor(df_num)
# observation de la fréquence des var qual
freq(quali_nitrate$code_departement, plain.ascii = FALSE, style = "rmarkdown", order = "freq")
freq_var_qual <- freq(quali_nitrate)
?freq


############### selection du département
# selection 
df_filtre_Ille_et_Vilaine <- subset(quali_nitrate, libelle_departement == "Ille-et-Vilaine")
dfSummary(df_filtre_Ille_et_Vilaine) %>% view()
dfSummary(df_filtre_Ille_et_Vilaine$libelle_station) %>% view()



##################################################################
# Create mean et remplissage des na dans la colonne valeur_q90 sur la dataset département

# Supposons que df soit ta DataFrame et 'colonne' la colonne cible
df_filtre_Ille_et_Vilaine$valeur_q90[is.na(df_filtre_Ille_et_Vilaine$valeur_q90)] <- mean(df_filtre_Ille_et_Vilaine$valeur_q90, na.rm = TRUE)
dfSummary(df_filtre_Ille_et_Vilaine) %>% view()

####################################################################
dfSummary(df_filtre_meu) %>% view()

# selection de la station pour faire derrière le traitement des na:
df_filtre_meu <- subset(df_filtre_Ille_et_Vilaine, libelle_station == "MEU à MORDELLES")

#########################################################
# selection 
df_filtre <- subset(quali_nitrate, nom_sage == "Couesnon")

skim(df_filtre$nom_sage)



#################################""
# analyse univarié
# analyse bi-var
#realisation par ex du heat map de corr

install.packages("purrr")
library(purrr)

# Appliquer la fonction mean à chaque colonne et retourner un vecteur numérique
moyennes <- map_dbl(df_filtre, mean)

print(moyennes)

library(leaflet)
moyennes <- map_dbl(df_filtre, mean)

#########################################################################################
#cartes 
# Chargement des bibliothèques
library(leaflet)

# Création de la carte
m <- leaflet(df_filtre) %>%
  addTiles() %>%  # Ajouter la couche de tuiles
  addCircles(lng = ~longitude, lat = ~latitude, color = "blue", fillOpacity = 0.7, radius = 200) %>% 
  addPopups(lng = ~longitude, lat = ~latitude, popup = ~nom_cours_eau)
m







