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
# install.packages("maps")



########### chargement de la data:
data <- read.csv("/Users/massambadiop/Documents/INSTITUT AGRO/M2 Sciences des données/Analyse de données massives sur R/Projet-R---shiny---2024--Agrocampus-1/qualite-des-cours-deau-vis-a-vis-des-nitrates-en-bretagne.csv",stringsAsFactors  = T,header =T)
quali_nitrate$code_region <- factor(quali_nitrate$code_region)
quali_nitrate$code_departement <- factor(quali_nitrate$code_departement)
#observation de la data
dim(quali_nitrate)
descr(quali_nitrate)

# Concentrations moyennes par région
ggplot(data = quali_nitrate, mapping = aes(x = code_region, y = concentration_moy, colour = code_region)) +
  geom_boxplot() +
  labs(x=NULL, y="CM par Région") +
  theme_bw()

tapply(quali_nitrate$code_region, quali_nitrate$concentration_moy, summary)
# boxplots pa régions et dep
ggplot(quali_nitrate, aes(x = code_region, y = concentration_moy)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) + 
  geom_plot()
ggplot(quali_nitrate, aes(x = code_region, y = concentration_moy,fill=code_region)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) + 
  geom_plot() +
  facet_grid(~annee) + labs(title="Concentration par région et par Annee")


barplot(quali_nitrate$code_departement~quali_nitrate$concentration_moy)
glimpse(quali_nitrate)
skim(quali_nitrate)
dfSummary(quali_nitrate) # sortie resumé dta magnifiaque!
dfSummary(quali_nitrate) %>% view()


nlevels(quali_nitrate$nom_parametre)
nlevels(quali_nitrate$libelle_station)
nlevels(quali_nitrate$traitement)




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
# ?freq



# selection 
df_filtre <- subset(quali_nitrate, nom_sage == "Couesnon")

str(df_filtre) 

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

# Analyse temporelle
install.packages("caschrono")
library(readxl)
library(caschrono)
#library(forecast) #fct Arima
library(ggplot2)
library(ggfortify) #fct Autoplot
#Meteo=read_excel("/Users/massambadiop/INSTITUT AGRO/M1H/BIOINFO/Projet rosier/Projet Roses/Requete_METEO_STATION_49020001.xls",skip = 13)
data <- quali_nitrate
View(data)
summary(data$annee)
data_ts=na.omit(data) # Enlever la colonne UM qui contient que des "NA"
data.ts<-ts(data,start=1995,end=2023,frequency=1)# transformation en série temporelle


View(data.ts)


CM_global <- autoplot(data.ts[,22], xlab="Année", ylab="concentration_moy", main="Décompose concentration_moy")


