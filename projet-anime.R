# 1- regrouper les données dans un seul et meme dataframe

library(dplyr)
library(readr)

multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
  Reduce(function(x,y) {merge(x,y,all = TRUE)}, datalist)
}

anime_dt = multmerge("C:/Users/cleme/Desktop/Projet_m2_tide/anime")

# 2) Observer pour chaque variable, le nombre et la proportion de valeurs manquantes

library(funModeling)

df_status(anime_dt) # Observer les variables

#Afficher un graph avec les valeurs manquantes vs les valeurs observées
library(Amelia)
missmap(anime_dt, main = "Missing values vs observed")

anime_dt[anime_dt == ""]<- NA # Remplacer les vides par des NA

#Afficher un graph avec les valeurs manquantes vs les valeurs observées
library(Amelia)
missmap(anime_dt, main = "Missing values vs observed")

df_status(anime_dt) # Observer à nouveau les variables

# Code personnel
#Compter le nombre de valeurs manquantes dans chacune des colonnes
sapply(anime_dt,function(x) sum(is.na(x)))

#Compter le nombre de NA dans une colonne en particulier
sum(is.na(anime_dt$rating_1000))

#Afficher les lignes dont la valeur dans la colonne "colonne" est NA
anime_dt[is.na(anime_dt$rating_1000),]

#Afficher un graph avec les valeurs manquantes vs les valeurs observées
library(Amelia)
missmap(anime_dt, main = "Missing values vs observed")

# 3) Représenter graphiquement la distribution du nombre d'épisodes

str(anime_dt) # verifier le type des variables du data

str(anime_dt["episodes"]) # verifier le type de la variable episode

sum(is.na(anime_dt$episodes)) # verifier s'il y a des NA ds episode

# convertir episodes en numeric
anime_dt <- transform(anime_dt, episodes = suppressWarnings(as.numeric(episodes)))

str(anime_dt["episodes"]) # verifier à nouveau le type de la variable episode

sum(is.na(anime_dt$episodes)) # verifier le nbre de NA

#Remplacer les valeurs manquantes par la mediane de la colonne
anime_dt$episodes[is.na(anime_dt$episodes)] <- median(anime_dt$episodes,na.rm=T)

## HISTOGRAMME
## Pour une seule variable
ggplot(anime_dt) +
  aes(x = episodes) +
  geom_histogram(fill ="yellow", breaks = c(0, 25, 50, 200, 500, 1000, 1800, 2000), color = "green", binwidth = 2) +
  ggtitle("la distribution du nombre d'épisodes") +
  xlab("Nbre d'épisodes") +
  ylab("Effectifs")

phd_hist <- anime_dt %>% ggplot(aes(x = episodes)) +
  geom_histogram(color = "black", fill = "green", bins = 50) +
  labs(x = "Years since Ph.D.", y = "Count", 
       title = "Years since Ph.D. Distribution")

phd_hist

# 4-Combien d'animes n'ont pas le genre « Shounen » ?
Shounen=filter(anime_dt,genre!="Shounen")
print(dim(Shounen))

# ou bien
shoun = subset(anime_dt, genre != "Shounen", select = c(name, genre))

# 5) Donner la proportion de « Shounen » au sein de chaque type d'anime


  
  





