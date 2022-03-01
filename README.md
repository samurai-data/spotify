# Présentation des données

Acousticness: Une mesure de confiance de 0,0 à 1,0 indiquant si la piste est acoustique. 1.0 représente une confiance élevée que la piste est acoustique.

Danceability: Danceability décrit à quel point une piste est appropriée pour la danse basée sur une combinaison d'éléments musicaux comprenant le tempo, la stabilité du rythme, la force du battement et la régularité générale. Une valeur de 0,0 est la moins dansable et 1,0 est la plus dansante.

Énergie: L'énergie est une mesure de 0,0 à 1,0 et représente une mesure perceptive de l'intensité et de l'activité. En règle générale, les pistes énergiques sont rapides, bruyantes et bruyantes. Par exemple, le death metal a une énergie élevée, tandis qu'un prélude de Bach obtient un score bas sur l'échelle. Les caractéristiques perceptives contribuant à cet attribut comprennent la plage dynamique, le volume sonore perçu, le timbre, la fréquence d'apparition et l'entropie générale.

Instrumentalité: Prédit si une piste ne contient pas de voix. Les morceaux de rap ou de mots parlés sont clairement «vocaux». Plus la valeur de l'instrument est proche de 1,0, plus la piste est susceptible de ne contenir aucun contenu vocal. Les valeurs supérieures à 0,5 sont destinées à représenter des pistes instrumentales, mais la confiance est plus élevée lorsque la valeur s'approche de 1,0.
Vivacité: détecte la présence d'un public dans l'enregistrement. Des valeurs de vivacité plus élevées représentent une probabilité accrue que la piste ait été jouée en direct. Une valeur supérieure à 0,8 offre une forte probabilité que la piste soit en direct.

Loudness: le volume global d'une piste en décibels (dB). Les valeurs de sonie sont moyennées sur toute la piste et sont utiles pour comparer le volume relatif des pistes. Le volume est la qualité d'un son qui est le principal corrélat psychologique de la force physique (amplitude). Les valeurs sont généralement comprises entre -60 et 0 db.

Speechiness: Speechiness détecte la présence de mots prononcés dans une piste. Plus l'enregistrement est exclusivement de type vocal (par exemple, talk-show, livre audio, poésie), plus la valeur d'attribut est proche de 1,0. Les valeurs supérieures à 0,66 décrivent des pistes qui sont probablement entièrement constituées de mots prononcés. Les valeurs comprises entre 0,33 et 0,66 décrivent des pistes qui peuvent contenir à la fois de la musique et de la parole, en sections ou en couches, y compris des cas tels que la musique rap. Les valeurs inférieures à 0,33 représentent très probablement de la musique et d'autres pistes non vocales.

Valence: Une mesure de 0,0 à 1,0 décrivant la positivité musicale véhiculée par une piste. Les pistes à valence élevée semblent plus positives (par exemple, joyeuses, gaies, euphoriques), tandis que les pistes à faible valence semblent plus négatives (par exemple, tristes, déprimés, en colère).
Tempo: Le tempo global estimé d'une piste en battements par minute (BPM). Dans la terminologie musicale, le tempo est la vitesse ou le rythme d'un morceau donné et dérive directement de la durée moyenne du battement.




# Chargements des librairies

```
library(jsonlite)
library(lubridate)
library(gghighlight)
library(spotifyr)
library(tidyverse)
library(knitr)
library(ggplot2)
library(plotly)
```

# Chargement des données

```
setwd('C:/Users/Inessa/Desktop/Portfolio Projects/my_spotify_data/MyData')
streamHistory0 <- fromJSON("StreamingHistory0.json", flatten = TRUE)
streamHistory1 <- fromJSON("StreamingHistory1.json", flatten = TRUE)
streamHistory <- rbind(streamHistory0,streamHistory1)
myLibrary <- fromJSON("YourLibrary.json", flatten = TRUE)
myLibrary <- data.frame(myLibrary$tracks)
```

# Statistique descriptive

```
head(streamHistory)
tail(streamHistory)
glimpse(streamHistory)
```
![Screenshot_1](https://user-images.githubusercontent.com/90149200/151656358-810447d0-6938-4140-87c1-dc0557f7f6f1.jpg)

```
head(myLibrary)
tail(myLibrary)
glimpse(myLibrary)
```
![Screenshot_2](https://user-images.githubusercontent.com/90149200/151656721-aa7f3cb7-3057-41b1-a926-6ef9a218ebae.jpg)

Nombre des chansons uniques que j'ai écouté pendant un an du 7/01/2021 à 7/01/2022
 ```
songs <- data.frame(unique(streamHistory$trackName))
count(songs) #2309 chansons
artists <- data.frame(unique(streamHistory$artistName))
count(artists) #1151
rm(songs, artists)
```
2309 chansons et 1151 artistes uniques sont identifiés.

# Nettoyage

Fusionner les deux variables qualitatives pour créer une variable ID. Je modifie également les liens web qui permettent l'accès aux chansons. On s'en servira par la suite pour créer un tableau de bord sur un logiciel de visualisation des données Tableau.
```
myLibrary <- myLibrary %>% mutate(ID = paste(myLibrary$artist, myLibrary$track, sep = ':')) %>% 
  mutate(URL = "https://open.spotify.com/embed/track/") %>%
  mutate(uri = sapply(uri, FUN = function(uri){substring(uri, regexpr('spotify:track:',uri) + 14, nchar(uri))}, USE.NAMES=FALSE) )
myLibrary <- mutate(URL = paste(URL, uri, sep = ''), myLibrary)
myLibrary <- select(myLibrary, -uri)
```


```
str(streamHistory) #Date est un caractère 
mySpotify <- streamHistory %>%
  mutate(endTime = as.POSIXct(endTime)) %>%
  mutate(date = floor_date(endTime, "day") %>% 
           as_date, weekday = wday(date, label = TRUE), seconds = msPlayed / 1000, minutes = seconds / 60, hours = minutes/60)
```

```
mySpotify <- data.frame(mySpotify) %>% mutate(ID = paste(mySpotify$artistName, mySpotify$trackName, sep = ':'))
str(mySpotify)

mySpotify <- mutate(date_hour = round_date(endTime,"hour"), mySpotify)
glimpse(mySpotify)  

data <- merge(x = mySpotify, y = myLibrary[,c('ID', 'URL')], by = 'ID', all.x = T)
data <- data %>% mutate(hour = hour(date_hour)) %>% 
  mutate(hour = as.character(hour))
data <- data %>% mutate(hour = paste(hour, 'h' ))
glimpse(data)



data <- data %>% mutate(URL = ifelse(ID == 'Robbie Williams:Angels', 'https://open.spotify.com/embed/track/1M2nd8jNUkkwrc1dgBPTJz', 
                                     ifelse(ID == 'Sara Lov:Fountain', 'https://open.spotify.com/embed/track/12xDGs4NYCsoNKdHYnoZZr', 
                                          ifelse(ID == 'C. Tangana:Antes de Morirme (feat. ROSALÍA)', 'https://open.spotify.com/embed/track/4Dl8bEzhAEGbcwkQawS1XG', URL))))

```


```
#Exportation de la base pour Tableau
write.csv(data,"C:/Users/Inessa/Desktop/Portfolio Projects/my_spotify_data/mySpotify.csv")

```


# Construction des graphiques
```
# Playback hour 
dayHour <- data %>% 
  group_by(date, hour = hour(date_hour)) %>% 
  summarize(hoursListened = sum(hours))

```

```
#Graphique
dayHour %>% 
  ggplot(aes(x= hour, y= hoursListened, group = date)) +
  geom_col(fill = "darkcyan") +
  scale_fill_brewer(palette = 3) +
  scale_x_continuous(breaks = seq(0, 24, 2)) +
  scale_y_continuous(breaks = seq(0, 60, 5)) +
  labs(title = "Mon activité sur Spotify pendant une journée", subtitle = "Activité de 0 à 24 heures") +
  xlab("L'heure") +
  ylab("Nombre d'heures") +
  theme_gray()
#Activité la plus elevée entre 16h et 18h

```

