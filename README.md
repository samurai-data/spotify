# Analyse de données Spotify


## Présentation du projet
Spotify est une plate-forme populaire de streaming musical développée en Suède. Du fait que cette plate-forme est pratiquement le seul biais par lequel j'écoute de la musique j'ai décidé de profiter de l'opportunité d'obtenir une copie de la plupart de mes données personnelles recueillies par ce service pour analyser mes habitudes musicales de l'année dernière (2021).

###### Plan
 
- Nettoyage des données obtenues
- Visualisation graphique
- Construction d'un tableau de bord dynamique
- Tentative d'une analyse de classification (clustering) par la méthode de k-means

###### Outils

- *R Studio* pour le traitement et l'analyse de données
- *Tableau Public* pour la construction du tableau de bord



## Chargement des données

```
setwd('C:/Users/Inessa/Desktop/Portfolio Projects/my_spotify_data/MyData')
streamHistory0 <- fromJSON("StreamingHistory0.json", flatten = TRUE)
streamHistory1 <- fromJSON("StreamingHistory1.json", flatten = TRUE)
streamHistory <- rbind(streamHistory0,streamHistory1)
myLibrary <- fromJSON("YourLibrary.json", flatten = TRUE)
myLibrary <- data.frame(myLibrary$tracks)
```

## Inspection visuelle des base de données

```
glimpse(streamHistory)
glimpse(myLibrary)
```
![Screenshot_1](https://user-images.githubusercontent.com/90149200/156365077-b758c0f7-5a85-44c2-bd68-b6f0dba2f26f.jpg)
![Screenshot_2](https://user-images.githubusercontent.com/90149200/156729811-0fcbbef1-e092-49ec-b66a-ca30e2386ac1.jpg)

#### Déscription des variables
*streamingHistory* - historique de streaming:
- endTime - la date et l'heure de fin d'écoute au format UTC
- artistName - le nom du créateur de chaque élément écouté (par ex., le nom de l'artiste s'il s'agit d'un titre)
- trackName - le nom des éléments écoutés
- msPlayed - le nombre de millisecondes pendant lesquelles le titre a été écouté.

*myLibrary* - bibliothèque (chansons aimées) :
- artist - les créateurs
- album - les noms des albums et émissions
- track - les noms des entités
- uri - les URI (Uniform Resource Identifiers) des éléments



Nombre des chansons uniques que j'ai écouté pendant un an du 7/01/2021 à 7/01/2022
 ```
songs <- data.frame(unique(streamHistory$trackName))
count(songs) #2309 chansons
artists <- data.frame(unique(streamHistory$artistName))
count(artists) #1151
rm(songs, artists)
```
2309 chansons et 1151 artistes uniques sont identifiés.

## Nettoyage

Je fusionne le nom de l'artiste et le titre de la chanson de chaque élément écouté pour créer une nouvelle variable *ID*. Je crée également la variable *URL* (les liens web qui permettent l'accès aux chansons sur Spotify) à partir de la variable *uri*. On s'en servira par la suite pendant la construction du tableau de bord sur le logiciel de visualisation des données *Tableau*.
```
myLibrary <- myLibrary %>% mutate(ID = paste(myLibrary$artist, myLibrary$track, sep = ':')) %>% 
  mutate(URL = "https://open.spotify.com/embed/track/") %>%
  mutate(id = sapply(uri, FUN = function(uri){substring(uri, regexpr('spotify:track:',uri) + 14, nchar(uri))}, USE.NAMES=FALSE) )
myLibrary <- myLibrary %>% mutate(URL = paste(URL, id, sep = '')) %>%
  select(,-c(uri,id))
glimpse(myLibrary)
```
![Screenshot_3](https://user-images.githubusercontent.com/90149200/156733512-baeddd89-72f7-4269-bd37-0bf46408816a.jpg)


Les lignes du code suivant permettent d'effectuer les traitements nécessaires avec les dates. Notamment, le problème de type de données incorrect est réglé et la date ainsi que la date avec l'heure arrondie sont extraites. De plus, les mille seconds sont converties en nombre d'heures.
```
mySpotify <- streamHistory %>%
  mutate(endTime = as.POSIXct(endTime)) %>%
  mutate(date = floor_date(endTime, "day") %>% 
           as_date, weekday = wday(date,label = T, week_start = getOption("lubridate.week.start", 1)), seconds = msPlayed / 1000, minutes = seconds / 60, hours = minutes/60)

#Date avec l'heure arrondie
mySpotify <- mutate(date_hour = round_date(endTime,"hour"), mySpotify)
glimpse(mySpotify)
```

![Screenshot_4](https://user-images.githubusercontent.com/90149200/156880599-12dbc178-03e1-4399-a0dc-c1ad01be6c26.jpg)


Le code ci-dessous fusionne les deux bases. Le but est d'ajouter la variable URL à la base mySpotify.
```
data <- merge(x = mySpotify, y = myLibrary[,c('ID', 'URL')], by = 'ID', all.x = T)
#Extraire l'heure arrondie uniquement à partir de la variable 'date_hour' dans la base fusionnée
data <- data %>% 
  mutate(hour = as.character(hour(date_hour))) %>% 
  mutate(hour = paste(hour, 'h' ))
glimpse(data)
```

```
#Pour jeter un coup d'œil sur la base finale
glimpse(data)
```
![Screenshot_5](https://user-images.githubusercontent.com/90149200/156881127-ccda5ba6-8560-47f1-ab9c-7499b24d687f.jpg)


```
#La je reviens après la construction de mon tableau de bord sur Tableau afin de faire qqs ajustements
#Notamment rajouter manuellement les URL manquants nécessaires à la visualisation graphique
data <- data %>% mutate(URL = ifelse(ID == 'Robbie Williams:Angels', 'https://open.spotify.com/embed/track/1M2nd8jNUkkwrc1dgBPTJz', 
                                     ifelse(ID == 'Sara Lov:Fountain', 'https://open.spotify.com/embed/track/12xDGs4NYCsoNKdHYnoZZr', 
                                            ifelse(ID == 'C. Tangana:Antes de Morirme (feat. ROSALÍA)', 'https://open.spotify.com/embed/track/4Dl8bEzhAEGbcwkQawS1XG', URL))))


data <- data %>% mutate(artist_url = ifelse(artistName == 'The Lumineers', 'https://open.spotify.com/embed/artist/16oZKvXb6WkQlVAjwo2Wbg',
                                   ifelse(artistName == 'Mika Newton', 'https://open.spotify.com/embed/artist/74EZBv1sl8tSXbttKYhAC0',
                                          ifelse(artistName == 'Alai Oli', 'https://open.spotify.com/embed/artist/4snI0qikpQST1U1VWAxEY6',
                                                 ifelse(artistName == 'Robbie Williams', 'https://open.spotify.com/embed/artist/2HcwFjNelS49kFbfvMxQYw',
                                                        ifelse(artistName == 'Freakonomics Radio', '<https://open.spotify.com/embed/show/6z4NLXyHPga1UmSJsPK7G1',
                                                               ifelse(artistName == 'Amaral', 'https://open.spotify.com/embed/artist/4OkeTQCk0fvX6VBYpOOxDi',
                                                                      ifelse(artistName == 'Sara Lov', 'https://open.spotify.com/embed/artist/53qqj1ih4yfPRgUYtEJjVR',
                                                                             ifelse(artistName == 'Ed Sheeran', 'https://open.spotify.com/embed/artist/6eUKZXaKkcviH0Ku9w2n3V',
                                                                                    ifelse(artistName == 'Backstreet Boys', 'https://open.spotify.com/embed/artist/5rSXSAkZ67PYJSvpUpkOr7',
                                                                                           ifelse(artistName == 'Paramore', 'https://open.spotify.com/embed/artist/74XFHRwlV6OrjEM0A2NCMF', NA )))))))))))
```

```
#Exportation de la base pour Tableau
write.csv(data,"C:/Users/Inessa/Desktop/Portfolio Projects/my_spotify_data/mySpotify.csv")

```


## Construction des graphiques
```
# Playback hour 
dayHour <- data %>% 
  group_by(date, hour = hour(date_hour)) %>% 
  summarize(hoursListened = sum(hours))

```

```
#Bar plot
dayHour %>% 
  ggplot(aes(x= hour, y= hoursListened, group = date)) +
  geom_col(fill = "darkcyan") +
  scale_fill_brewer(palette = 3) +
  scale_x_continuous(breaks = seq(0, 24, 2)) +
  scale_y_continuous(breaks = seq(0, 60, 5)) +
  labs(title = "Evolution horaire de mon activité sur Spotify pendant une journée(24heures)", subtitle = "Mon activité sur Spotify en 2021") +
  xlab("L'heure de la journée") +
  ylab("Nombre d'heures ecoutées par an") +
  theme_gray()
#Activité la plus elevée entre 16h et 18h
```

![Rplot](https://user-images.githubusercontent.com/90149200/156884676-f7338316-bd36-46ef-b599-560526d9c31a.jpeg)


```
# QUEL JOUR DE LA SEMAINE ET À QUELLE HEURE J'AI ECOUTE DE LA MUSIQUE LE PLUS SOUVENT?
#Préparation des données 
weekHour <- data %>% 
  group_by(weekday, hour=hour(date_hour)) %>% 
  summarize(hours = sum(hours))

# Heatmap
heatmap <- weekHour%>%
  ggplot(aes(x = hour, weekday, fill = hours)) + 
  geom_tile() + 
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(x= "L'heure de la journée", y= "Jour de la semaine") + labs(fill="Nombre d'heures par an") +
  ggtitle("Mon activité horaire cumulée sur Spotify en 2021 par jour de la semaine et l'heure de la journée")
heatmap

```
![Rplot01](https://user-images.githubusercontent.com/90149200/156884734-ef5cd466-37dd-42a8-bf73-7a87a3449b19.jpeg)


```
#EVOLUTION DE MON ACTIVITE SUR SPOTIFY 
dayType <- weekHour %>% 
  mutate(day_type = if_else(weekday %in% c("Sat", "Sun"), "Weekend", "Semaine")) %>% 
  group_by(day_type, hour) %>% 
  summarize(hours = sum(hours)) %>% 
  ggplot(aes(x = hour, y = hours, color = day_type)) + 
  geom_line() +
  labs(x= "L'heure de la journée", y= "Nombre d'heures", color = '') + 
  ggtitle("Evolution de mon activité sur Spotify en semaine et les weekends", "Mon activité sur Spotify en 2021") 
dayType
```
![Rplot02](https://user-images.githubusercontent.com/90149200/156884883-0ee8e904-da5b-46f6-ad7c-774287dbcdd2.jpeg)


```
# LES ARTISTES LES PLUS ECOUTES (PLUS DE 4 HEURES)
ArtistsMostListened <- data %>% 
  group_by(artistName) %>% 
  summarize(hours = sum(hours)) %>%
  filter(hours > 5) %>%
  ggplot(aes(x = artistName, y = hours)) + 
  geom_col(aes(fill = hours)) +
  scale_fill_gradient(low = "yellow", high = "red") + 
  labs(x= "Artistes", y= "Nombre d'heures", fill = "Nombre d'heures") + 
  ggtitle("Les artistes les plus ecoutés en 2021", "> 5 heures par an") +
  theme(axis.text.x = element_text(angle = 90))
ArtistsMostListened
```
![Rplot03](https://user-images.githubusercontent.com/90149200/156884863-81fa3b43-7120-4a80-bed0-16ed67576391.jpeg)

```
# LES CHANSONS LES PLUS ECOUTEES
SongsMostListened <- data %>% 
  group_by(trackName) %>% 
  summarize(hours = sum(hours)) %>% 
  filter(hours > 5) %>%
  arrange(hours) %>%
  mutate(trackName=factor(trackName, levels=trackName)) %>% 
  ggplot(aes(x = trackName, y = hours)) + 
  geom_col(fill = 'brown1' ) +
  labs(x= "Chansons", y= "Nombre d'heures") + 
  ggtitle("Les chansons les plus ecoutées en 2021?", "> 5 heures") +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip()
SongsMostListened
```

![Rplot04](https://user-images.githubusercontent.com/90149200/156884960-d762ad84-df7d-4b86-b9e7-341b7d1fdc84.jpeg)


## Clustering

###### Présentation des variables choisies pour l'analyse

- **Acousticness**: Une mesure de confiance de 0,0 à 1,0 indiquant si la piste est acoustique. 1.0 représente une confiance élevée que la piste est acoustique.
- **Danceability**: Danceability décrit à quel point une piste est appropriée pour la danse basée sur une combinaison d'éléments musicaux comprenant le tempo, la stabilité du rythme, la force du battement et la régularité générale. Une valeur de 0,0 est la moins dansable et 1,0 est la plus dansante.
- **Energie**: L'énergie est une mesure de 0,0 à 1,0 et représente une mesure perceptive de l'intensité et de l'activité. En règle générale, les pistes énergiques sont rapides et bruyantes. Par exemple, le death metal a une énergie élevée, tandis qu'un prélude de Bach obtient un score bas sur l'échelle. Les caractéristiques perceptives contribuant à cet attribut comprennent la plage dynamique, le volume sonore perçu, le timbre, la fréquence d'apparition et l'entropie générale.
- **Instrumentalness**: Prédit si une piste ne contient pas de voix. Les morceaux de rap ou de mots parlés sont clairement «vocaux». Plus la valeur de l'instrument est proche de 0, plus la piste est susceptible de ne contenir aucun contenu vocal. Les valeurs supérieures à 0,5 sont destinées à représenter des pistes instrumentales, mais la confiance est plus élevée lorsque la valeur s'approche de 1
- **Liveness**: détecte la présence d'un public dans l'enregistrement. Des valeurs de vivacité plus élevées représentent une probabilité accrue que la piste ait été jouée en direct. Une valeur supérieure à 0,8 offre une forte probabilité que la piste soit en direct.
- **Loudness**: le volume global d'une piste en décibels (dB). Les valeurs de sonie sont moyennées sur toute la piste et sont utiles pour comparer le volume relatif des pistes. Le volume est la qualité d'un son qui est le principal corrélat psychologique de la force physique (amplitude). Les valeurs sont généralement comprises entre -60 et 0 db.
- **Speechiness**: Speechiness détecte la présence de mots prononcés dans une piste. Plus l'enregistrement est exclusivement de type vocal (par exemple, talk-show, livre audio, poésie), plus la valeur d'attribut est proche de 1,0. Les valeurs supérieures à 0,66 décrivent des pistes qui sont probablement entièrement constituées de mots prononcés. Les valeurs comprises entre 0,33 et 0,66 décrivent des pistes qui peuvent contenir à la fois de la musique et de la parole, en sections ou en couches, y compris des cas tels que la musique rap. Les valeurs inférieures à 0,33 représentent très probablement de la musique et d'autres pistes non vocales.
- **Valence**: Une mesure de 0,0 à 1,0 décrivant la positivité musicale véhiculée par une piste. Les pistes à valence élevée semblent plus positives (par exemple, joyeuses, gaies, euphoriques), tandis que les pistes à faible valence semblent plus négatives (par exemple, tristes, déprimés, en colère).
- **Tempo**: Le tempo global estimé d'une piste en battements par minute (BPM). Dans la terminologie musicale, le tempo est la vitesse ou le rythme d'un morceau donné et dérive directement de la durée moyenne du battement.

