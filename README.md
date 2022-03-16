# Analyse de données Spotify
> Emmanuel Kant : « La musique est la langue des émotions »

## Présentation du projet
Spotify est une plate-forme populaire de streaming musical développée en Suède. Du fait que cette plate-forme est pratiquement le seul biais par lequel j'écoute de la musique j'ai décidé de profiter de l'opportunité d'obtenir une copie de la plupart de mes données personnelles recueillies par ce service pour analyser mes habitudes musicales de l'année dernière (2021).

###### Plan
 
- Nettoyage des données obtenues
- Visualisation graphique
- Construction d'[un tableau de bord dynamique](https://public.tableau.com/app/profile/inessa7042/viz/SpotifyViz_16429507843540/Dashboard1)
- Tentative d'une analyse de clustering par la méthode de k-means

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

## Inspection visuelle des bases de données

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

## Nettoyage et d'autres opérations de traitement des données

Je commence par la création de la variable *ID* (le nom de l'artiste et le titre de la chanson fusionnés) pour faciliter le traitement des données. La variable *URL* (les liens web qui permettent l'accès aux chansons sur Spotify) est également créée à partir de *uri* de chaque élément écouté. On s'en servira par la suite pendant la construction du tableau de bord sur le logiciel de visualisation des données *Tableau*.
```
myLibrary <- myLibrary %>% mutate(ID = paste(myLibrary$artist, myLibrary$track, sep = ':')) %>% 
  mutate(URL = "https://open.spotify.com/embed/track/") %>%
  mutate(id = sapply(uri, FUN = function(uri){substring(uri, regexpr('spotify:track:',uri) + 14, nchar(uri))}, USE.NAMES=FALSE) )
myLibrary <- myLibrary %>% mutate(URL = paste(URL, id, sep = '')) %>%
  select(,-c(uri))
glimpse(myLibrary)
```

![Screenshot_11](https://user-images.githubusercontent.com/90149200/157263762-518ddcac-c265-4f6c-aac7-d124c03e0423.jpg)


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
#On jete un coup d'œil sur la base finale
glimpse(data)
```
![Screenshot_5](https://user-images.githubusercontent.com/90149200/156881127-ccda5ba6-8560-47f1-ab9c-7499b24d687f.jpg)


Là je reviens après la construction de mon tableau de bord sur Tableau afin de faire quelques ajustements. Notamment, rajouter manuellement les URL manquants nécessaires à la visualisation graphique.
```
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
#Exportation de la base finale pour Tableau
write.csv(data,"C:/Users/Inessa/Desktop/Portfolio Projects/my_spotify_data/mySpotify.csv")

```


## Construction des graphiques
Comment mon activité sur Spotify évolue pendant 24h en 2021?
```
# Préparation des données
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

![Screenshot_17](https://user-images.githubusercontent.com/90149200/157633706-b4a22b5d-a366-4541-916e-d6705fe63087.jpg)



Quel jour de la semaine et à quelle heure j'écoute de la musique le plus souvent en 2021?
```
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

![Screenshot_16](https://user-images.githubusercontent.com/90149200/157633746-d751872f-687c-435b-934a-140b48143eb5.jpg)


Est-ce que l'évolution de mon activité sur Spotify en semaine est différente de celle de week-ends ?
```
#Line chart 
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

![Screenshot_15](https://user-images.githubusercontent.com/90149200/157633211-6c490aa4-bbac-4dba-8639-d579cf96fa09.jpg)



```
# LES ARTISTES LES PLUS ECOUTES (PLUS DE 5 HEURES)
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


![Screenshot_18](https://user-images.githubusercontent.com/90149200/157634008-7c74b0b0-d0a9-4baa-8621-d1be02dd5784.jpg)


```
# LES CHANSONS LES PLUS ECOUTEES (PLUS DE 5 HEURES)
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

![Screenshot_14](https://user-images.githubusercontent.com/90149200/157632885-a661933f-d1f9-4f25-8576-cb45f18e256e.jpg)


Pour la construction du dernier graphique il faut établir la connexion spotify API afin d'obtenir le code d'autorisation qui va me permettre d'accéder les caractéristiques audio des artistes sélectionnés. 
```
#Préparation des données
Sys.setenv(SPOTIFY_CLIENT_ID = '40798829f9cf4e028eea17da9f299abe')
Sys.setenv(SPOTIFY_CLIENT_SECRET ='b14ce5c852de4975b20683b0c373393f')
get_spotify_authorization_code()

favArtist1 <- get_artist_audio_features('The Lumineers')
favArtist2 <- get_artist_audio_features('Robbie Williams')
favArtist3 <- get_artist_audio_features('Mika Newton')
favArtist4 <- get_artist_audio_features('Alai Oli')

topFourArtists <- rbind(favArtist1, favArtist2, favArtist3, favArtist4)

# PLOT EMOTIONAL QUADRANT TOP 4 ARTISTS
emotionalQuadrant <- ggplot(data = topFourArtists, aes(x = valence, y = energy, color = artist_name)) +
  geom_jitter() +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  annotate('text', 0.25 / 2, 0.95, label = "Turbulent & Pessimiste") +
  annotate('text', 1.75 / 2, 0.95, label = "Joyeux & Energique") +
  annotate('text', 1.75 / 2, 0.05, label = "Calme & Optimiste") +
  annotate('text', 0.25 / 2, 0.05, label = "Depressif & Triste") +
  labs(x= "Valence", y= "Energie") +
  ggtitle("Quadrant emotionnel Top quatre artistes", "Basée sur énergie et valence")  
emotionalQuadrant

```

![Screenshot_13](https://user-images.githubusercontent.com/90149200/157632574-6d78b4b0-648c-4492-a23d-00d2556a68bb.jpg)


## Clustering k-means

Clustering est une méthode de classification automatique dont le but est de créer des groupes (clusters) d'observations homogènes de sorte que les observations au sein d’un groupe soient les plus semblables possibles, alors que les groupes soient les plus différents possibles les uns des autres. 
Dans le cadre de ce projet l'objectif de clustering est d'analyser la musique que j'écoute selon les caractéristiques audio de mes chansons préférées. Pour ce faire, un échantillon représentatif sera crée principalement à partir de ma bibliothèque Spotify *myLibrary* i.e. chansons aimées. 

Parmi les deux algorithmes de clustering les plus connus (k-means et classification hiérarchique) j'ai recours à l'algorithme de k-means afin de réaliser cette analyse. Celui-ci repose sur la notion de distances entre observations. On utilise généralement la distance euclidienne sur des données normalisées (si les échelles de valeurs des variables sont différentes). Elle se calcule comme suit : 

![Screenshot_20](https://user-images.githubusercontent.com/90149200/157688931-400042c8-60da-4999-bf55-5bfdab03cc16.jpg)

La variation intra classe totale est alors définie comme la somme des distances euclidiennes entre chaque observation et son centroïde correspondant:


![Screenshot_28](https://user-images.githubusercontent.com/90149200/157912273-357a954f-5f70-4b42-b420-385836f630c1.jpg)

[*source*](https://bradleyboehmke.github.io/HOML/kmeans.html)

L'idée principale de k-means est de construire les clusters de telle sorte que la variation intraclasse soit minimisée. 


###### Présentation des variables sélectionnées pour l'analyse

- **Acousticness**: Une mesure de confiance de 0,0 à 1,0 indiquant si la piste est acoustique. 1.0 représente une confiance élevée que la piste est acoustique.
- **Danceability**: Danceability décrit à quel point une piste est appropriée pour la danse basée sur une combinaison d'éléments musicaux comprenant le tempo, la stabilité du rythme, la force du battement et la régularité générale. Une valeur de 0,0 est la moins dansable et 1,0 est la plus dansante.
- **Energie**: L'énergie est une mesure de 0,0 à 1,0 et représente une mesure perceptive de l'intensité et de l'activité. En règle générale, les pistes énergiques sont rapides et bruyantes. Par exemple, le death metal a une énergie élevée, tandis qu'un prélude de Bach obtient un score bas sur l'échelle. Les caractéristiques perceptives contribuant à cet attribut comprennent la plage dynamique, le volume sonore perçu, le timbre, la fréquence d'apparition et l'entropie générale.
- **Instrumentalness**: Prédit si une piste ne contient pas de voix. Les morceaux de rap ou de mots parlés sont clairement «vocaux». Plus la valeur de l'instrument est proche de 0, plus la piste est susceptible de ne contenir aucun contenu vocal. Les valeurs supérieures à 0,5 sont destinées à représenter des pistes instrumentales, mais la confiance est plus élevée lorsque la valeur s'approche de 1
- **Liveness**: détecte la présence d'un public dans l'enregistrement. Des valeurs de vivacité plus élevées représentent une probabilité accrue que la piste ait été jouée en direct. Une valeur supérieure à 0,8 offre une forte probabilité que la piste soit en direct.
- **Loudness**: le volume global d'une piste en décibels (dB). Les valeurs de sonie sont moyennées sur toute la piste et sont utiles pour comparer le volume relatif des pistes. Le volume est la qualité d'un son qui est le principal corrélat psychologique de la force physique (amplitude). Les valeurs sont généralement comprises entre -60 et 0 db.
- **Speechiness**: Speechiness détecte la présence de mots prononcés dans une piste. Plus l'enregistrement est exclusivement de type vocal (par exemple, talk-show, livre audio, poésie), plus la valeur d'attribut est proche de 1,0. Les valeurs supérieures à 0,66 décrivent des pistes qui sont probablement entièrement constituées de mots prononcés. Les valeurs comprises entre 0,33 et 0,66 décrivent des pistes qui peuvent contenir à la fois de la musique et de la parole, en sections ou en couches, y compris des cas tels que la musique rap. Les valeurs inférieures à 0,33 représentent très probablement de la musique et d'autres pistes non vocales.
- **Valence**: Une mesure de 0,0 à 1,0 décrivant la positivité musicale véhiculée par une piste. Les pistes à valence élevée semblent plus positives (par exemple, joyeuses, gaies, euphoriques), tandis que les pistes à faible valence semblent plus négatives (par exemple, tristes, déprimés, en colère).
- **Tempo**: Le tempo global estimé d'une piste en battements par minute (BPM). Dans la terminologie musicale, le tempo est la vitesse ou le rythme d'un morceau donné et dérive directement de la durée moyenne du battement.


###### Préparation de l'échantillon
D'abord, il faut obtenir les variables nécessaires, càd les fonctionnalités audio des pistes qui se trouvent dans ma bibliothèque.
Tant que la fonction *get_track_audio_features* ne fonctionne que pour une base de données limitée à 100 observations, la table *myLibrary* contenant 383 chansons est alors découpée en 4 tables. Ensuite, j'obtient toutes les variables nécessaires et crée une base. 
```
dt <- myLibrary[c(1:100),]
dt2 <- myLibrary[c(101:200),]
dt3<- myLibrary[c(201:300),]
dt4 <- myLibrary[c(301:383),]


dat <- get_track_audio_features(dt$id, get_spotify_access_token())
dat2  <- get_track_audio_features(dt2$id, get_spotify_access_token())
dat3  <- get_track_audio_features(dt3$id, get_spotify_access_token())
dat4  <- get_track_audio_features(dt4$id, get_spotify_access_token())

features <- rbind(dat, dat2, dat3, dat4) %>% as_tibble()
rm(dt, dt2, dt3, dt4, dat, dat2, dat3, dat4)

features <- merge(myLibrary[,c('ID','id')],features[,c('danceability', 'energy', 'loudness',
                                                       'speechiness','acousticness', 'instrumentalness', 
                                                       'liveness', 'valence', 'tempo', 'id')])
```



Ensuite, je rajoute manuellement quelques playlists que j'écoute souvent pour élargir la taille de l'échantillon, puisque k-means est le plus efficace avec les échantillons de grande taille. Un filtre est appliqué afin de sélectionner les observations qui se trouvent également dans mon historique de streaming. Ainsi, parmi les 331 morceaux retrouvés dans les playlists j'ai pu ajouter 150 observations (chansons).

```
playlist_uris = c('2EyrMzdCEzrJZroVISvpeH', '1Q5ShmHUpMqgMMsiYeSlnv', '21FgZfWciibMrQJUHAaDnM','37i9dQZF1DX2oc5aN4UDfD', '1rPzZa9xevryBnc5TKEcd1', '37i9dQZF1DWTwnEm1IYyoj', '37i9dQZF1DWUH2AzNQzWua', '37i9dQZF1DZ06evO0AGqf6')
new_features <- get_playlist_audio_features(username = '21fpb4vqdeiicsqeug75tiuta',playlist_uris)
#331 chansons


new_features <- new_features %>% filter(track.name %in% data$trackName) #150

new_features <- new_features[,c('track.artists','track.name', 'track.id',
                                'danceability', 'energy', 'loudness','speechiness','acousticness', 'instrumentalness','liveness', 'valence', 'tempo')]

glimpse(new_features)
```
![Screenshot_21](https://user-images.githubusercontent.com/90149200/157696204-5683d64f-8b0f-42d4-99c2-f92b8bb8deec.jpg)


Je termine la préparation de la base destinée à l'analyse de clustering par quelques manipulations du traitement des données. 
```
playlists_features = new_features %>% 
  mutate(track.artists=sapply(new_features$track.artists,'[[',3)) %>%
  mutate(ID = paste(track.artists, track.name, sep = ':')) %>%
  select(,-c(track.artists, track.name)) %>%
  relocate(ID, .after=track.id) %>%
  rename('id' = 'track.id')

df_features <- rbind(features, playlists_features) #533
```
![Screenshot_19](https://user-images.githubusercontent.com/90149200/157639209-4d7443eb-6a65-46ba-9b2c-24f2783b4379.jpg)


Après avoir détecté la présence des doublons je les retire. De plus, j'exclut les variables id et ID dans le but d'isoler les variables quantitatives pour l'analyse.
```
#éliminer les doublons
duplicated(df_features)
df_features <- unique(df_features) #504

#sélectionner les variables quantitatives
df_clustering <- df_features[,c(3:11)]

head(df_clustering)
```
![Screenshot_22](https://user-images.githubusercontent.com/90149200/157744600-f50155fd-5319-4af0-97c1-c2d11baa4c7b.jpg)

###### Prédiagnostique et prétraitement de la base

Maintenant je dois vérifier si les données manquantes sont présentes dans la table et si cette dernière contient les variables qui sont fortement corrélées entre elles. Cette vérification est nécessaire avant la réalisation de clustering, puisque l'efficacité de cette technique n'est pas garantie en présence de ces éléments.
```
#Vérifier la présence des données manquantes
colSums(is.na(df_clustering)) #pas de données manquantes
#Matrice de corrélation
cor = cor(df_clustering)
#corrélogramme pour visualiser la matrice de corrélation
library(corrplot)
corrplot(cor, method = 'color') 
```
![Screenshot_24](https://user-images.githubusercontent.com/90149200/157746077-6157cf3b-58a5-4926-ab24-d59f1ae73b97.jpg)

On constate l'absence de données manquantes, mais il y a une forte corrélation entre acousticness et energy mais j'ai décidé quand meme de garder ces variables puisque la suppression de l'une ou l'autre n'a pas impacté de manière significative la qualité du modèle. 

Ensuite, il faut standardiser les variables pour assurer la comparabilité de ses valeurs dont les échelles sont différentes. 
```
#Standardisation (normalisation) des variables
df_clustering_scaled <- data.frame(scale(df_clustering))
```

Une autre étape importante est d'évaluer la tendance de mes données au partitionnement. Pour cela j'utilise [la statistique de Hopkins](https://en.wikipedia.org/wiki/Hopkins_statistic). 
```
library(hopkins)
h <- get_clust_tendency(df_clustering_scaled, n=5)
h$hopkins_stat
#hopkins_stat = 0,83 : highly clusterable data
```
La statistique de Hopkins est proche de 1. J'en déduis alors que mes données ont une tendance élevée au partitionnement ce qui me permet de les, finalement, regrouper. 

La dernière étape avant le clustering est de déterminer le nombre optimal des clusters. Parmi les nombreuses méthodes qui existent j'ai recours à celle qui est la plus connue et utilisée: la méthode du coude. Elle nous permet de visualiser le nombre differents des clusters en fonction de l'inertie (variation totale intra classe). 
```
#nombre des clusters optimal
library(NbClust)
#Elbow method
fviz_nbclust(df_clustering_scaled, kmeans, method = "wss") +
  labs(subtitle = "Elbow method") # add subtitle
```

![Screenshot_27](https://user-images.githubusercontent.com/90149200/157909254-37693080-40f5-4f77-8f14-35e4e05d19dd.jpg)

En suivant la logique de la méthode de k-means on veut minimiser l'inertie ce qui m'incite à choisir k = 5. 
```
model <- kmeans(df_clustering_scaled, centers = 5)
clusters <-  model$centers
model$betweenss/model$totss
#visualisation des clusters
fviz_cluster(model, data = df_clustering_scaled,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())
```

![Screenshot_25](https://user-images.githubusercontent.com/90149200/157881779-473face7-24be-4cff-aabd-efdf6e3be91a.jpg)


```
#Visualiser différents profils des clusters
df_features$cluster <- model$cluster
cluster_prof <- df_features %>% group_by(cluster) %>% summarise_if(is.numeric,mean)
```
![Screenshot_26](https://user-images.githubusercontent.com/90149200/157882096-6220ea5b-4a9c-4aea-931d-11f741ea2dd4.jpg)

Cluster 1: Les pistes plutôt appropriées pour la danse et relativement positives
Cluster 2: Acoustiques, calmes et instrumentales
Cluster 3: Energiques avec le rythme assez rapide
Cluster 4: Acoustiques, vocales et avec le taux de valence assez bas donc négatives
Cluster 5: Les pistes très énergiques jouées très probablement en direct (concerts)

