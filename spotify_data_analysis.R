# CHARGEMENT DES LIBRARIE
library(jsonlite)
library(lubridate)
library(gghighlight)
library(spotifyr)
library(tidyverse)
library(knitr)
library(ggplot2)
library(plotly)


# CHARGEMENT DES DONNEES
setwd('C:/Users/Inessa/Desktop/Portfolio Projects/my_spotify_data/MyData')
streamHistory0 <- fromJSON("StreamingHistory0.json", flatten = TRUE)
streamHistory1 <- fromJSON("StreamingHistory1.json", flatten = TRUE)
streamHistory <- rbind(streamHistory0,streamHistory1)
myLibrary <- fromJSON("YourLibrary.json", flatten = TRUE)
myLibrary <- data.frame(myLibrary$tracks)


#INSPECTION VISUELLE DES DONNEES
glimpse(streamHistory)
glimpse(myLibrary)

#Nombre des chansons uniques que j'ai écouté pendant un an du 7/01/2021 à 7/01/2022
songs <- data.frame(unique(streamHistory$trackName))
count(songs) #2309 chansons
artists <- data.frame(unique(streamHistory$artistName))
count(artists) #1151
rm(songs, artists)


#NETTOYAGE ET D'AUTRES OPERATIONS DU TRAITEMENT DES DONNEES
myLibrary <- myLibrary %>% mutate(ID = paste(myLibrary$artist, myLibrary$track, sep = ':')) %>% 
  mutate(URL = "https://open.spotify.com/embed/track/") %>%
  mutate(id = sapply(uri, FUN = function(uri){substring(uri, regexpr('spotify:track:',uri) + 14, nchar(uri))}, USE.NAMES=FALSE) )
myLibrary <- mutate(URL = paste(URL, id, sep = ''), myLibrary) %>%
  select(,-c(uri))
glimpse(myLibrary)


#Problème: Date est un caractère, on convertit alors cette variable en datetime
mySpotify <- streamHistory %>%
  mutate(endTime = as.POSIXct(endTime)) %>%
  mutate(date = floor_date(endTime, "day") %>% 
           as_date, weekday = wday(date,label = T, week_start = getOption("lubridate.week.start", 1)), seconds = msPlayed / 1000, minutes = seconds / 60, hours = minutes/60)

#Date avec l'heure arrondie
mySpotify <- mutate(date_hour = round_date(endTime,"hour"), mySpotify)

#Création de la variable ID dans la base mySpotify
mySpotify <- data.frame(mySpotify) %>% 
  mutate(ID = paste(mySpotify$artistName, mySpotify$trackName, sep = ':'))

glimpse(mySpotify) 


#La fusion de deux bases
data <- merge(x = mySpotify, y = myLibrary[,c('ID', 'URL')], by = 'ID', all.x = T)
# L'heure arrondie dans la base fusionnée
data <- data %>% 
  mutate(hour = as.character(hour(date_hour))) %>% 
  mutate(hour = paste(hour, 'h' ))

#Pour jeter on coup d'oeuil sur la base finale
glimpse(data)

#Rajouter manuellement les URL manquants nécessaires à la visualisation graphique
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
#Exportation de la base sur Excel
write.csv(data,"C:/Users/Inessa/Desktop/Portfolio Projects/my_spotify_data/mySpotify.csv")



############VISUALISATION GRAPHIQUE##################

#A quelle heure de la journée J'écoutais de la musique le plus souvent?
# Préparation des données pour un bar plot
dayHour <- data %>% 
  group_by(date, hour = hour(date_hour)) %>% 
  summarize(hoursListened = sum(hours))


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
###########

#Choisir les couleurs pour les graphiques
library(remotes)
#l'installation est pour la première fois uniquement
#remotes::install_github("smach/rcolorutils", build_vignettes = TRUE) 
library(rcolorutils)
create_color_table(page_length = 50)


# Quelle jour de la semaine et à quelle heure J'écoutais de la musique le plus souvent?
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
#pic: weekends ~16h, semaine ~18h

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
  ggtitle("Les chansons les plus ecoutées en 2021", "> 5 heures") +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip()
SongsMostListened


# Etablir la connexion spotify API afin d'obtenir le code d'autorisation
Sys.setenv(SPOTIFY_CLIENT_ID = '40798829f9cf4e028eea17da9f299abe')
Sys.setenv(SPOTIFY_CLIENT_SECRET ='b14ce5c852de4975b20683b0c373393f')
get_spotify_authorization_code()

favArtist1 <- get_artist_audio_features('The Lumineers')
favArtist2 <- get_artist_audio_features('Robbie Williams')
favArtist3 <- get_artist_audio_features('Mika Newton')
favArtist4 <- get_artist_audio_features('Alai Oli')

topFourArtists <- rbind(favArtist1, favArtist2, favArtist3, favArtist4)

# PLOT EMOTIONAL QUADRANT TOP FOUR ARTISTS
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


###############ANALYSE DE CLUSTERING#################

#OBTENIR LES CARACTERISTIQUES AUDIO DES CHANSONS DE MA LIBRARIE  

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
#383


#Préparation de l'échantillon pour clustering

playlist_uris = c('2EyrMzdCEzrJZroVISvpeH', '1Q5ShmHUpMqgMMsiYeSlnv', '21FgZfWciibMrQJUHAaDnM','37i9dQZF1DX2oc5aN4UDfD', '1rPzZa9xevryBnc5TKEcd1', '37i9dQZF1DWTwnEm1IYyoj', '37i9dQZF1DWUH2AzNQzWua', '37i9dQZF1DZ06evO0AGqf6')
new_features <- get_playlist_audio_features(username = '21fpb4vqdeiicsqeug75tiuta',playlist_uris)
#331 chansons


new_features <- new_features %>% filter(track.name %in% data$trackName)                                                
#150

new_features <- new_features[,c('track.artists','track.name', 'track.id',
                                'danceability', 'energy', 'loudness','speechiness','acousticness', 'instrumentalness','liveness', 'valence', 'tempo')]

glimpse(new_features)

#quelques manipulations du traitement des données
playlists_features = new_features %>% 
  mutate(track.artists=sapply(new_features$track.artists,'[[',3)) %>%
  mutate(ID = paste(track.artists, track.name, sep = ':')) %>%
  select(,-c(track.artists, track.name)) %>%
  relocate(ID, .after=track.id) %>%
  rename('id' = 'track.id')

df_features <- rbind(features, playlists_features) #533
glimpse(df_features)

#éliminer les doublons
duplicated(df_features)
df_features <- unique(df_features) #504

glimpse(df_features)

#sélectionner les variables quantitatives
df_clustering <- df_features[,c(3:11)] 

head(df_clustering)

#PRE-DIAGNISTIQUE

#Vérifier la presence des données manquantes
colSums(is.na(df_clustering))
#Matrice de corrélation
cor = cor(df_clustering)
#Corrélogramme pour visualiser la matrice de corrélation
library(corrplot)
corrplot(cor, method = 'color') 

#Standartisation (normalisation) des variables
df_clustering_scaled <- data.frame(scale(df_clustering))

#Evaluer la tendance des données au partitionnement
library(factoextra)
h <- get_clust_tendency(df_clustering_scaled, n=5)
h$hopkins_stat # 0.83
#our data is highly clusterable

#Détérminer le nombre des clusters optimal
library(NbClust)
#Elbow method
fviz_nbclust(df_clustering_scaled, kmeans, method = "wss") +
  labs(subtitle = "Elbow method") # add subtitle


#K-MEANS

model <- kmeans(df_clustering_scaled, centers = 5)
model
clusters <-  model$centers
model$size
model$betweenss/model$totss
fviz_cluster(model, data = df_clustering_scaled,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())

#Visualiser dif profiles des clusters
df_features$cluster <- model$cluster
cluster_prof <- df_features %>% group_by(cluster) %>% summarise_if(is.numeric,mean)
glimpse(df_features)


#Visualiser chaque cluster
cluster1 = df_features %>% filter(df_features$cluster == 1)
cluster2 = df_features %>% filter(df_features$cluster == 2)
cluster3 = df_features %>% filter(df_features$cluster == 3)
cluster4 = df_features %>% filter(df_features$cluster == 4)
cluster5 = df_features %>% filter(df_features$cluster == 5)












