# Chargements des librairies
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


#PREMIERE STAT DESC
glimpse(streamHistory)
head(streamHistory)
tail(streamHistory)
summary(streamHistory)

#Nombre des chansons uniques que j'ai écouté pendant un an du 7/01/2021 à 7/01/2022
songs <- data.frame(unique(streamHistory$trackName))
count(songs) #2039 chansons
artists <- data.frame(unique(streamHistory$artistName))
count(artists) #1151
rm(songs, artists)

#NETTOYAGE
str(streamHistory) #Date est un caractère 
mySpotify <- streamHistory %>%
  mutate(endTime = as.POSIXct(endTime)) %>%
  mutate(date = floor_date(endTime, "day") %>% 
           as_date, weekday = wday(date, label = TRUE), seconds = msPlayed / 1000, minutes = seconds / 60, hours = minutes/60)

levels(mySpotify$weekday) <- list(Lundi = 'Mon', Mardi = 'Tue', Mercredi = 'Wed', Jeudi = 'Thu', Vendredi = 'Fri', Samedi = 'Sat', Dimanche = 'Sun')
mySpotify <- data.frame(mySpotify)

str(mySpotify)
#check
mySpotify <- mySpotify  %>% mutate(hour = hour(endTime)))
check <- mySpotify[which(mySpotify$hour == 5 & mySpotify$weekday== 'Dimanche'),]
str(check)

mySpotify <- mySpotify %>% mutate(heure = format(round(mySpotify$endTime, units="hours")), format="%H:%M")
  
  
# PLAYBACK ACTIVITY BY DATE AND TIME OF DAY
mois <- mySpotify %>%
  group_by(date = floor_date(date, "month")) %>% 
  summarize(hours = sum(minutes) / 60) %>% arrange(date)

semaine <- mySpotify %>%
  group_by(date = floor_date(date, "week")) %>% 
  summarize(hours = sum(minutes) / 60) %>% arrange(date)
rm(mois, semaine)
############
# Playback hour 
dayHour <- mySpotify %>% 
  group_by(date, hour = hour(endTime)) %>% 
  summarize(hoursListened = sum(hours))
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
#Activité la plus elevée entre 15h et 18h
###########

#CHOISIR LES COULEURS POUR LES GRAPHIQUES
library(remotes)
remotes::install_github("smach/rcolorutils", build_vignettes = TRUE)
library(rcolorutils)
create_color_table(page_length = 50)


# PLAYBACK ACTIVITY BY TIME OF THE DAY AND WEEKDAY
# Playback minutes 
dayMinutes <- mySpotify %>% 
  group_by(weekday, hour=hour(endTime)) %>% 
  summarize(minutes = sum(minutes))

dayMinutes%>%
  ggplot(aes(x = hour, weekday, fill = minutes)) + 
  geom_tile() + 
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(x= "Time of the day", y= "Weekday") + 
  ggtitle("What weekday and time of day I've listened to the most music on Spotify?", "Weekly activity from 0 to 24 hours")

dayType <- dayMinutes %>% 
  mutate(day_type = if_else(weekday %in% c("Samedi", "Dimanche"), "weekend", "weekday")) %>% 
  group_by(day_type, hour) %>% 
  summarize(minutes = sum(minutes)) %>% 
  ggplot(aes(x = hour, y = minutes, color = day_type)) + 
  geom_line() +
  labs(x= "Time of the day", y= "Minutes of music playback") + 
  ggtitle("What day type I've listened to the most music on Spotify?", "Weekday and weekend activity from 0 to 24 hours") 
dayType

# LES ARTISTES LES PLUS ECOUTES (PLUS DE 3 HEURES)
minutesMostListened <- mySpotify %>% 
  group_by(artistName) %>% 
  summarize(minutes = sum(minutes)) %>% 
  filter(minutes >= 240) %>%
  ggplot(aes(x = artistName, y = minutes)) + 
  geom_col(aes(fill = minutes)) +
  scale_fill_gradient(low = "yellow", high = "red") + 
  labs(x= "Artist", y= "Minutes of music playback") + 
  ggtitle("What were the most listened artists on my Spotify?", "> 8 hours listened") +
  theme(axis.text.x = element_text(angle = 90))
minutesMostListened


# ETABLIR CONNECTION SPOTIFY API AFIN D'OBTENIR D'AUTRES VARIABLES  
Sys.setenv(SPOTIFY_CLIENT_ID = '40798829f9cf4e028eea17da9f299abe')
Sys.setenv(SPOTIFY_CLIENT_SECRET ='b14ce5c852de4975b20683b0c373393f')
get_spotify_authorization_code()


###########

favArtist1 <- get_artist_audio_features('The Lumineers')
favArtist2 <- get_artist_audio_features('James Blunt')
favArtist3 <- get_artist_audio_features('Kodaline')
favArtist4 <- get_artist_audio_features('Alai Oli')

topFourArtists <- rbind(favArtist1, favArtist2, favArtist3, favArtist4)

# PLOT EMOTIONAL QUADRANT TOP FOUR ARTISTS
emotionalQuadrant <- ggplot(data = topFourArtists, aes(x = valence, y = energy, color = artist_name)) +
  geom_jitter() +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  annotate('text', 0.25 / 2, 0.95, label = "Angry / Turbulent") +
  annotate('text', 1.75 / 2, 0.95, label = "Joyful / Happy") +
  annotate('text', 1.75 / 2, 0.05, label = "Peace / Chill") +
  annotate('text', 0.25 / 2, 0.05, label = "Depressing / Sad") +
  labs(x= "Valence", y= "Energy") +
  ggtitle("Emotional quadrant Top four artists", "Based on energy y valence")  
emotionalQuadrant






dt <- get_my_top_artists_or_tracks(type = 'artists', time_range = 'long_term', limit = 5) %>% 
  select(name, genres) %>% 
  rowwise %>% 
  mutate(genres = paste(genres, collapse = ',')) %>% 
  ungroup %>% 
  kable() #créer une table pour illustrer les résultats
