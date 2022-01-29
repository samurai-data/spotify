# Welcome to GitHub Desktop!

This is your README. READMEs are where you can communicate what your project is and how to use it.

Write your name on line 6, save it, and then head back to GitHub Desktop.


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

#Statistique descriptive

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

# Nettoyage

Fusionner les deux variables qualitatives pour créer une variable ID et modifier les liens qui permettent l'accès aux chansons.
```
myLibrary <- myLibrary %>% mutate(ID = paste(myLibrary$artist, myLibrary$track, sep = ':')) %>% 
  mutate(URL = "https://open.spotify.com/embed/track/") %>%
  mutate(uri = sapply(uri, FUN = function(uri){substring(uri, regexpr('spotify:track:',uri) + 14, nchar(uri))}, USE.NAMES=FALSE) )
myLibrary <- mutate(URL = paste(URL, uri, sep = ''), myLibrary)
myLibrary <- select(myLibrary, -uri)
```



