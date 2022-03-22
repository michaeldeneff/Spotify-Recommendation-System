install.packages("spotifyr")
install.packages('lubridate')
library(spotifyr)
library(lubridate)
library(dplyr)
library(knitr)
library(sqldf)

Sys.setenv(SPOTIFY_CLIENT_ID = '827d1bba45e741f3b8ca6bde69b8ce4c')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '63bd8b4312fe474dbe3025bddf96031d')
access_token <- get_spotify_access_token()


# compare artists
# switch artists at the bottom

spotify=function(){
  cat("
Welcome to Spotify Reccomender\n
Enter S to start or Q to quit.")  
  userA= readline()
  count=1
  while(toupper(userA)!= "Q"){
    cat("Which artist would you like to look into?")
    artist= readline()
    cat(
"Which song do you like? \n
I can provide you a reccomendation of other music by ", artist , "\n
based on sound characteristics.")
    song=readline()
    cat("Awesome! One moment.......\n")

    spotify_data <- get_artist_audio_features(artist)
    
    spotify_data <- spotify_data[,c(1,30,20,6,9:12,14:19,37:38)]
    spotify_data= spotify_data %>% 
      mutate(Index = row_number()) %>% 
      filter(!duplicated(track_name))
    spotify_data=spotify_data[-17]
    
    user_track_id=spotify_data$track_id[spotify_data$track_name==song]
    
    song_stats=take_in(user_track_id)
    
    this= which(spotify_data$track_id == user_track_id, arr.ind=TRUE)
    
    spotify_data=spotify_data[-(this),]
    
    spotify_data= compare(song_stats)
  
    cat("Here's reccomendation #" ,count ,":\n
         Try out ", spotify_data$track_name[count],"\n
        It was released on ", spotify_data$album_release_date[count],". \n
        If you liked " ,song , " you'll defintley like this one!","\n
        ------------------------------------------------------------------ \n
        Want try another reccomendation? Click S \n
        Want to quit? Click Q")
        count=count+1
    userA=readline()
  }
  cat("Thanks for coming!")
}
debugonce(spotify)
spotify()


take_in=function(track_id){
  id=track_id
  danceability=spotify_data$danceability[spotify_data$track_id==id]
  energy=spotify_data$energy[spotify_data$track_id==id]
  key=spotify_data$key[spotify_data$track_id==id]
  loudness=spotify_data$loudness[spotify_data$track_id==id]
  mode=spotify_data$mode[spotify_data$track_id==id]
  speechiness=spotify_data$speechiness[spotify_data$track_id==id]
  acousticness=spotify_data$acousticness[spotify_data$track_id==id]
  instrumentalness=spotify_data$instrumentalness[spotify_data$track_id==id]
  liveness=spotify_data$liveness[spotify_data$track_id==id]
  valence=spotify_data$valence[spotify_data$track_id==id]
  tempo=spotify_data$tempo[spotify_data$track_id==id]
  key_name=spotify_data$key_name[spotify_data$track_id==id]
  mode_name=spotify_data$mode_name[spotify_data$track_id==id]
  key_mode=spotify_data$key_mode[spotify_data$track_id==id]
  
  song_stats=data.frame(danceability,energy,key,loudness,speechiness,acousticness,
                        instrumentalness,liveness,valence,tempo,
                        stringsAsFactors = FALSE)
  return(song_stats)
  
}

compare=function(song_stats){
  song_stats
 distances= numeric(0)
 value=numeric(0)
 count=1
 euclidean <- function(a, b) sqrt(sum((a - b)^2))
 while(count<=nrow(spotify_data)){
  value= euclidean(spotify_data[count,c(5:14)], song_stats)
  distances=c(distances,value)
  count=count+1
 }
   spotify_data= cbind(spotify_data, distance=distances)
   spotify_data= spotify_data[order(spotify_data$distance),]
   return(spotify_data)
}
debugonce(compare)
spotify_data= compare(song_stats)





























