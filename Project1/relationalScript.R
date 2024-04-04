library(tidyverse)
library(ggplot2)
library(dplyr)
library(arrow)
library(readr)

#creates date and time column from listened_at column
getDateTime <- function(df) {
  #progress bar creation
  min <- 1
  max <- nrow(df) 
  
  #if no proper amount of given albums to look from, then return nothing
  timePB <- txtProgressBar(min, max, style = 3)  
  
  #separate into 2 columns
  date <- character(0)
  time <- character(0)
  
  print("Obtaining Time Data...")
  #iterate through each row of df
  for (i in 1:nrow(df)) {
    #get listened row value
    listened <- as.character(df[i,]$listened_at)
    
    #get date split by " " 
    listenedSplit <- strsplit(listened, " ")[[1]]
    
    #insert into char vector based on length of data received
    if (length(listenedSplit) == 2) {
      #both data are present
      #insert data into resepective character vector
      date <- c(date, listenedSplit[[1]])
      time <- c(time, listenedSplit[[2]])
      
    } else if (length(listenedSplit) == 1) {
      #we know it contains data on one part of date, but not the other
      if (grepl("-", listenedSplit[[1]])) {
        #DD-MM-YYYY
        date <- c(date, listenedSplit[[1]])
        time <- c(time, "")
        
      } else {
        #HH:MM:SS
        date <- c(date, "")
        time <- c(time, listenedSplit[[2]])
      }
    } else {
      #nothing === NA
      date <- c(date, "")
      time <- c(time, "")
    }
    
    #update PB
    setTxtProgressBar(timePB, i)
  }
  
  #Close the progress bar for date and time
  close(timePB)
  print("Time Data Obtained!")
  
  #create new columns based on character vectors
  df$date <- date
  df$time <- time
  
  #delete the old column
  df <- df[,!names(df) %in% c("listened_at")]
  
  return (df)
}

#get visual for artist
getTop10 <- function(df) {
  
  artistCounts <- table(df$artist_name)
  
  top10 <- head(sort(artistCounts, decreasing = TRUE), n = 10)
  
  par(oma = c(0,4,0,0))
  
  top10df <- as.data.frame(top10)
  ggsave("top10.png", dpi=400)
  
  plot <- ggplot(top10df, aes(y = Var1, x = Freq, fill = Freq)) + 
    geom_bar(position = "dodge", stat = "identity") + 
    scale_fill_gradient2() + 
    labs(title = "Top 6 Artists by Song Count", y = "Artist", x = "Number of Songs")
  
  dev.off()
  
  return()
}

#choose only 1 file and work with that. then we will repeat this process with
#a few more files for more data after the presentation
parquetDF <- read_parquet('0.parquet')

#recording_mbid, release_mbid, recording_msid are not needed:
#   recording_mbid - id used for the song in the db for data retrieval/storage
#                       not relevant for the scope of our project
#
#   release_mbid - unique id for each album that is released (eg. diff versions)
#                       unnecessary since each album will have a diff name
#
#   recording_msid - id for this particular song. Redundant since we will use
#                       song, album, and artist for identifying the song
#
#   artist_credit_id - id for the artist. Redundant since we will use the
#                       artist name
#
#   artist_credit_mbids - artists credited with release. Irrelevant since they 
#                       don't match the artist_credit_id's => some other set of
#                       id's used internally for tracking

parquetDF <- parquetDF[,!(names(parquetDF) %in% c("recording_mbid", 
                "release_mbid", "recording_msid", "artist_credit_id",
                "artist_credit_mbids"))]

#get date, time columns
parquetDF <- getDateTime(parquetDF)

#replace NA's
if (sum(is.na(parquetDF$user_id)) > 0) {
  #there exist NA's for user_id
  #drop these rows
  parquetDF <- parquetDF[!is.na(parquetDF$user_id),]
}
if (sum(is.na(parquetDF$artist_name)) > 0) {
  #there exist NA's for artist_name
  #rename to Unknown Artist
  parquetDF <- parquetDF %>% replace_na(list(artist_name = "Unknown Artist"))
}
if (sum(is.na(parquetDF$release_name)) > 0) {
  #there exist NA's for release_name
  #rename to Unknown Album
  parquetDF <- parquetDF %>% replace_na(list(release_name = "Unknown Album"))
}
if (sum(is.na(parquetDF$recording_name)) > 0) {
  #there exist NA's for recording_name
  #rename to Unknown Song
  parquetDF <- parquetDF %>% replace_na(list(recording_name = "Unknown Song"))
}

#get unique Artists and amount
print(unique(parquetDF$artist_name))
print(length(unique(parquetDF$artist_name)))

#get img for top 10 artists
getTop10(parquetDF)

#add the data to a csv file
print("Adding song data to csv file ...")
write_csv(parquetDF, "completeData.csv")

#read the data from the csv file
CSVData <- read_csv("completeData.csv")