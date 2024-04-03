library(tidyverse)
library(rvest)
library(utils)
library(stringr)

#import the genres from the txt file and songData from csv
genresList <- readLines("genres.txt") %>% lapply(tolower)
songData <- read.csv("songData.csv") %>% unique()

#saved the data from this list of genres into a txt file
# https://gist.github.com/andytlr/4104c667a62d8145aa3a

#########################################################################
# Fix the non-genre tags

tags <- songData$Tags

#iterate through each row of tags
for (i in 1:length(tags)) {
  #get tags split by the ";" (lowercased)
  rowTags <- strsplit(tags[i], ";")[[1]] %>% lapply(tolower)
  
  newRowTags <- character(0)
  
  #iterate through the tags for each row and append the ones matching the list
  for (tag in rowTags) {
    if ((tag %in% genresList) && !(tag %in% newRowTags)) {
      newRowTags <- c(newRowTags, tag)
    }
  }
  
  #create the string we will replace the tag with
  stringOfTags = ""
  for (tag in newRowTags) {
    stringOfTags <- paste(stringOfTags, tag, sep = ";")
  }
  
  #replace index in tags
  tags[i] <- stringOfTags %>% substr(2, nchar(stringOfTags))
}

#replace the tags in the df
songData['Tags'] <- tags

#     Note:     No rows should be removed
rm(tag)
rm(tags)
rm(rowTags)
rm(stringOfTags)
rm(newRowTags)
#########################################################################
# change all release dates to be just the year

years <- songData$Date.Released

#iterate through each row of years
for (i in 1:length(years)) {
  #get tags split by the " " (lowercased)
  entireDate <- strsplit(years[i], " ")[[1]] %>% lapply(tolower)
  
  newDate <- ""
  
  #iterate through the strings for each row and keep the one that matches
  #the length of 4 and is numerical if it exists
  for (datePart in entireDate) {
    if ((nchar(datePart) == 4) && (grepl('[0-9]+', datePart))){
      newDate <- datePart
      break
    }
  }
  
  #o/w change to N/A
  if (newDate == "") {
    newDate <- "N/A"
  }
  
  #replace index in years
  years[i] <- newDate
}

#Note: there still exist some N/A's in Date.Released

#obtain this subset of data
dateNA <- filter(songData, Date.Released == 'N/A')



#replace the tags in the df
songData['Date.Released'] <- years

#     Note:     No rows should be removed
rm(years)
rm(newDate)
rm(entireDate)
rm(datePart)
#########################################################################
# ensure all songs have a proper song length

songLengths <- songData$Song.Length

improperLengthsdf <- data.frame()

#iterate through each row of song lengths
for (i in 1:length(songLengths)) {
  
  #if the current row has an improper length
  if (grepl('[^0-9:]+', songLengths[i])) {
    improperLengthsdf <- rbind(improperLengthsdf, songData[i,])
  }
}




#     Note:     No rows should be removed

#########################################################################
#duplicate names of songs are to be removed


