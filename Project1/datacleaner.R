library(tidyverse)
library(rvest)
library(utils)

#########################################################################
# Fix the non-genre tags

#saved the data from this list of genres into a txt file
# https://gist.github.com/andytlr/4104c667a62d8145aa3a


#import the genres from the txt file and songData from csv

genresList <- readLines("genres.txt") %>% lapply(tolower)
songData <- read.csv("songData.csv") %>% unique()

tags <- songData$Tags

for (i in 1:length(tags)) {
  #for each set of tags split by the ";" (lowercased)
  rowTags <- strsplit(tags[i], ";")[[1]] %>% lapply(tolower)
  
  newRowTags <- character(0)
  
  #we iterate through the tags for each row and append the ones matching the list
  for (tag in rowTags) {
    if ((tag %in% genresList) && !(tag %in% newRowTags)) {
      newRowTags <- c(newRowTags, tag)
    }
  }
  
  stringOfTags = ""
  for (tag in newRowTags) {
    stringOfTags <- paste(stringOfTags, tag, sep = ";")
  }
  
  tags[i] <- stringOfTags %>% substr(2, nchar(stringOfTags))
}

#replace the tags in the csv
songData['Tags'] <- tags

#     Note:     No rows should be removed

#########################################################################
# ensure all songs have a proper song length



#     Note:     No rows should be removed

#########################################################################
#change all release dates to be year




#     Note:     No rows should be removed

#########################################################################
#duplicate names of songs are to be removed