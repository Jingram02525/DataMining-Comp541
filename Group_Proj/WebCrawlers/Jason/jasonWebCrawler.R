library(rvest)
library(tidyverse)
library(utils)

url <- "https://www.allmusic.com/genres"


genres <- url %>% 
  read_html() %>%
  html_elements(".meta") %>% 
  html_text2()

#print(genres)

newGenres <- url %>%
  read_html() %>% 
  html_elements(".meta") %>%
  html_nodes("a") %>% html_attr("href")

#print(newGenres)
#newGenres <- paste("https://www.allmusic.com", newGenres, sep = "")
#print(newGenres)
#newGenres <- url %>% read_html() %>%  html_elements(".meta") %>% html_nodes("a") %>% html_attr("title")
#print(newGenres)
#get all genres
newUrl <- paste("https://www.allmusic.com", newGenres, sep = "")

print(newUrl)

subGenreUrls <- character(0)

for(url in newUrl){
  subGenres <- url %>%
    read_html() %>%
    html_elements(".genre-links") %>%
    html_text2()
  
  subGenreLinks <- url %>%
    read_html() %>% 
    html_elements(".styles") %>%
    html_nodes("a") %>% html_attr("href")
  subGenreUrls <- c(subGenreUrls, subGenreLinks)
  
}

print(subGenreUrls)


subGenreLinks <- paste("https://www.allmusic.com", subGenreUrls, sep = "")
print(subGenreLinks)


#get all sub-genres links
#create a list of all links
listOfLinksForAllGenresAndSubGenres <- character(0)
listOfLinksForAllGenresAndSubGenres <- c(newUrl, subGenreLinks)
print(listOfLinksForAllGenresAndSubGenres)

#switch to songs for either genre or sub-genre
songListLinks <- paste(listOfLinksForAllGenresAndSubGenres, "/songs", sep = "")
print(songListLinks)


#grab links to song data
songLinks <- character(0)
for(link in songListLinks){
  
    
  
  songs <- link %>%
    read_html() %>% 
    html_elements(".songTitle") %>%
    html_nodes("a") %>% 
    html_attr("href")
  
  songLinks <- c(songLinks, songs)
}
print(songLinks)
#go to link to extract song data

for (songLink in songLinks){
  songName <- songLink %>%
    read_html() %>%
    html_elements(".meta") %>%
    html_text2()
  
  songArtist <- songLink %>%
    read_html() %>%
    html_elements(".meta") %>%
    html_text2()
  
  songData <- songLink %>%
    read_html() %>%
    html_elements(".meta") %>%
    html_text2()
}

SongDataFromCSV <- data.frame()
result <- tryCatch() 

