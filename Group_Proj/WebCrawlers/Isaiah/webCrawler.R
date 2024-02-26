library(tidyverse)
library(rvest)
library(utils)

learningRVEST <- function() {
  starwars <- read_html("https://rvest.tidyverse.org/articles/starwars.html")
  
  #link is a list containing 2 elements
  #   head
  #   body
  
  #we will now read the part of the page that we want
  #by using html_elements()
  
  #we will use section to find all the sections of the page
  films <- starwars %>% html_elements("section")
  films
  
  #films now contains all of the different sections data
  
  #now we will extract the titles
  title <- films %>% html_element("h2") %>% html_text2()
  title
  
  #alternatively can use html_attr to extract attributes from website
  #html_attr always returns a string
  episode <- films %>% html_element("h2") %>% html_attr("data-id") %>% readr::parse_integer()
  episode
  
  #if the page has tabular data you can extract it into a dataframe directly with html_table()
  
  html <- read_html("https://en.wikipedia.org/w/index.php?title=The_Lego_Movie&oldid=998422565")
  
  html %>% html_element(".tracklist") %>% html_table()
}

# Utilizing Google Chrome in MacOS

# use this link to get a list of related artists for the snowball: https://www.music-map.com/

# use this link to get information on songs: https://www.last.fm/home

# The main premise for how the webcrawler will operate is as follows:
#   1) Start by manually inputting a music group or artist into the function
#       This will obtain and output list of related artists.
#   2) Save this list of artists to a larger txt file containing all of the artists that have been listed.
#       This file will only contain unique artists.
#       It will not be sorted either.
#       This is to show the order in which the data was accumulated.
#   3) Given the group or artist, insert them into a different txt file containing all artists that have had song data extracted.
#   4) The second function will take the given artist, search them on last.fm follow a sequence of steps:
#       1) Search the artist
#       2) Find all albums and insert it into an stack
#       3) Look at each album and insert its songs into an queue
#       4) Grab the music data related to each song and store it into a csv file
#       5) remove the song from the queue
#       6) when the queue is empty, pop the current album off the stack
#       7) repeat until stack is empty
#   5) Store the current data so that the process can be stopped at any time with saved data of point in time.

#if the given parameter is a string return true. False o/w
givenParamIsString <- function(argument) {
  if (typeof(argument) == "character") { return (TRUE) }
  return (FALSE)
}

#gets the related artists for a given Artist
getRelatedArtists <- function(artistName) {
  
  #assert artistName is a string
  if (!(givenParamIsString(artistName))) {
    return ("Given Artist Name is not valid. Please enter a string")
  }
  
  #URL encode the given artistName for web query
  artistName <- URLencode(artistName)
  
  #form the html link
  relatedArtistsHTML <- paste("https://www.music-map.com/", artistName, sep = "") 
  
  #obtain the webpage and extract the related artists from the webpage
  relatedArtistsList <- relatedArtistsHTML %>% read_html() %>%
                        html_elements(".S") %>% html_text2()
  
  #exclude the given artist name in the list and return the list
  return (relatedArtistsList[2:length(relatedArtistsList)])
}

data <- getRelatedArtists("Muse")

data <- getRelatedArtists("Kanye West")

writeLines(data, "artistNames.txt")







