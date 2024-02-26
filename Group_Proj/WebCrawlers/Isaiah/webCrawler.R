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
#uses this link to get a list of related artists for the snowball:
#https://www.music-map.com/
getRelatedArtists <- function(artistName) {
  
  #URL encode the given artistName for web query
  artistName <- URLencode(artistName)
  
  #form the html link
  relatedArtistsHTML <- paste("https://www.music-map.com/", artistName, sep = "") 
  
  #obtain the webpage and extract the related artists from the webpage
  relatedArtistsList <- relatedArtistsHTML %>% read_html() %>%
                        html_elements(".S") %>% html_text2()
  
  #CHECK IF THERE IS ANY VALUES
  #IF THERE ARE, RETURN THEM. O/W RETURN ERROR AND ADD THEM TO SEPARATE LIST
  
  #exclude the given artist name in the list and return the list
  return (relatedArtistsList[2:length(relatedArtistsList)])
}

#gets all the albums for a given artist from a single page containing their
#albums. uses this link to get albums from artist:
#https://www.last.fm/music/ARTIST/+albums?order=most_popular&page=PAGENUMBER
getAlbums <- function(artistAlbumsHTML) {
  
  #obtain the webpage and extract the albums from the webpage
  albumsList <- artistAlbumsHTML %>% read_html() %>% 
    html_elements("h3") %>% html_text2()
  
  #obtain the list of albums
  albumsList <- albumsList[((which(albumsList %in% "All Albums")[1]) + 1) : 
                             ((which(albumsList %in% "Similar Artists")[1]) - 1)]
  
  # print("Albums")
  # print(albumsList)
  
  return (albumsList)
}

#gets all albums for a given artist
getAllAlbums <- function(artistName, numPages) {
  
  #instantiate the `albums` variable. this will be what we return
  albums <- character(0)
  
  #append new albums to `albums`
  if (numPages >= 1) {
    for (i in 1:numPages) {
      #make the link for each of the pages
      #uses this link to get albums from artist: https://www.last.fm/music/ARTIST/+albums?order=most_popular&page=
      albumHTML <- paste("https://www.last.fm/music/", artistName, sep = "")
      albumHTML <- paste(albumHTML, "/+albums?order=most_popular&page=", i, sep = "")
      
      albums <- c(albums, getAlbums(albumHTML))
    }  
  }
  
  return (albums)
}

# songExtraction <- function(artistName, albumName) {
#   
# }

#gets the song info for a given artist
#uses this link to get relevant info from the artist: https://www.last.fm/music/
getSongInfoFromArtist <- function(artistName) {
  
  #URL encode the given artistName for web query
  artistName <- URLencode(artistName)
  
  #replace spaces (`%20`) with `+` character
  artistName <- gsub("%20", "+", artistName)
  
  #form the html links
  artistHTML <- paste("https://www.last.fm/music/", artistName, sep = "")
  artistAlbumsHTML <- paste(artistHTML, "/+albums?order=most_popular", sep = "")
  
  #find the number of pages to search for the albums
  numPages <- artistAlbumsHTML %>% read_html() %>% 
              html_elements(".pagination-page") %>% html_text2()
  numPages <- numPages[length(numPages)]
  
  #we have the number of pages
  # print("Pages")
  # print(numPages)
  
  #obtain all albums for the given artist
  albums <- getAllAlbums(artistName, numPages)
  print(albums)
  
  #for each album, extract the songs from the album
  for (i in 1:length(albums)) {
    #create a new url to extract the songs from each album
    albumURLName <- gsub("/", "%2F", albums[i])
    albumURL <- paste(artistHTML, "/", albumURLName, sep = "") %>%
                URLencode()
    albumURL <- gsub(" ", "+", albumURL)
    albumURL <- gsub("%20", "+", albumURL)
    
    #new url for albums to obtain songs info
    #print(albumURL)
    
    #check if the album has a tracklist
    
    result <- tryCatch({
      albumURLPage <- albumURL %>% read_html() %>% 
        html_elements("h3") %>% html_text2()
      
      if (!("Tracklist" %in% albumURLPage)) {
        print(i)
        print(albumURL)
        print(albums[i])
      }
    }, error = function(e) {
      print("error with given html")
      print(i)
      print(albumURL)
      print(albums[i])
    })
    
    
    
    
  }
 
  # #print(albums[78])
  # albumURL <- paste(artistHTML, "/", albums[78], sep = "")
  # albumURL <- gsub(" ", "+", albumURL)
  # print(albumURL)
  # albumURLPage <- albumURL %>% read_html() %>% 
  #   html_elements("h3") %>% html_text2()
  # print(albumURLPage)
  # 
  # 
  # #print(albums[87])
  # albumURL <- paste(artistHTML, "/", albums[87], sep = "")
  # albumURL <- gsub(" ", "+", albumURL)
  # print(albumURL)
  # albumURLPage <- albumURL %>% read_html() %>% 
  #   html_elements("h3") %>% html_text2()
  # print(albumURLPage)
  # 
  
  # 
  # artistTagsHTML <- paste(artistHTML, "/+tags", sep = "")
  # 
  # #obtain the tags for the artist 
  # tagsList <- artistTagsHTML %>% read_html() %>% 
  #   html_elements("h3") %>% html_text2()
  # 
  # #skip the first element and removes the elements after the first 
  # #"similar artists" section
  # tagsList <- tagsList[2 : ((which(tagsList %in% "Similar Artists")[1]) - 1)]
  # 
  #we now have the tags!
  # print("Tags")
  # print(tagsList)
}

getSongInfoFromArtist("(G)I-dle")
#getSongInfoFromArtist("Kanye West")

#used to place given artist into a file
#used to place related artists in a file
#receives info for all songs for this given artist
# getInfoFromArtist <- function(artistName) {
#   
#   #assert artistName is a string
#   if (!(givenParamIsString(artistName))) {
#     return ("Given Artist Name is not valid. Please enter a valid string")
#   }
#   
#   #check if the given name has already been read
#   snowballed <- readLines("snowballedArtistNames.txt")
#   
#   #assert the artistName is not already a part of the snowball
#   if (artistName %in% snowballed) {
#     paste(artistName, "was already in the snowball") %>% print()
#     return ()
#   }
# 
#   #write the current artistName into the snowball
#   writeLines(artistName, "snowballedArtistNames.txt")
#   
#   #obtain the list of relatedArtists
#   relatedArtists <- getRelatedArtists("Muse")
#   
#   #obtain the list to be snowballed
#   toSnowball <- readLines("artistNamesToBeSnowballed.txt")
#   
#   #for each related artist, add if they are not in the to be snowballed list
#   for (possibleSnowball in relatedArtists) {
#     if (!(possibleSnowball %in% toSnowball)) {
#       writeLines(possibleSnowball, "artistNamesToBeSnowballed.txt")
#     }
#   }
#   
#   #obtain the song data for the given artist
#   
#   
#   
#   
# }
#
# 
# 
# 
# data <- getRelatedArtists("Kanye West")
# 
# writeLines(data, "artistNamesToBeSnowballed.txt")
# 
# newdata <- readLines("artistNamesToBeSnowballed.txt")
# 
# data <- getRelatedArtists("Drake")
# 
# 