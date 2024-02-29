library(tidyverse)
library(rvest)
library(utils)

#if the given parameter is a string return true. False o/w
givenParamIsString <- function(argument) {
  if (typeof(argument) == "character") { return (TRUE) }
  return (FALSE)
}

#gets the related artists for a given Artist
#uses this link to get a list of related artists for the snowball:
#https://www.music-map.com/
#CHECK EDGE CASE!
getRelatedArtists <- function(artistName) {
  
  #URL encode the given artistName for web query
  artistName <- URLencode(artistName)
  
  #form the html link
  relatedArtistsHTML <- paste("https://www.music-map.com/", artistName, sep = "") 
  
  #obtain the webpage and extract the related artists from the webpage
  relatedArtistsList <- relatedArtistsHTML %>% read_html() %>%
    html_elements(".S") %>% html_text2()
  
  #CHECK IF THERE ARE ANY VALUES
  #IF THERE ARE, RETURN THEM. O/W RETURN ERROR AND ADD THEM TO SEPARATE LIST
  
  #exclude the given artist name in the list and return the list
  return (relatedArtistsList[2:length(relatedArtistsList)])
}

#gets all the albums for a given artist from a single page containing their
#albums. uses this link to get albums from artist:
#https://www.last.fm/music/ARTIST/+albums?order=most_popular&page=PAGENUMBER
getAlbumsFromSinglePage <- function(artistAlbumsHTML) {
  
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

#gets "useful" albums (albums with nonempty tracklists)
getUsefulAlbums <- function(albums, artistHTML) {
  
  #will be the albums we can get songs from
  goodAlbums <- character(0)
  
  #for each album, extract the songs from the album
  for (i in 1:length(albums)) {
    #create a new url to extract the songs from each album
    albumURLName <- gsub("/", "%2F", albums[i])
    albumURL <- paste(artistHTML, "/", albumURLName, sep = "") %>%
      URLencode()
    albumURL <- gsub(" |%20", "+", albumURL)
    
    #new url for albums to obtain songs info
    #print(albumURL)
    
    #check if the album has a tracklist and add it to goodAlbums if so
    result <- tryCatch({
      albumURLPage <- albumURL %>% read_html() %>% 
        html_elements("h3") %>% html_text2()
      
      if ("Tracklist" %in% albumURLPage) {
        goodAlbums <- c(goodAlbums, albums[i])
      }
    }, error = function(e) {
      # print("error with given html")
      # print(i)
      # print(albums[i])
    })
  }
  
  #print(goodAlbums)
  return (goodAlbums)
}

#gets all albums for a given artist
getAllAlbums <- function(artistName, numPages, artistHTML) {
  
  #instantiate the `albums` variable. this will be what we return
  albums <- character(0)
  
  #append new albums to `albums`
  if (numPages >= 1) {
    for (i in 1:numPages) {
      #make the link for each of the pages
      #uses this link to get albums from artist: https://www.last.fm/music/ARTIST/+albums?order=most_popular&page=
      albumHTML <- paste("https://www.last.fm/music/", artistName, sep = "")
      albumHTML <- paste(albumHTML, "/+albums?order=most_popular&page=", i, sep = "")
      
      albums <- c(albums, getAlbumsFromSinglePage(albumHTML))
    }  
  }
  
  #albums now holds all albums from lastfm
  #we will reduce the size of albums by removing the albums without tracklists
  albums <- getUsefulAlbums(albums, artistHTML)
  
  return (albums)
}

#changes format of tags from character array to a single string
changeTagsFormat <- function(tags) {
  
  stringOfTags = ""
  for (tag in tags) {
    stringOfTags <- paste(stringOfTags, tag, sep = ";")
  }
  
  stringOfTags <- substr(stringOfTags, 2, nchar(stringOfTags))
  
  return (stringOfTags)
}

#gets song info from a single album
#returns a character vector containing the info for all songs on the album
getSongInfoFromAlbum <- function(album, artist, artistHTML) {
  
  #make link for album
  albumURLName <- gsub("/", "%2F", album)
  albumURL <- paste(artistHTML, "/", albumURLName, sep = "") %>%
    URLencode()
  albumURL <- gsub(" ", "+", albumURL)
  albumURL <- gsub("%20", "+", albumURL)
  
  #get all song links
  songLinks <- albumURL %>% read_html() %>% html_elements(".chartlist-name") %>%
    html_nodes("a") %>% html_attr("href")
  songLinks <- paste("https://last.fm", songLinks, sep = "")
  
  songInfoFromAlbum <- data.frame()
  
  #extract info from each songlink
  for (songLink in songLinks) {
    #if the tags exist for the song:
    #   extract info and enter into to char vector
    
    #go to the link for the song and check if it has tags
    tagLink <- songLink %>% read_html() %>% html_elements(".tags-view-all")
    
    #if the link to the tags for the song exists then retrieve them
    if (length(tagLink) != 0) {
      
      #form the link to extract tags
      tagLink <- tagLink %>% html_attr("href")
      tagLink <- paste("https://last.fm", tagLink, sep = "")
      
      #read page and extract tags
      tagsInfo <- tagLink %>% read_html() %>% 
        html_elements(".big-tags-item-name") %>% html_text2()
      tagsInfo <- changeTagsFormat(tagsInfo)
      
      #now we want to grab all the other song info:
      # song name
      songName <- songLink %>% read_html() %>% html_elements("h1") %>% 
        html_text2()
      
      # song length
      songLength <- songLink %>% read_html() %>% 
        html_element(".catalogue-metadata-description") %>% 
        html_text2()
      
      # album is already obtained
      # artist is already obtained
      
      # date released (can be found on album page)
      songReleaseDate <- albumURL %>% read_html() %>% 
        html_elements(".catalogue-metadata-description") %>% 
        html_text2()
      
      #create a single dataframe with 1 row of data
      songInfo <- data.frame("Song Name" = songName, 
                             "Song Length" = songLength,
                             "Album" = album,
                             "Artist" = artist,
                             "Date Released" = songReleaseDate[2],
                             "Tags" = tagsInfo)
      
      #append new row of data for the current song to the songInfo from the album
      songInfoFromAlbum <- rbind(songInfoFromAlbum, songInfo)
    }
    #done looking at current song
  }
  
  #return dataframe of all info obtained from this album
  return (songInfoFromAlbum)
}

#returns table of song info from all albums
getAllSongInfo <- function(albums, artist, artistHTML) {
  
  allSongInfo <- data.frame()
  for (album in albums) {
    allSongInfo <- rbind(allSongInfo, getSongInfoFromAlbum(album, artist, artistHTML))
  }
  
  return (allSongInfo)
}

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
  
  #obtain all albums for the given artist
  albums <- getAllAlbums(artistName, numPages, artistHTML)
  
  #get all songInfo from the albums
  songInfo <- getAllSongInfo(albums, artistName, artistHTML)
  
  #tag link from the artist
  artistTagsHTML <- paste(artistHTML, "/+tags", sep = "")
  
  #obtain the tags for the artist
  tagsList <- artistTagsHTML %>% read_html() %>%
    html_elements("h3") %>% html_text2()
  
  #skip the first element and removes the elements after the first
  #"similar artists" section
  tagsList <- tagsList[2 : ((which(tagsList %in% "Similar Artists")[1]) - 1)]
  
  #update the format of the tagsList
  tagsList <- changeTagsFormat(tagsList)
  
  #attach the tags to all of the songs within the dataframe
  songInfo[["Tags"]] <- paste(songInfo[["Tags"]], tagsList, sep = ";")
  
  print(songInfo)
  view(songInfo)
  
  return(songInfo)
}

#used to place given artist into a file
#used to place related artists in a file
#receives info for all songs for this given artist
getInfoFromArtist <- function(artistName) {
  
  #assert artistName is a string
  if (!(givenParamIsString(artistName))) {
    return ("Given Artist Name is not valid. Please enter a valid string")
  }
  
  #check if the given name has already been read
  snowballed <- readLines("snowballedArtistNames.txt")
  
  #assert the artistName is not already a part of the snowball
  if (artistName %in% snowballed) {
    paste(artistName, "was already in the snowball") %>% print()
    return ()
  }
  
  #write the current artistName into the snowball
  writeLines(artistName, "snowballedArtistNames.txt")
  
  #obtain the list of relatedArtists
  relatedArtists <- getRelatedArtists("Muse")
  
  #obtain the list to be snowballed
  toSnowball <- readLines("artistNamesToBeSnowballed.txt")
  
  #for each related artist, add if they are not in the to be snowballed list
  for (possibleSnowball in relatedArtists) {
    if (!(possibleSnowball %in% toSnowball)) {
      writeLines(possibleSnowball, "artistNamesToBeSnowballed.txt")
    }
  }
  
  #obtain the song data for the given artist
  getSongInfoFromArtist(artistName)
  
  
}

#getSongInfoFromArtist("Kanye West")
getSongInfoFromArtist("(G)I-dle")
