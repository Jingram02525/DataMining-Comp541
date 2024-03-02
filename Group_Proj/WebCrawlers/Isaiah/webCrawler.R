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
  
  print("Checking albums ...")
  
  #progress bar creation
  min <- 0
  max <- length(albums)
  albumValidPB <- txtProgressBar(min, max, style = 3)  
  
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
    }, finally = {
      #update the progress bar
      setTxtProgressBar(albumValidPB, i)
    })
  }
  
  #Close the progress bar for valid albums
  close(albumValidPB)
  
  #print(goodAlbums)
  return (goodAlbums)
}

#gets all albums for a given artist
getAllAlbums <- function(artistName, numPages, artistHTML) {
  
  #instantiate the `albums` variable. this will be what we return
  albums <- character(0)
  
  #append new albums to `albums`
  if (numPages >= 1) {
    print("Getting albums ...")
    
    #progress bar creation
    min <- 0
    max <- length(numPages)
    pagesPB <- txtProgressBar(min, max, style = 3)  
    
    for (i in 1:numPages) {
      #make the link for each of the pages
      #uses this link to get albums from artist: https://www.last.fm/music/ARTIST/+albums?order=most_popular&page=
      albumHTML <- paste("https://www.last.fm/music/", artistName, sep = "")
      albumHTML <- paste(albumHTML, "/+albums?order=most_popular&page=", i, sep = "")
      
      albums <- c(albums, getAlbumsFromSinglePage(albumHTML))
      
      #update the progress bar
      setTxtProgressBar(pagesPB, i)
    }
    
    #close progress bar
    close(pagesPB)
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
  
  
  paste("Getting Song Information from all albums found for", artist, "...") %>%
  print()
  
  #progress bar creation
  min <- 0
  max <- length(albums)
  songInfoPB <- txtProgressBar(min, max, style = 3)  
  
  allSongInfo <- data.frame()
  for (albumNum in 1:length(albums)) {
    allSongInfo <- rbind(allSongInfo, getSongInfoFromAlbum(albums[albumNum], 
                                                           artist, artistHTML))
    
    #update the progress bar
    setTxtProgressBar(songInfoPB, albumNum)
  }
  
  #close progress bar
  close(songInfoPB)
  
  return (allSongInfo)
}

#gets the song info for a given artist
#uses this link to get relevant info from the artist: https://www.last.fm/music/
getSongInfoFromArtist <- function(artistName) {
  
  #URL encode the given artistName for web query
  artistNameWeb <- URLencode(artistName)
  
  #replace spaces (`%20`) with `+` character
  artistNameWeb <- gsub("%20", "+", artistNameWeb)
  
  #form the html links
  artistHTML <- paste("https://www.last.fm/music/", artistNameWeb, sep = "")
  artistAlbumsHTML <- paste(artistHTML, "/+albums?order=most_popular", sep = "")
  
  #find the number of pages to search for the albums
  numPages <- artistAlbumsHTML %>% read_html() %>%
    html_elements(".pagination-page") %>% html_text2()
  numPages <- numPages[length(numPages)]
  
  #if there is only one page, there will not be the desired html element
  #thus we default numPages to 1
  if (length(numPages) == 0) {
    numPages <- 1
  }
  
  #obtain all albums for the given artist
  albums <- getAllAlbums(artistNameWeb, numPages, artistHTML)
  
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
  
  return(songInfo)
}

#used to place given artist into a file
#used to place related artists in a file
#receives info for all songs for this given artist
getInfoFromArtist <- function(artistName) {
  
  #assert artistName is a string
  if (!(givenParamIsString(artistName))) {
    print("Given Artist Name is not valid. Please enter a valid string")
    paste("Given Artist Name `", artistName, "`", sep = "") %>% print()
    return (FALSE)
  }

  #check if the given artist's songs have already been datamined
  print("Checking if artist is already in snowball ...")
  snowballed <- readLines("snowballedArtistNames.txt")

  #assert the artistName is not already a part of the snowball
  if (artistName %in% snowballed) {
    paste(artistName, "was already in the snowball") %>% print()
    return (FALSE)
  }
  
  #write the given artistName into the snowball
  paste("Adding", artistName, "to snowball ...") %>% print()
  write(artistName, "snowballedArtistNames.txt", append = TRUE)

  #obtain the list of relatedArtists
  print("Checking related artists ...")
  relatedArtists <- getRelatedArtists(artistName)

  #obtain the list to be snowballed
  print("Adding related artists to snowball ...")
  toSnowball <- readLines("artistNamesToBeSnowballed.txt")
  
  #progress bar creation
  min <- 0
  max <- length(relatedArtists)
  relatedArtistPB <- txtProgressBar(min, max, style = 3)
  
  #add relatedArtists and update progress bar
  for (relatedArtistNum in 1:length(relatedArtists)) {
    if (!(relatedArtists[relatedArtistNum] %in% toSnowball)) {
      write(relatedArtists[relatedArtistNum], "artistNamesToBeSnowballed.txt", append = TRUE)
    }
    
    #update the progress bar
    setTxtProgressBar(relatedArtistPB, relatedArtistNum)
  }
  
  # Close the progress bar for related Artists
  close(relatedArtistPB)

  #obtain dataframe of the song data for the given artist
  paste("Obtaining Song Data for", artistName, "...") %>% print()
  newSongData <- getSongInfoFromArtist(artistName)

  #try to read data in csv file
  songDataFromCSV <- data.frame()
  result <- tryCatch({
    songDataFromCSV <- read.csv("songData.csv")
  }, error = function(e) {
    print("No saved data!")
  })

  #add the data to a csv file
  print("Adding song data to csv file ...")
  newSongData <- rbind(songDataFromCSV, newSongData)
  write.csv(newSongData, "songData.csv", row.names = FALSE)

  #returns true, indicating that the song info process has completed
  paste("Finished obtaining information for given artist:", artistName) %>% print()
  return(TRUE)
}

#begins web crawler process. looks at `artistNamesToBeSnowballed`
beginWebCrawler <- function() {
  
  #read snowball list
  toSnowball <- readLines("artistNamesToBeSnowballed.txt")
  
  #define a starting point for the snowball if list is empty
  if (length(toSnowball) == 0) {
    
    defaultArtist <- "(G)I-dle"
    
    print("Artists to be snowballed is empty!")
    paste("Default start with artist:", defaultArtist) %>% print()
    
    res <- getInfoFromArtist(defaultArtist)
    
    #if fail to get data from starting point
    if (!(res)) {
      print("Error with default start!")
      return(res)
    }
  } 
  
  #assert there is some data in snowball list
  toSnowball <- readLines("artistNamesToBeSnowballed.txt")
  while (length(toSnowball) != 0) {
    
    #grab the first element from the snowball list
    startPt <- toSnowball[1]
    
    #remove first element from snowball list
    toSnowball <- toSnowball[2:length(toSnowball)]
    
    #rewrite artists to be snowballed
    writeLines(toSnowball, "artistNamesToBeSnowballed.txt")
    
    #using starting point, obtain info
    res <- getInfoFromArtist(startPt)
    
    #on failure for artist
    if (!(res)) {
      print("Error with snowball propogation!")
      print(startPt)
      return(res)
    }
  }
  
  return (TRUE)
}

#run the web crawler
beginWebCrawler()