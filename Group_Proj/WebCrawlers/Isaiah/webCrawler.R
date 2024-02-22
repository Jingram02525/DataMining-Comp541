library(tidyverse)
library(rvest)

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