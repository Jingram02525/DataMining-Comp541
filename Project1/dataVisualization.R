library(tidyverse)
library(ggplot2)
library(dplyr)
library(arrow)

#base R
songData <- read.csv("songData.csv") %>% unique()

artistCounts <- table(songData$"Artist")

top10 <- head(sort(artistCounts, decreasing = TRUE), n = 6)

par(oma = c(0,4,0,0))

barplot(top10, main = "Top 10 Artists by Song Count", 
        xlab = "Number of Songs",
        las = 2,
        horiz = TRUE)



#ggplot2
top10df <- as.data.frame(top10)

ggsave("top6.png", dpi=400)

plot <- ggplot(top10df, aes(y = Var1, x = Freq, fill = Freq)) + 
        geom_bar(position = "dodge", stat = "identity") + 
        scale_fill_gradient2() + 
        labs(title = "Top 6 Artists by Song Count", y = "Artist", x = "Number of Songs")

dev.off()

# plot <- ggplot(top10df, aes(x = Var1, y = Freq), fill = c("gray98", "gray88", "gray78", "gray68", "gray58", "gray48"))+#c("red", "blue", "green", "grey", "black", "pink"))) + 
#         geom_bar(stat = "identity", position = "dodge") +
#         #scale_fill_viridis_c() + 
#         #scale_fill_grey(start = 0.25, end = 0.75) + 
#         #scale_fill_manual()
#         labs(title = "Top 10 Artists by Song Count", x = "Artist", y = "Number of Songs")

print(plot)

artists <- unique(songData$"Artist")
print(artists)
view(artists)


# Read the data from parquet files (holds user relational data)


