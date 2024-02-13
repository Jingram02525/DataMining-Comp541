#reads data from the csv
data <- read.csv("data.csv")

#NOTE: the column `%fat` has been changed to `X.fat` due to R mis-reading 
#the csv file

#######     Part a)     #######
#   calculate:
#     mean
#     median
#     std
#   for `age` and `%fat`

#mean
cat("Mean of Age column is:", mean(data$age))
cat("Mean of % Fat column is:", mean(data$X.fat))

#median
cat("Median of Age column is:", median(data$age))
cat("Median of % Fat column is:", median(data$X.fat))

#std
cat("Standard Deviation of Age column is:", sd(data$age))
cat("Standard Deviation of % Fat column is:", sd(data$X.fat))

#######     Part b)     #######
#   Plot the boxplots for `age` and `%fat`
#I went ahead and saved the plots as png images

#boxplot for `age`
png("Age_Boxplot.png", width = 800, height = 600, res = 200)
boxplot(data$age, horizontal = TRUE, xlab = "Age (years)")
title("Age Boxplot")
dev.off()

#boxplot for `%fat`
png("PercentFat_Boxplot.png", width = 800, height = 600, res = 200)
boxplot(data$X.fat, horizontal = TRUE, xlab = "Percentage")
title("% Fat Boxplot")
dev.off()

#######     Part c)     #######
#   Plot a scatter plot and a q-q plot for `age` and `%fat`
#I also went ahead and saved these plots as png files as well

#scatterplot
png("ScatterplotAgeVSPercentFat.png", width = 800, height = 800, res = 200)
plot(data$age, data$X.fat, xlab = "Age", ylab = "% Fat", 
     main = "Scatterplot of Age vs % Fat")
dev.off()

#Q-Q plot
png("QQAgeVSPercentFat.png", width = 800, height = 800, res = 200)
qqplot(data$age, data$X.fat, main = "Q-Q Plot of Age vs % Fat", 
       xlab = "Age", ylab = "% Fat")
dev.off()