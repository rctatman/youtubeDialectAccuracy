# Youtube dialect study

# library we'll need
library(ggplot2)

# Read in file
data <- read.csv("D:/Dropbox/miscPersonal/youtubeAutocaptinByDialect.txt", sep = "\t", header = T, row.names = NULL)
data

#plot
plot(data$Total, data$Correct, col = data$State)
plot(data$Correct, data$Total, col = data$Gen)
