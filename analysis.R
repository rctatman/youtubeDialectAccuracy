# Youtube dialect study

# library we'll need
library(ggplot2)

# Read in file
data <- read.csv("youtubeAutocaptinByDialect.txt", sep = "\t", header = T)
data

#plot
plot(data$Total, data$Correct, col = data$State)
plot(data$Correct, data$Total, col = data$Gen)
