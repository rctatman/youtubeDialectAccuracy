# Youtube dialect study

# library we'll need
library(ggplot2)
library(RCurl)
library(sm)

# Read in file (from copy on Github)
x <- getURL("https://raw.githubusercontent.com/rctatman/youtubeDialectAccuracy/master/youtubeAutocaptinByDialect.txt")
data <- read.csv(text = x, sep = "\t", header = T)
data

#density plots
# comaring classifiraction accuracy between men and women
sm.density.compare(percentCorr, data$Gen, xlim = c(0,1))
legend("topright", levels(data$Gen), col = c("red","green"), lty = c(1,2))
# between dialect regions
sm.density.compare(percentCorr, data$State, col = 1:5, xlim = c(0,1))
legend("topright", levels(data$State), col = 1:5, lty = 1:5)

# barplots
numberPerDialect <- 4
numberPerGender <- 10 
ggplot(data, aes(x = factor(State), y = percentCorr/numberPerDialect)) + geom_bar(stat = "identity")
ggplot(data, aes(x = factor(Gen), y = percentCorr/numberPerGender)) + geom_bar(stat = "identity")


# is there a reliable differnce between men and women?
t.test(percentCorr[data$Gen== "F"], percentCorr[data$Gen== "M"])

# what about between dialect regions? 
summary(aov(formula = percentCorr ~ State, data = data))

# state and dialect region? 
summary(aov(formula = percentCorr ~ State + Gen, data = data))
