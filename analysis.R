# Youtube dialect study

# library we'll need
library(ggplot2)
library(RCurl)
library(sm)

# Read in file (from copy on Github)
x <- getURL("https://raw.githubusercontent.com/rctatman/youtubeDialectAccuracy/master/youtubeAutocaptinByDialect.txt")
data <- read.csv(text = x, sep = "\t", header = T)
data

#plot
plot(data$Total, data$Correct, col = data$State)
plot(data$Correct, data$Total, col = data$Gen)

percentCorr <- data$Correct/data$Total
plot(density(percentCorr))

plot(density(percentCorr[data$Gen== "F"]))
plot(density(percentCorr[data$Gen== "M"]), add = T)

sm.density.compare(percentCorr, data$Gen)
legend(locator(), levels(data$Gen), col = c("red","green"))
sm.density.compare(percentCorr, data$State)

# linear model
percentCorr <- data$Correct/data$Total
fit <- lm(percentCorr ~ Gen, data = data)
plot(fit)


ggplot(data, aes(x = factor(State), y = percentCorr*100)) + geom_bar(stat = "identity")
ggplot(data, aes(x = factor(Gen), y = percentCorr)) + geom_bar(stat = "identity")


ggplot(data, aes(x = percentCorr)) + geom_bar(stat = "identity") 


interplot(fit, var1 = "Correct")
interplot(fit, var1 = "percentCorr", var2 = "Gen")

# is there a reliable differnce between men and women?
t.test(percentCorr[data$Gen== "F"], percentCorr[data$Gen== "M"])
