# Youtube dialect study

# library we'll need
library(ggplot2)
library(RCurl)
library(sm)
library(plyr)
library(plotly)

# Read in file (from copy on Github)
x <- getURL("https://raw.githubusercontent.com/rctatman/youtubeDialectAccuracy/master/youtubeAutocaptinByDialect.txt")
data <- read.csv(text = x, sep = "\t", header = T)
summary(data)

# percent of correct answers
percentCorr <- data$Correct/data$Total
data$percentCorr <- percentCorr

# median percent correct, by state and gender
medState <- aggregate(data[,6], list(data$State), FUN = median)
medGen <- aggregate(data[,6], list(data$Gen), FUN = median)

#density plots
# comaring classifiraction accuracy between men and women
sm.density.compare(percentCorr, data$Gen, xlim = c(0,1))
legend("topright", levels(data$Gen), col = c("red","green"), lty = c(1,2))
# between dialect regions
sm.density.compare(percentCorr, data$State, col = 1:5, xlim = c(0,1))
legend("topright", levels(data$State), col = 1:5, lty = 1:5)

# barplots
numberPerDialect <- 8
numberPerGender <- 20 
ggplot(data, aes(x = factor(State), y = percentCorr/numberPerDialect)) + geom_bar(stat = "identity") + ylim(0,1)
ggplot(data, aes(x = factor(Gen), y = percentCorr/numberPerGender)) + geom_bar(stat = "identity")+ ylim(0,1)

# seperate by gender
men <- data[data$Gen == "M",]
percentCorrMen <- men$Correct/men$Total

women <- data[data$Gen == "F",]
percentCorrWo <- women$Correct/women$Total

medStateWo <- cbind(aggregate(women[,6], list(women$State), FUN = median), Gen = "F")
medStateMen <- cbind(aggregate(men[,6], list(men$State), FUN = median), Gen = "M")
medStateGen <- rbind(medStateWo, medStateMen)
names(medStateGen) <- c("State", "PrecentCorr", "Gen")

# plot dialect area by gender
ggplot(men, aes(x = factor(State), y = percentCorrMen/(numberPerDialect/2))) + geom_bar(stat = "identity") + ylim(0,1)
ggplot(women, aes(x = factor(State), y = percentCorrWo/(numberPerDialect/2))) + geom_bar(stat = "identity") + ylim(0,1)
# compare gender within each dialect area
ggplot(medStateGen, aes(x = State, y = PrecentCorr, fill = Gen)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  ylim(0,1)
# differences between genders by dialect area
diff <- cbind(medStateGen[1:5,], diff = medStateGen[1:5,2] - medStateGen[6:10,2])
positions <- diff$State[sort.list(diff$diff)]
ggplot(data = diff, aes(x = State, y = diff)) + ylim(-1, 1) +
         geom_bar(stat = "identity") + scale_x_discrete(limits = positions)

#barplots of individuals
sortedData <- data[order(percentCorr),]
percentCorrSorted <- percentCorr[order(percentCorr)]
# by gender
barplot(percentCorrSorted, col = sortedData$Gen)
legend("topleft", levels(data$Gen), col = c("black","red"), lty = 1)
# by region
barplot(percentCorrSorted, col = sortedData$State)
legend("topleft", levels(data$State), col = c(1:5), lty = 1)

# is there a reliable differnce between men and women?
t.test(percentCorr[data$Gen== "F"], percentCorr[data$Gen== "M"])

# what about between dialect regions? 
summary(aov(formula = percentCorr ~ State, data = data))

# state and dialect region? 
summary(aov(formula = percentCorr ~ State + Gen, data = data))
