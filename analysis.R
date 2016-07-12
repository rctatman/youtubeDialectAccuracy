# Youtube dialect and study

# Data is the number of correct transcriptions and total words for the word-list
# portion of "accent challange" or "accent tag" (more info: 
# http://www.pri.org/stories/2015-07-21/accent-quiz-swept-english-speaking-world)
# videos. All transcriptions are taken from auto captions provided by YouTube,
# using Google's speech recogntion technology.

# libraries we'll need
library(ggplot2)
library(RCurl)
library(sm)
library(plyr)
library(lme4)
library(pwr)
library(effsize)

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
numberPerDialect <- 10
numberPerGender <- 25
ggplot(data, aes(x = factor(State), y = percentCorr/numberPerDialect)) + geom_bar(stat = "identity") + ylim(0,1)
ggplot(data, aes(x = factor(Gen), y = percentCorr/numberPerGender)) + geom_bar(stat = "identity") + ylim(0,1)

# prettied up boxplot for region
data$State <- factor(data$State, levels = c("California","NewZealand","Maine","Georgia","Scotland"))
ggplot(data, aes(x = factor(State), y = percentCorr, fill = State)) +
  geom_boxplot() + geom_point() + scale_fill_manual(values = c("#B22234","royalblue4","#B22234","#B22234","white")) +
  ylim(0,1) + xlab("") + ylab("Proportion of Correctly Recognized Words") +  guides(fill=FALSE) +
  theme(text = element_text(size=20))

# prettied up boxplot for gender
data$Gen <- mapvalues(data$Gen, c("F","M"), c("Women","Men"))
ggplot(data, aes(x = factor(Gen), y = percentCorr, fill = Gen)) +
  geom_boxplot() + geom_point() + #scale_fill_manual(values = c("#B22234","royalblue4","#B22234","#B22234","white")) +
  ylim(0,1) + xlab("") + ylab("Proportion of Correctly Recognized Words") +  guides(fill=FALSE) +
  theme(text = element_text(size=18))


# seperate by gender
men <- data[data$Gen == "Men",]
percentCorrMen <- men$Correct/men$Total
women <- data[data$Gen == "Women",]
percentCorrWo <- women$Correct/women$Total

medStateWo <- cbind(aggregate(women[,6], list(women$State), FUN = median), Gen = "Women")

medStateMen <- cbind(aggregate(men[,6], list(men$State), FUN = median), Gen = "Men")
medStateGen <- rbind(medStateWo, medStateMen)
names(medStateGen) <- c("State", "PrecentCorr", "Gen")

# plot dialect area by gender
ggplot(men, aes(x = factor(State), y = percentCorrMen/(numberPerDialect/2))) + geom_bar(stat = "identity") + ylim(0,1)
ggplot(women, aes(x = factor(State), y = percentCorrWo/(numberPerDialect/2))) + geom_bar(stat = "identity") + ylim(0,1)
# compare gender within each dialect area, fancy version 
ggplot(data = data, aes(x = data$State, y = data$percentCorr, fill = data$Gen)) +
  geom_boxplot() +  
  geom_point(aes(x = data$State, y = data$percentCorr), position = position_dodge(width = .75))+
  ylim(0,1) + xlab("") + ylab("Proportion of Correctly Recognized Words") + scale_shape(solid = FALSE) +
  theme(text = element_text(size=18), legend.position = c(.9, .8), legend.title = element_blank()) 
# compare gender within each dialect area, simple version 
ggplot(data = medStateGen, aes(x = State, y = PrecentCorr, fill = Gen)) +
  geom_bar(stat = "identity", position = "dodge") +  ylim(0,1) + xlab("") + 
  ylab("Proportion of Correctly Recognized Words") + scale_shape(solid = FALSE) +
  theme(text = element_text(size=18), legend.position = c(.9, .8), legend.title = element_blank()) 


# differences between genders by dialect area
diff <- cbind(medStateGen[1:5,], diff = medStateGen[1:5,2] - medStateGen[6:10,2])
positions <- diff$State[sort.list(diff$diff)]
ggplot(data = diff, aes(x = State, y = diff)) + ylim(-1, 1) +
         geom_bar(stat = "identity") + scale_x_discrete(limits = positions)

#barplots of individuals
sortedData <- data[order(percentCorr),]
percentCorrSorted <- percentCorr[order(percentCorr)]
# colored by gender
barplot(percentCorrSorted, col = sortedData$Gen)
legend("topleft", levels(data$Gen), col = c("black","red"), lty = 1)
# colored by region
barplot(percentCorrSorted, col = sortedData$State)
legend("topleft", levels(data$State), col = c(1:5), lty = 1)

# is there a reliable differnce between men and women?
t.test(percentCorr[data$Gen== "Women"], percentCorr[data$Gen== "Men"])
# yes. very much so. Effect size: 
cohen.d(data$percentCorr, data$Gen)

# what about between dialect regions? 
summary(aov(formula = percentCorr ~ State, data = data))
# no significant difference
# power analysis -- need more data 
pwr.anova.test(k=5,f=.25,sig.level=.05,power=.8)

# no interaction  of dialct and gender 
summary(aov(formula = percentCorr ~ State + Gen, data = data))

# lienar models
model1 <- lm(data$Correct ~ data$Total + data$Gen)
model2 <- lm(data$Correct ~ data$Total + data$State + data$Gen)
anova(model1, model2)

# only for women
model1 <- lm(women$Correct ~ women$Total)
model2 <- lm(women$Correct ~ women$Total + women$State)
anova(model1, model2)
# only for men
model1 <- lm(men$Correct ~ men$Total)
model2 <- lm(men$Correct ~ men$Total + men$State)
anova(model1, model2)
# so dialect area is more important for men than women