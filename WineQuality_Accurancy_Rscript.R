---
title: "WineQuality"
author: "Seshan Senga"
date: "6/12/2020"
output:
  pdf_document: default
  html_document: default
  word_document: default
---

library(ggplot2)# Create Elegant Data Visualisations 
library(caret) # For model tuning 
library(corrgram) # For correlation matrix
library(corrplot)# For graphical display of correlation matrix
library(gridExtra) # For multi-object layouts
library(car)# For smoother scatterplots
library(MASS) # For MASS function 
library(GGally) #extension of ggplot2
library(dplyr) # For select and mutate function

#Importing the data of the red wine for the first set of analysis

wine = read.csv("D:/Harvard/winequality-red.csv", sep = ";", header = T)

#data can be imported from local machine if the local machine's antivirus setting doesn't per to import from url of the repository 

#viewing structure of the data and summary
str(wine)

#check the names of the variables present in the data.
colnames(wine)

#summary to better understand the dataset
summary(wine)

#Removing the Duplicate Rows
wine <- wine[!duplicated(wine), ]
dim(wine)
str(wine)
summary(wine)

#Check for NA in dataset
sum(is.na(wine))

#Response count in the dataset
table(wine$quality)


#Plotting the histograms using hist() for the data.

#plot comparison of "QUALITY", "FIXED ACIDITY", "VOLATILE ACIDITY", "CITRIC ACID"

attach(wine)

par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)
barplot((table(quality)), col=c("slateblue4", "slategray", "slategray1", "slategray2", "slategray3", "skyblue4"))
mtext("Quality", side=1, outer=F, line=2, cex=0.8)


truehist(fixed.acidity, h = 0.5, col="slategray3")
mtext("Fixed Acidity", side=1, outer=F, line=2, cex=0.8)

truehist(volatile.acidity, h = 0.05, col="slategray3")
mtext("Volatile Acidity", side=1, outer=F, line=2, cex=0.8)

truehist(citric.acid, h = 0.1, col="slategray3")
mtext("Citric Acid", side=1, outer=F, line=2, cex=0.8)

#PLOT COMPARISON OF "RESIDUAL SUGAR", "CHLORIDE", "ALCHOL", "DENSITY"

par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)

truehist(residual.sugar, h = 5, col="slategray3")
mtext("Residual Sugar", side=1, outer=F, line=2, cex=0.8)

truehist(chlorides, h = 0.01, col="slategray3")
mtext("Chloride", side=1, outer=F, line=2, cex=0.8)

truehist(alcohol, h = 0.5, col="slategray3")
mtext("Alcohol", side=1, outer=F, line=2, cex=0.8)


truehist(density, h = 0.005, col="slategray3")
mtext("Density", side=1, outer=F, line=2, cex=0.8)

#PLOT COMPARISON OF "FREE SULFUR DIOXIDE", "pH VALUES", "SULPHATES", "TOTAL.SULFUR.DIOXIDE"

par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)

truehist(free.sulfur.dioxide, h = 10, col="slategray3")
mtext("Free Sulfur Dioxide", side=1, outer=F, line=2, cex=0.8)

truehist(pH, h = 0.1, col="slategray3")
mtext("pH values", side=1, outer=F, line=2, cex=0.8)

truehist(sulphates, h = 0.1, col="slategray3")
mtext("sulphates", side=1, outer=F, line=2, cex=0.8)


truehist(total.sulfur.dioxide, h = 20, col="slategray3")
mtext("total.sulfur.dioxide", side=1, outer=F, line=2, cex=0.8)

#Relationship between each Attribute and Quality (Rating)

red_wine <- melt(wine, "quality")
ggplot(red_wine, aes(value, quality, color = variable)) +  
  geom_point() + 
  geom_smooth(aes(value,quality, colour=variable), method=lm, se=FALSE)+
  facet_wrap(.~variable, scales = "free")

#Collinearity between Attributes

par(mfrow = c(1,1))
cor.wine <- cor(wine)
corrplot(cor.wine, method = 'number')


# Storing quality in factor form

#wine$quality_as_factor = factor(wine$quality, levels = #c(0,1,2,3,4,5,6,7,8,9,10))
#summary(wine)

wine$quality <- as.factor(wine$quality)
str(wine)
#wine$quality_as_factor = NULL

#setting the seed value to 1
set.seed(1)

#splitting the dataset into train and test (2/3rd for train remaining for test)

inTrain <- createDataPartition(wine$quality, p = 2/3, list = F)
train <- wine[inTrain,]
test <- wine[-inTrain,]

#K-nearest neighbour model

t.ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
kknn.grid <- expand.grid(kmax = c(3, 5, 7 ,9, 11), distance = c(1, 2),
                         kernel = c("rectangular", "gaussian", "cos"))
kknn.train <- train(quality ~ ., data = train, method = "kknn",
                    trControl = t.ctrl, tuneGrid = kknn.grid,
                    preProcess = c("center", "scale"))

save(kknn.train, file = "kknn.train.rda")

# saveRDS(model, "model.rds")
# my_model <- readRDS("model.rds")
# This lets you to choose a new name for the object (you don't need to remember the name you used when you saved it)

plot(kknn.train)

#gaussian kernel of distance 1 has outperform others. Check for best tune.

load("kknn.train.rda")
kknn.train$bestTune

#The cos kernal with a distance of 1 outperformed the other combinations. For the red wine data, a kmax of 9 returned the highest accuracy.

kknn.predict <- predict(kknn.train, test)
confusionMatrix(kknn.predict, test$quality)

#Random Forest
rf.grid <- expand.grid(mtry = 1:11)
rf.train <- train(quality ~ ., data = train, method = "rf",
                  trControl = t.ctrl, tuneGrid = rf.grid,
                  preProcess = c("center", "scale"))

save(rf.train, file = "rf.train.rda")
plot(rf.train)

load("rf.train.rda")
rf.train$bestTune

rf.predict <- predict(rf.train, test)
confusionMatrix(rf.predict, test$quality)





#white wine analysis

#Importing the data of the white wine for the second step of analysis

white = read.csv("D:/Harvard/winequality-white.csv", sep = ";", header = T)

#viewing structure of the data and summary
str(white)

#check the names of the variables present in the data.
colnames(white)

#summary to better understand the dataset
summary(white)

#Removing the Duplicate Rows
white <- white[!duplicated(white), ]
dim(white)
str(white)
summary(white)

#Check for NA in dataset
sum(is.na(white))

#Response count in the dataset
table(white$quality)

#Plotting the histograms using hist() for the data.

#plot comparison of "QUALITY", "FIXED ACIDITY", "VOLATILE ACIDITY", "CITRIC ACID"

attach(white)

par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)
barplot((table(quality)), col=c("slateblue4", "slategray", "slategray1", "slategray2", "slategray3", "skyblue4"))
mtext("Quality", side=1, outer=F, line=2, cex=0.8)


truehist(fixed.acidity, h = 0.5, col="slategray3")
mtext("Fixed Acidity", side=1, outer=F, line=2, cex=0.8)

truehist(volatile.acidity, h = 0.05, col="slategray3")
mtext("Volatile Acidity", side=1, outer=F, line=2, cex=0.8)

truehist(citric.acid, h = 0.1, col="slategray3")
mtext("Citric Acid", side=1, outer=F, line=2, cex=0.8)

#PLOT COMPARISON OF "RESIDUAL SUGAR", "CHLORIDE", "ALCHOL", "DENSITY"

par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)

truehist(residual.sugar, h = 5, col="slategray3")
mtext("Residual Sugar", side=1, outer=F, line=2, cex=0.8)

truehist(chlorides, h = 0.01, col="slategray3")
mtext("Chloride", side=1, outer=F, line=2, cex=0.8)

truehist(alcohol, h = 0.5, col="slategray3")
mtext("Alcohol", side=1, outer=F, line=2, cex=0.8)


truehist(density, h = 0.005, col="slategray3")
mtext("Density", side=1, outer=F, line=2, cex=0.8)

#PLOT COMPARISON OF "FREE SULFUR DIOXIDE", "pH VALUES", "SULPHATES", "TOTAL.SULFUR.DIOXIDE"

par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)

truehist(free.sulfur.dioxide, h = 10, col="slategray3")
mtext("Free Sulfur Dioxide", side=1, outer=F, line=2, cex=0.8)

truehist(pH, h = 0.1, col="slategray3")
mtext("pH values", side=1, outer=F, line=2, cex=0.8)

truehist(sulphates, h = 0.1, col="slategray3")
mtext("sulphates", side=1, outer=F, line=2, cex=0.8)


truehist(total.sulfur.dioxide, h = 20, col="slategray3")
mtext("total.sulfur.dioxide", side=1, outer=F, line=2, cex=0.8)


#Relationship between each Attribute and Quality (Rating)

white_wine <- melt(white, "quality")
ggplot(white_wine, aes(value, quality, color = variable)) +  
  geom_point() + 
  geom_smooth(aes(value,quality, colour=variable), method=lm, se=FALSE)+
  facet_wrap(.~variable, scales = "free")

#Collinearity between Attributes
par(mfrow = c(1,1))
cor.white <- cor(white)
corrplot(cor.white, method = 'number')


white$quality_as_factor = factor(white$quality, levels = c(0,1,2,3,4,5,6,7,8,9,10))
summary(white)

white$quality <- as.factor(white$quality)
white$quality_as_factor = NULL

#setting the seed value to 1
set.seed(1)

#splitting the dataset into train and test (2/3rd for train remaining for test)

inTrain <- createDataPartition(white$quality, p = 2/3, list = F)
train <- white[inTrain,]
test <- white[-inTrain,]

#Structure and summary after split
str(train)
str(test)

summary(train)
summary(test)

##K-nearest neighbour model

t.ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
kknn.grid <- expand.grid(kmax = c(3, 5, 7 ,9, 11), distance = c(1, 2),
                         kernel = c("rectangular", "gaussian", "cos"))
kknn.train <- train(quality ~ ., data = train, method = "kknn",
                    trControl = t.ctrl, tuneGrid = kknn.grid,
                    preProcess = c("center", "scale"))

save(kknn.train, file = "kknn.train.rda")

# saveRDS(model, "model.rds")
# my_model <- readRDS("model.rds")
# This lets you to choose a new name for the object (you don't need to remember the name you used when you saved it)

plot(kknn.train)

load("kknn.train.rda")
kknn.train$bestTune

#Predicted accurancy
kknn.predict <- predict(kknn.train, test)
confusionMatrix(kknn.predict, test$quality)

#Random Forest

rf.grid <- expand.grid(mtry = 1:11)
rf.train <- train(quality ~ ., data = train, method = "rf",
                  trControl = t.ctrl, tuneGrid = rf.grid,
                  preProcess = c("center", "scale"))

save(rf.train, file = "rf.train.rda")
plot(rf.train)

load("rf.train.rda")
rf.train$bestTune

#Accurancy prediction
rf.predict <- predict(rf.train, test)
confusionMatrix(rf.predict, test$quality)

##############END OF THE CODE####################
