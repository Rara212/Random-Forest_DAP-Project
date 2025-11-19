#Setting working directory for the file
setwd("~/DAI_IML Project")

#Getting data
library(readr)
online_shoppers_intention <- read_csv("online_shoppers_intention.csv")

#Exploratory Data Analysis
str(online_shoppers_intention)
summary(online_shoppers_intention)
##Changing character data into factors for Month and VisitorType
online_shoppers_intention$Month <- as.factor(online_shoppers_intention$Month)
online_shoppers_intention$VisitorType <- as.factor(online_shoppers_intention$VisitorType)
online_shoppers_intention$Region <- as.factor(online_shoppers_intention$Region)
online_shoppers_intention$TrafficType <- as.factor(online_shoppers_intention$TrafficType)
online_shoppers_intention$Browser <- as.factor(online_shoppers_intention$Browser)
online_shoppers_intention$OperatingSystems <- as.factor(online_shoppers_intention$OperatingSystems)
online_shoppers_intention$Revenue <- as.factor(online_shoppers_intention$Revenue)
##Changing Month levels to 12 months
online_shoppers_intention$Month <- as.character(online_shoppers_intention$Month)
online_shoppers_intention$Month[online_shoppers_intention$Month == "June"] <- "Jun"
online_shoppers_intention$Month <- as.factor(online_shoppers_intention$Month)
online_shoppers_intention$Month = factor(online_shoppers_intention$Month, levels = month.abb)
##Checking distributions of data
prop.table(table(online_shoppers_intention$Revenue))#knowing proportions on categorical variables
barchart(online_shoppers_intention$Revenue, main = "Revenue") #shows imbalance
prop.table(table(online_shoppers_intention$Weekend))
barchart(online_shoppers_intention$Weekend, main = "Online Visit on Weekend") #shows imbalance
prop.table(table(online_shoppers_intention$Month))
barchart(online_shoppers_intention$Month, main = "Month")
prop.table(table(online_shoppers_intention$Region))#doesn't seems to have significant impact on one another
prop.table(table(online_shoppers_intention$Browser))
prop.table(table(online_shoppers_intention$TrafficType))
prop.table(table(online_shoppers_intention$OperatingSystems))
##Feature Plots
library(caret)
featurePlot(x = online_shoppers_intention[c("Administrative", "Administrative_Duration", "Informational", "Informational_Duration", "ProductRelated", "ProductRelated_Duration", "BounceRates", "ExitRates", "PageValues", "SpecialDay")], 
            y = online_shoppers_intention$Revenue, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))
hist(online_shoppers_intention$Administrative, main = "Histogram of Administrative Pages Visited")
hist(online_shoppers_intention$Administrative_Duration, main = "Histogram of Administrative Pages Duration Visited")
hist(online_shoppers_intention$Informational, main = "Histogram of Informational Pages Visited")
hist(online_shoppers_intention$Informational_Duration, main = "Histogram of Informational Pages Duration Visited")
hist(online_shoppers_intention$ProductRelated, main = "Histogram of ProductRelated Pages Visited")
hist(online_shoppers_intention$ProductRelated_Duration, main = "Histogram of ProductRelated Pages Duration Visited")
##Checking correlations
featurePlot(x = online_shoppers_intention[c("Administrative", "Administrative_Duration", "Informational", "Informational_Duration", "ProductRelated", "ProductRelated_Duration", "BounceRates", "ExitRates", "PageValues", "SpecialDay")], 
            y = online_shoppers_intention$Revenue, 
            plot = "pairs",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))
plot(online_shoppers_intention[c("Administrative", "Administrative_Duration", "Informational", "Informational_Duration", "ProductRelated", "ProductRelated_Duration", "BounceRates", "ExitRates", "PageValues", "SpecialDay")])

#Cleaning Data
##Deleting columns Region, Browser, OperatingSystems 
online_shoppers_intention <- online_shoppers_intention[,-(12:14), drop=FALSE]
View(online_shoppers_intention)
##Handling imbalance data of Revenue
osi_data <- upSample(online_shoppers_intention[,-15], online_shoppers_intention$Revenue, yname = "Revenue")
str(osi_data)
##Checking missing values
sum(is.na(online_shoppers_intention))
##Checking duplicate data and removing duplicated data
duplicated(online_shoppers_intention)
sum(duplicated(online_shoppers_intention)) #there is duplicate in the data by 125
library(tidyverse)
online_shoppers_intention <- online_shoppers_intention[!duplicated(online_shoppers_intention), ]

