
library(tidyverse)
library(readr)
library(dplyr)
library(knitr)
library(kableExtra)
library(forcats)
library(Envstats)
library(mice)
library(naniar)

test <- read.csv("Test.csv")
train <- read.csv("Train.csv")

attach(train)

##Data Understanding

#There appears to be an outlier 
hist(train$revenue)
ggplot(train,
       aes(x = revenue,
           y = visitNumber)) +
  geom_miss_point()

#Mostly equal transaction amounts here BUT
# It looks like Tablet has lower transactions than mobile
# and mobile is less than desktop
# but need to do some missing value imputation (sub in an other category)
ggplot(train,
       aes(x = revenue,
           y = deviceCategory)) +
  geom_miss_point()

#Windows, Mac, and Chrome OS are highest transactions
ggplot(train,
       aes(x = revenue,
           y = operatingSystem)) +
  geom_miss_point()

#Notes:

#We should perform a test for outliers. There is almost certainly 
#one outlier (the largest transaction)



#The below code is just me playing around with the data
train2 <- train[,!colnames(train) %in% c("browser", "country")]

model1 <- lm(data = train2, formula = revenue ~ operatingSystem + deviceCategory + visitNumber)

predict <- predict.lm(model1, test)

anyNA(predict)

plot(predict)

averagedPredict <- aggregate(predict, by= list(test$custId), mean)

anyNA(averagedPredict$x)
hist(averagedPredict$x)


min(averagedPredict$x)
max(averagedPredict$x)

head(averagedPredict)
