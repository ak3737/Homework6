
library(tidyverse)
library(readr)
library(dplyr)
library(knitr)
library(kableExtra)
library(forcats)
library(pandas)
library(Envstats)
library(outliers)
library(mice)
library(naniar)
library(pls)
library(lars)


test <- read.csv("Test.csv")
train <- read.csv("Train.csv")

attach(train)
View(train)
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

train %>% select_if(is.numeric) %>% names()
train %>% select_if(is.factor) %>% names()
train %>% select_if(is.character) %>% names()

train <- train %>% mutate_if(is.character, as.factor)

train %>% select_if(is.factor) %>% sapply(levels)
train %>% select_if(is.factor) %>% sapply(levels) %>% sapply(length)

anyNA(trainNoOutlier$isMobile)
#Notes:

#We should perform a test for outliers. There is almost certainly 
#one outlier (the largest transaction)
outlierTest <- grubbs.test(train$revenue)
trainNoOutlier <- train[train$revenue != 15980.79,]


##Models

#Model 1 Score was 2.17158
model1 <- lm(data = train, formula = revenue ~ operatingSystem + deviceCategory + visitNumber)
predict <- predict.lm(model1, test)

#Model 2 Score was 2.11272
model2 <- lm(data = trainNoOutlier, formula = revenue ~ operatingSystem + deviceCategory + visitNumber)

#Model 3 Score was 1.98235
model3 <- lm(data = trainNoOutlier, formula = revenue ~ 
               operatingSystem + deviceCategory + visitNumber + isTrueDirect)

#Model 4 Score was 1.97586
model4 <- lm(data = trainNoOutlier, formula = revenue ~ 
               operatingSystem + deviceCategory + visitNumber + isTrueDirect +
               timeSinceLastVisit + visitStartTime)

#Aggregate Training and Test Data
mdldata <- train %>% 
  group_by(custId) %>% 
  mutate(revenue = sum(revenue)) %>%
  mutate(operatingSystem  = first(operatingSystem)) %>%
  mutate(deviceCategory = first(deviceCategory)) %>%
  mutate(visits = sum(visitNumber)) %>%
  select(custId, revenue, deviceCategory,operatingSystem, visits)

mdldatatest <- test %>% 
  group_by(custId) %>% 
  mutate(operatingSystem  = first(operatingSystem)) %>%
  mutate(deviceCategory = first(deviceCategory)) %>%
  mutate(visits = sum(visitNumber)) %>%
  select(custId, deviceCategory,operatingSystem, visits)

#Model 5 Score: 1.41180
model5<- lm(data=mdldata, log(revenue + 1) ~ deviceCategory + operatingSystem + visits)

##Output

#Logic for outputting results for aggregated models
predict <- predict.lm(model5, mdldatatest)
predictDataFrame <- data.frame(predict)
predictDataFrame$custID <- mdldatatest$custId
predictDataFrame <- predictDataFrame[, c("custID", "predict")]
colnames(predictDataFrame) <- c("custID", "predRevenue")
noduplicates <- predictDataFrame[!duplicated(predictDataFrame$custID),]
write.csv(noduplicates, "submission.csv", row.names=FALSE)

#Aggregate by taking the sum per cust ID
averagedPredict <- aggregate(predict, by= list(test$custId), sum)

#Convert Negative Revenue to 0
averagedPredict[averagedPredict < 0] <- 0

#Transform data to be ln(predictedValue + 1)
averagedPredict[, 2] <- log(averagedPredict[, 2] + 1) 

#Change headers to the expected names
colnames(predict) <- c("custID", "predRevenue")

#Write to csv for submission
write.csv(averagedPredict, "submission.csv", row.names=FALSE)

