title: "IEOR 142 Project Model Building"
author: "XiaoRou Liang"
date: "11/18/2018"
output: html_document
---
  ```{r}
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(caTools)
library(ggplot2)
library(VIF)
library(car)
library(caret)
install.packages('VIF')
install.packages('car')
```
```{r}


```
```{r}


#Text analysis 

library(tm)
library(SnowballC)
library(MASS)
library(caTools)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(ggplot2)
library(VIF)
library(car)

#load the data
airbnb <- read.csv("final-airbnb.csv", sep = '\t')

#converting listing names, summary, space, description to a "corpus"
summary_corpus = Corpus(VectorSource(airbnb$Summary))
house_rules_corpus = Corpus(VectorSource(airbnb$House.Rules))
host_about_corpus = Corpus(VectorSource(airbnb$Host.About))

summary_corpus[[1]]
strwrap(house_rules_corpus[[2]])

#lower case
summary_corpus = tm_map(summary_corpus, tolower)
house_rules_corpus = tm_map(house_rules_corpus, tolower)
host_about_corpus = tm_map(host_about_corpus, tolower)
# remove punctuation
summary_corpus = tm_map(summary_corpus, removePunctuation)
house_rules_corpus = tm_map(house_rules_corpus, removePunctuation)
host_about_corpus = tm_map(host_about_corpus, removePunctuation)
# remove stop words
summary_corpus = tm_map(summary_corpus, removeWords, stopwords("english"))
house_rules_corpus = tm_map(house_rules_corpus, removeWords, stopwords("english"))
host_about_corpus = tm_map(host_about_corpus, removeWords, stopwords("english"))
#stem
summary_corpus = tm_map(summary_corpus, stemDocument)
house_rules_corpus = tm_map(house_rules_corpus, stemDocument)
host_about_corpus = tm_map(host_about_corpus, stemDocument)
#create a word count matrix
summary_frequencies = DocumentTermMatrix(summary_corpus)
house_rules_frequencies = DocumentTermMatrix(house_rules_corpus)
host_about_frequencies = DocumentTermMatrix(host_about_corpus)
# Words that appear at least 50 times:
findFreqTerms(summary_frequencies, lowfreq=50)
findFreqTerms(house_rules_frequencies, lowfreq=50)
findFreqTerms(host_about_frequencies, lowfreq=50)
# Words that appear at least 20 times:
findFreqTerms(summary_frequencies, lowfreq=20)

#sparsity
summary_sparse = removeSparseTerms(summary_frequencies, 0.90)
#59 words
house_rules_sparse = removeSparseTerms(house_rules_frequencies, 0.90)
#32 words
host_about_sparse = removeSparseTerms(host_about_frequencies, 0.90)
#37 words
#creating dataframe
summary_dat = as.data.frame(as.matrix(summary_sparse))
colnames(summary_dat) = make.names(colnames(summary_dat))

house_rules_dat = as.data.frame(as.matrix(house_rules_sparse))
colnames(house_rules_dat) = make.names(colnames(house_rules_dat))

host_about_dat = as.data.frame(as.matrix(host_about_sparse))
colnames(host_about_dat) = make.names(colnames(host_about_dat))

#dat.dup <- cbind(name_dat, space_dat, description_dat)
#dups <- unique(names(dat.dup)[duplicated(names(dat.dup))])
#for (i in dups) {
#  dat.dup[[i]] <- rowSums(dat.dup[names(dat.dup) == i])
#}
#dat <- dat.dup[!duplicated(names(dat.dup))]

#putting everything into a dataframe
data_new <- data.frame(summary_dat, house_rules_dat, host_about_dat)
data_new$Price <- airbnb$Price

# Split data into training and testing sets
set.seed(1)  # So we get the same results
spl = sample.split(data_new$Price, SplitRatio = 0.7)

textTrain = data_new %>% filter(spl == TRUE)
textTest = data_new %>% filter(spl == FALSE)



#numerical analysis 

OSR2 <- function(predictions, train, test) {
  SSE <- sum((test - predictions)^2)
  SST <- sum((test - mean(train))^2)
  r2 <- 1 - SSE/SST
  return(r2)
}
data$Neighbourhood.Cleansed
#dataset for first set of models
data <- airbnb
data$Host.Response.Time

#dataset for first set of models with crime 
data1 <- subset(data, select = c(Host.Response.Time, Host.Location, Calculated.host.listings.count,
                Host.Verifications, Property.Type, Room.Type, Accommodates, Bathrooms, Bedrooms, Beds, Bed.Type, Price,
                Guests.Included, Minimum.Nights, Maximum.Nights, Availability.365, Calendar.Updated,
                Number.of.Reviews, Review.Scores.Rating,
                Review.Scores.Accuracy, Review.Scores.Cleanliness,
                Review.Scores.Checkin, Review.Scores.Communication, Review.Scores.Location,
                Review.Scores.Value, Cancellation.Policy, Reviews.per.Month, duration_rev,
                Time.since.last.review, Number.of.Amenities,
                Host.Duration, crime, Neighbourhood.Cleansed ))
#dataset for first set of models with Neighbourhood.Cleansed
ncol(data1)


#merging text and numerical 

data_final = cbind(data1, data_new)
ncol(data_final)


str(data1)
```
```{r}
set.seed(1)
train.ids = sample(nrow(data_final), 0.70*nrow(data_final))
train = data_final[train.ids,]
test = data_final[-train.ids,]
```
```{r}


#bootstrap BS 
library(boot)

mean_squared_error <- function(data, index) {
  responses <- data$response[index]
  predictions <- data$prediction[index]
  MSE <- mean((responses - predictions)^2)
  return(MSE)
}

mean_absolute_error <- function(data, index) {
  responses <- data$response[index]
  predictions <- data$prediction[index]
  MAE <- mean(abs(responses - predictions))
  return(MAE)
}

OS_R_squared <- function(data, index) {
  responses <- data$response[index]
  predictions <- data$prediction[index]
  baseline <- data$baseline[index]
  SSE <- sum((responses - predictions)^2)
  SST <- sum((responses - baseline)^2)
  r2 <- 1 - SSE/SST
  return(r2)
}

OSR2 <- function(predictions, train, test) {
  SSE <- sum((test - predictions)^2)
  SST <- sum((test - mean(train))^2)
  r2 <- 1 - SSE/SST
  return(SSE)
}

all_metrics <- function(data, index) {
  mse <- mean_squared_error(data, index)
  mae <- mean_absolute_error(data, index)
  OSR2 <- OS_R_squared(data, index)
  return(c(mse, mae, OSR2))
}



#linear regression 
#training 



train.reg <- lm(Price ~ ., data = train)
summary(train.reg)
vif(train.reg)

#we do stepwise regression 
BNBStep = step(train.reg, direction = "backward")
summary(BNBStep)
PredictStep = predict(BNBStep, newdata = test)

#metrics 
MAE = mean(abs(test$Price-PredictStep))
MAE
RMSE = sqrt(mean((test$Price - PredictStep)^2))
RMSE
OS2 = OSR2(predictions = PredictStep, train$Price, test$Price)
OS2


# Cart cross validated 

train.cart <- train(Price ~ .,
                    data = train,
                    method = "rpart",
                    tuneGrid = data.frame(cp=seq(0, 0.1, 0.005)),
                    trControl = trainControl(method="cv", number=5),
                    metric = "RMSE")
train.cart$results
best_cp_b=train.cart$bestTune
best.cart = train.cart$finalModel
test.ctr.mm = as.data.frame(model.matrix(Price ~ ., data=test))
pred.best.cart = predict(best.cart, newdata = test.ctr.mm)
cfmatrix= table(test$Price, pred.best.cart)
cfmatrix

prp(best.cart)
#metrics (test)

MAE = mean(abs(test$Price-pred.best.cart))
MAE
RMSE = sqrt(mean((test$Price - pred.best.cart)^2))
RMSE
OS2 = OSR2(predictions = pred.best.cart, train$Price, test$Price)
OS2

```
#cI bootttt

CART_test_set = data.frame(response = test$Price, prediction = pred.best.cart, baseline = mean(train$Price))

#cross validated random forests 
all_metrics(CART_test_set, 1:687)


```
```{r}
set.seed(1000)
train.rf = train(Price ~ .,
                 data = train,
                 method = "rf",
                 tuneGrid = data.frame(mtry = 1:15),
                 trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE))
train.rf
train.rf$results


mod.rf = train.rf$finalModel

test.ctr = as.data.frame(model.matrix(Price ~ ., data=test))

predict.rf = predict(mod.rf, newdata = test.ctr)

#metrics on test 


MAE = mean(abs(test$Price-predict.rf))
MAE
RMSE = sqrt(mean((test$Price - predict.rf)^2))
RMSE
OS2 = OSR2(predictions = predict.rf, train$Price, test$Price)
OS2


####





