library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(gbm)
library(caTools)
library(dplyr)
library(ggplot2)

 
OSR2 <- function(predictions, test, train) {
  SSE <- sum((test - predictions)^2)
  SST <- sum((test - mean(train))^2)
  r2 <- 1 - SSE/SST
  return(r2)
}

 


# begin analysis

ctr = read.csv("CTR.csv")

set.seed(144)
train.ids = sample(nrow(ctr), 0.7*nrow(ctr))
train.ctr = ctr[train.ids,]
test.ctr = ctr[-train.ids,]

# Let's try basic CART first
set.seed(33)

train.cart = train(CTR ~ .,
                   data = train.ctr,
                   method = "rpart",
                   tuneGrid = data.frame(cp=seq(0, 0.1, 0.005)),
                   trControl = trainControl(method="cv", number=5),
                   metric = "RMSE")
train.cart$results ## results : A data frame the training error rate and values of the tuning parameters.
train.cart
best.cart = train.cart$finalModel ##finalModel: A fit object using the best parameters
test.ctr.mm = as.data.frame(model.matrix(CTR ~ . + 0, data=test.ctr)) 


pred.best.cart = predict(best.cart, newdata = test.ctr.mm)


# Now let's try random forests
# First, basic training of a RF, can take a minute
set.seed(144)
mod.rf <- randomForest(CTR ~ ., data = train.ctr, mtry = 5, nodesize = 5, ntree = 500)

pred.rf <- predict(mod.rf, newdata = test.ctr) # just to illustrate

importance(mod.rf)

train.ctr.mm = as.data.frame(model.matrix(CTR ~ . + 0, data = train.ctr)) 
set.seed(3432)
mod.bag <- randomForest(x = train.ctr.mm, y = train.ctr$CTR, mtry = 17, nodesize = 5, ntree = 500)
pred.bag <- predict(mod.bag, newdata = test.ctr.mm)


set.seed(99)
train.rf <- train(CTR ~ .,
                  data = train.ctr,
                  method = "rf",
                  tuneGrid = data.frame(mtry=1:16),
                  trControl = trainControl(method="cv", number=5, verboseIter = TRUE),
                  metric = "RMSE")

train.rf$results
train.rf
best.rf <- train.rf$finalModel
pred.best.rf <- predict(best.rf, newdata = test.ctr.mm) # can use same model matrix

ggplot(train.rf$results, aes(x = mtry, y = Rsquared)) + geom_point(size = 3) + 
  ylab("CV Rsquared") + theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18))
