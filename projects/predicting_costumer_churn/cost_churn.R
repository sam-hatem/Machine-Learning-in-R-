
library(dplyr)
library(ggplot2)
library(GGally)
library(caTools)
library(ROCR)
library(MASS)

churn <- read.csv("customerchurn.csv")

set.seed(144)

split <- sample.split(churn$Churn, SplitRatio = 0.7)

train <- filter(churn, split == TRUE)
test <- filter(churn, split == FALSE)

str(train) # any issues with categorical x variables?

LogModel <- glm(Churn ~ MonthlyCharges + SeniorCitizen + PaymentMethod + InternetService + tenure + 
                  Contract, data=train, family="binomial")
summary(LogModel)


# Let's remove MonthlyCharges
LogModel2 <- glm(Churn ~ SeniorCitizen + PaymentMethod + InternetService + tenure + 
                  Contract, data=train, family="binomial")
summary(LogModel2)

# Let's remove PaymentMethod Credit Card and PaymentMethodMailed check.  

train_2 <- train

train$ElectronicCheck <- as.numeric(train$PaymentMethod == "Electronic check")
test$ElectronicCheck <- as.numeric(test$PaymentMethod == "Electronic check")

# Do it in other way:
train_2$ElectricCheck <- 0
train_2[train_2$PaymentMethod == "Electronic check",]$ElectricCheck <- 1
train
#
LogModel3 <- glm(Churn ~ SeniorCitizen + ElectronicCheck + InternetService + tenure + 
                   Contract, data=train, family="binomial")
summary(LogModel3)

predTestLog <- predict(LogModel, newdata=test, type="response") 


# Confusion matricies based on decision tree threshold 
table(test$Churn, predTestLog > 1/3)


# Using LDA:

LdaModel <- lda(Churn ~ MonthlyCharges + SeniorCitizen + PaymentMethod + InternetService + tenure + 
                  Contract, data=train )
summary(LdaModel)

predTestLDA <- predict(LdaModel, newdata=test)
predTestLDA_probs <- predTestLDA$posterior[,2]
summary(predTestLDA)
table(test$Churn, predTestLDA_probs > 1/3)

# ROC curves
rocr.log.pred <- prediction(predTestLog, test$Churn)
logPerformance <- performance(rocr.log.pred, "tpr", "fpr")
plot(logPerformance, colorize = TRUE)
abline(0, 1)
as.numeric(performance(rocr.log.pred, "auc")@y.values)


rocr.lda.pred <- prediction(predTestLDA_probs, test$Churn)
ldaPerformance <- performance(rocr.lda.pred, "tpr", "fpr")
plot(ldaPerformance, colorize = TRUE)
abline(0, 1)
as.numeric(performance(rocr.lda.pred, "auc")@y.values)

plot(logPerformance, col="blue")
plot(ldaPerformance, col="red", add=TRUE)
abline(0,1)


lda_fun_mod <- lda(PaymentMethod ~ MonthlyCharges + SeniorCitizen + InternetService + 
                     tenure + Contract, data = train)

pred_test_lda_fun <- predict(lda_fun_mod, newdata = test)
pred_test_lda_fun_class <- pred_test_lda_fun$class
pred_test_lda_fun_probs <- pred_test_lda_fun$posterior

tab <- table(test$PaymentMethod, pred_test_lda_fun_class)
accuracy <- sum(diag(tab))/ sum(tab)
accuracy
tab
