
#boosting 
mod.boost <- gbm(CTR ~ .,
                 data = train.ctr,
                 distribution = "gaussian",
                 n.trees = 1000,
                 shrinkage = 0.001,
                 interaction.depth = 2)

# NOTE: we need to specify number of trees to get a prediction for boosting
pred.boost <- predict(mod.boost, newdata = test.ctr, n.trees=1000)

pred.boost.earlier <- predict(mod.boost, newdata = test.ctr, n.trees=330)

summary(mod.boost) # tells us what is the most influential variables.

# cross validation on n.trees and interaction depth

tGrid = expand.grid(n.trees = (1:75)*500, interaction.depth = c(1,2,4,6,8,10),
                    shrinkage = 0.001, n.minobsinnode = 10)

set.seed(232)
train.boost <- train(CTR ~ .,
                     data = train.ctr,
                     method = "gbm",
                     tuneGrid = tGrid,
                     trControl = trainControl(method="cv", number=5, verboseIter = TRUE),
                     metric = "RMSE",
                     distribution = "gaussian")
train.boost
best.boost <- train.boost$finalModel
pred.best.boost <- predict(best.boost, newdata = test.ctr.mm, n.trees = 11500) # can use same model matrix

ggplot(train.boost$results, aes(x = n.trees, y = Rsquared, colour = as.factor(interaction.depth))) + geom_line() + 
  ylab("CV Rsquared") + theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18)) + 
  scale_color_discrete(name = "interaction.depth")


# linear model for baseline 
mod.lm <- lm(CTR ~ ., data = train.ctr)
pred.mod.lm <- predict(mod.lm, newdata = test.ctr)




# final comparisons
print("Linear Regression OSR2:")
OSR2(pred.mod.lm, test.ctr$CTR, train.ctr$CTR)
print("CART OSR2:")
OSR2(pred.best.cart, test.ctr$CTR, train.ctr$CTR)
print("Random Forests OSR2:")
OSR2(pred.best.rf, test.ctr$CTR, train.ctr$CTR)
print("Boosting OSR2:")
OSR2(pred.best.boost, test.ctr$CTR, train.ctr$CTR)

print("Linear Regression Out-of-sample MAE:")
sum(abs(test.ctr$CTR - pred.mod.lm))/nrow(test.ctr.mm)
print("CART Out-of-sample MAE:")
sum(abs(test.ctr$CTR - pred.best.cart))/nrow(test.ctr.mm)
print("Random Forests Out-of-sample MAE:")
sum(abs(test.ctr$CTR - pred.best.rf))/nrow(test.ctr.mm)
print("Boosting Out-of-sample MAE:")
sum(abs(test.ctr$CTR - pred.best.boost))/nrow(test.ctr.mm)

# for boosting/rf let's look at MAE restricted to CTR above and below 10%
test.ctr.above <- filter(test.ctr, CTR > .1)
test.ctr.below <- filter(test.ctr, CTR <= .1)
test.ctr.above.mm = as.data.frame(model.matrix(CTR ~ . + 0, data=test.ctr.above))
test.ctr.below.mm = as.data.frame(model.matrix(CTR ~ . + 0, data=test.ctr.below))

nrow(test.ctr.below)
nrow(test.ctr.above)

pred.boost.above <- predict(best.boost, newdata = test.ctr.above.mm, n.trees = 11500)
pred.boost.below <- predict(best.boost, newdata = test.ctr.below.mm, n.trees = 11500)

pred.rf.above <- predict(best.rf, newdata = test.ctr.above.mm)
pred.rf.below <- predict(best.rf, newdata = test.ctr.below.mm)

print("Boosting Out-of-sample MAE for CTR above 10%:")
sum(abs(test.ctr.above$CTR - pred.boost.above))/nrow(test.ctr.above)
print("Boosting Out-of-sample MAE for CTR below 10%:")
sum(abs(test.ctr.below$CTR - pred.boost.below))/nrow(test.ctr.below)

print("RF Out-of-sample MAE for CTR above 10%:")
sum(abs(test.ctr.above$CTR - pred.rf.above))/nrow(test.ctr.above)
print("RF Out-of-sample MAE for CTR below 10%:")
sum(abs(test.ctr.below$CTR - pred.rf.below))/nrow(test.ctr.below)

