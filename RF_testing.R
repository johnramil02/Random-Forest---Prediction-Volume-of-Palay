library(randomForest)
library(dplyr)
library(Metrics)
library(caret)
library(e1071)

palay <- DM_DataCleaningFinal[1:540,]

palay$GEOLOCATION <- as.factor(palay$GEOLOCATION)
palay$YEAR <- as.integer(palay$YEAR)
palay$QUARTER <- as.factor(palay$QUARTER)
palay[is.na(palay)] = 0

print(palay)

test <- DM_DataCleaningFinal[541:680,]

test$GEOLOCATION <- as.factor(test$GEOLOCATION)
test$YEAR <- as.integer(test$YEAR)
test$QUARTER <- as.factor(test$QUARTER)
test[is.na(test)] = 0

print(test)

set.seed(222)

palay.rf <- randomForest(VOLUMEOFPRODUCTION ~ ., data=palay, ntree=500,
                         mtry=2, importance=TRUE, na.action = na.omit)

importance(palay.rf)
print(palay.rf)
plot(palay.rf)

plot(palay.rf$rsq)
plot(palay.rf$mse)

predictions1 = predict(palay.rf, newdata=test)
print(predictions1)

rmse(test$VOLUMEOFPRODUCTION,predictions1)
rmsle(test$VOLUMEOFPRODUCTION,predictions1)
mape(predictions1, test$VOLUMEOFPRODUCTION)
