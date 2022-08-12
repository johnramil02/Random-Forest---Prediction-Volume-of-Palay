library(randomForest)
library(dplyr)
library(Metrics)
library(caret)
library(e1071)

palay <- DM_DataCleaningFinal

palay$GEOLOCATION <- as.factor(palay$GEOLOCATION)
palay$YEAR <- as.integer(palay$YEAR)
palay$QUARTER <- as.factor(palay$QUARTER)
palay[is.na(palay)] = 0

print(palay)

topredict <- DataSetPredict

topredict$GEOLOCATION <- as.factor(topredict$GEOLOCATION)
topredict$YEAR <- as.integer(topredict$YEAR)
topredict$QUARTER <- as.factor(topredict$QUARTER)
topredict[is.na(topredict)] = 0

print(topredict)

palay.rf <- randomForest(VOLUMEOFPRODUCTION ~., data = palay, ntree=500,
                         mtry=2, importance=TRUE, na.action = na.omit)

importance(palay.rf)
print(palay.rf)
plot(palay.rf)

predictions2 = predict(palay.rf, newdata=topredict)
print(predictions1)

predicted <- test
predicted$VOLUME_OF_PRODUCTION <- predictions1
print(predicted)
