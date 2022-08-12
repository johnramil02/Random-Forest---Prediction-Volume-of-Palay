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

topredict <- DataSetPredict[1:20,]

topredict$GEOLOCATION <- as.factor(topredict$GEOLOCATION)
topredict$YEAR <- as.integer(topredict$YEAR)
topredict$QUARTER <- as.factor(topredict$QUARTER)
topredict[is.na(topredict)] = 0

print(topredict)

palay.rf <- randomForest(VOLUMEOFPRODUCTION ~ ., data=palay, ntree=500,
                         mtry=2, importance=TRUE, na.action = na.omit)

print(palay.rf)
plot(palay.rf)

test$GEOLOCATION <- as.factor(test$GEOLOCATION)
test$YEAR <- as.integer(test$YEAR)
test$QUARTER <- as.factor(test$QUARTER)
testpredict <- predict(palay.rf, newdata = test)

predictions1 = predict(palay.rf, newdata=topredict)
print(predictions1)

predicted <- topredict
predicted$VOLUMEOFPRODUCTION <- predictions1
print(predicted)

palay2 <- rbind(palay,predicted)

print(palay2)

topredict2 <- DataSetPredict[21:40,]

topredict2$GEOLOCATION <- as.factor(topredict2$GEOLOCATION)
topredict2$YEAR <- as.integer(topredict2$YEAR)
topredict2$QUARTER <- as.factor(topredict2$QUARTER)
topredict2[is.na(topredict2)] = 0

print(topredict2)

palay2.rf <- randomForest(VOLUMEOFPRODUCTION ~ ., data=palay2, ntree=500,
                         mtry=2, importance=TRUE, na.action = na.omit)

print(palay2.rf)
plot(palay2.rf)

predictions2 = predict(palay2.rf, newdata=topredict2)
print(predictions2)

predicted2 <- topredict2
predicted2$VOLUMEOFPRODUCTION <- predictions2
print(predicted2)

palay3 <- rbind(palay2,predicted2)

print(palay3)

topredict3 <- DataSetPredict[41:60,]

topredict3$GEOLOCATION <- as.factor(topredict3$GEOLOCATION)
topredict3$YEAR <- as.integer(topredict3$YEAR)
topredict3$QUARTER <- as.factor(topredict3$QUARTER)
topredict3[is.na(topredict3)] = 0

print(topredict3)

palay3.rf <- randomForest(VOLUMEOFPRODUCTION ~ ., data=palay3, ntree=500,
                          mtry=2, importance=TRUE, na.action = na.omit)

print(palay3.rf)
plot(palay3.rf)

predictions3 = predict(palay3.rf, newdata=topredict3)
print(predictions3)

predicted3 <- topredict3
predicted3$VOLUMEOFPRODUCTION <- predictions3
print(predicted3)

palay4 <- rbind(palay3,predicted3)

print(palay4)

topredict4 <- DataSetPredict[61:80,]

topredict4$GEOLOCATION <- as.factor(topredict4$GEOLOCATION)
topredict4$YEAR <- as.integer(topredict4$YEAR)
topredict4$QUARTER <- as.factor(topredict4$QUARTER)
topredict4[is.na(topredict4)] = 0

print(topredict4)

palay4.rf <- randomForest(VOLUMEOFPRODUCTION ~ ., data=palay4, ntree=500,
                          mtry=2, importance=TRUE, na.action = na.omit)

print(palay4.rf)
plot(palay4.rf)

predictions4 = predict(palay4.rf, newdata=topredict4)
print(predictions4)

predicted4 <- topredict4
predicted4$VOLUMEOFPRODUCTION <- predictions4
print(predicted4)

palay5 <- rbind(palay4,predicted4)

print(palay5)

topredict5 <- DataSetPredict[81:100,]

topredict5$GEOLOCATION <- as.factor(topredict5$GEOLOCATION)
topredict5$YEAR <- as.integer(topredict5$YEAR)
topredict5$QUARTER <- as.factor(topredict5$QUARTER)
topredict5[is.na(topredict5)] = 0

print(topredict5)

palay5.rf <- randomForest(VOLUMEOFPRODUCTION ~ ., data=palay5, ntree=500,
                          mtry=2, importance=TRUE, na.action = na.omit)

print(palay5.rf)
plot(palay5.rf)

predictions5 = predict(palay5.rf, newdata=topredict5)
print(predictions5)

predicted5 <- topredict5
predicted5$VOLUMEOFPRODUCTION <- predictions5
print(predicted5)

palay6 <- rbind(palay5,predicted5)

print(palay6)

onlypredicted <- palay6[681:780,]
barplot(onlypredicted$VOLUMEOFPRODUCTION, onlypredicted$YEAR)


