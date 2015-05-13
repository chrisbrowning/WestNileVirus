# load data
weather <- read.csv('weather.csv')
spray <- read.csv('spray.csv')
train <- read.csv('train.csv')
test <- read.csv('test.csv')

# summarize
prop.table(table(train$WnvPresent))

# str(weather)
# str(spray)
# str(train)
# str(test)

# unfactor weather data
weather$Tavg <- as.numeric(levels(weather$Tavg))[weather$Tavg]
weather$Sunset <- as.numeric(levels(weather$Sunset))[weather$Sunset]

# append weather data to training
train$Tavg <- weather$Tavg[match(train$Date,weather$Date)]
train$Sunset <- weather$Sunset[match(train$Date,weather$Date)]
train$DewPoint <- weather$DewPoint[match(train$Date,weather$Date)]
train$WetBulb <- weather$WetBulb[match(train$Date,weather$Date)]

# append weather data to test
test$Tavg <- weather$Tavg[match(test$Date,weather$Date)]
test$Sunset <- weather$Sunset[match(test$Date,weather$Date)]
test$DewPoint <- weather$DewPoint[match(test$Date,weather$Date)]
test$WetBulb <- weather$WetBulb[match(test$Date,weather$Date)]

# build logistic regression model
model <- WnvPresent ~ DewPoint + WetBulb + Tavg + Sunset
logistic_model <- glm(model, data=train,family="binomial")

# logistic_model
drop1(logistic_model)
Prediction <- predict(logistic_model,newdata = test,type="response")

summary(Prediction)
quantile(Prediction, .95)

# Set prediction threshold
Prediction[Prediction >= .124] <- 1
Prediction[Prediction < .124] <- 0

# make submission
submission <- data.frame(Id = test$Id, WnvPresent = Prediction)
write.csv(submission, file = "submission.csv", row.names = FALSE)
