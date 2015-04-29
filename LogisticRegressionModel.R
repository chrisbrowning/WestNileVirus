# load data
weather <- read.csv('weather.csv')
spray <- read.csv('spray.csv')
train <- read.csv('train.csv')
test <- read.csv('test.csv')

# summarize
prop.table(table(train$WnvPresent))

# build logistic model
logistic_fit <- glm(WnvPresent ~ Latitude + Longitude, data=train, family= "binomial")
Prediction <- predict(logistic_fit, test,type= "response")
submission <- data.frame(Id = test$Id, WnvPresent = Prediction)
write.csv(submission, file = "submission.csv", row.names = FALSE)
