library(randomForest)

set.seed(1)
train <- sample(1:nrow(TedClean),nrow(TedClean)/2)
test <- (-train)

Ted.test <-TedClean[-train,"views"]

bag.Ted <- randomForest(views~., data=TedClean, subset = train,
                        mtry = 145, importance =T)
bag.Ted

#how does the bagged model (all predictors) perform on the test?
yhat.bag <- predict(bag.Ted,newdata=TedClean[-train,])
mean((yhat.bag -Ted.test)^2)

importance(bag.Ted)
varImpPlot(bag.Ted)

## try random forest with p/3 variables
set.seed(1)
rf.Ted <- randomForest(views~., data=TedClean, subset = train,
                       importance = T)
yhat.rf <- predict(rf.Ted,newdata=TedClean[-train,])
#much lower MSE error, random forest showed an improvement over bagging
mean((yhat.rf-Ted.test)^2)

importance(rf.Ted)
varImpPlot(rf.Ted)
