library(leaps)
#set up train and test
set.seed(1)
train <- sample(1:nrow(x),nrow(x)/2)
test <- (-train)

regfit.best <- regsubsets(views~.,data = TedClean[train,])
