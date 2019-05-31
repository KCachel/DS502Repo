library(glmnet)

set.seed(1)
data <- TedClean
y <- as.double(as.matrix(data$views)) # Only class
data$views <- NULL
x <- as.matrix(data) # Removes class

# Fitting the model (Ridge: Alpha = 0)
#select test and train data
set.seed(1)
train <- sample(1:nrow(x),nrow(x)/2)
test <- (-train)
y.test <- y[test]

ridge.mod <-glmnet(x[train,],y[train], alpha = 0)

#ridge regression with really large lambda
ridge.predlarge <- predict(ridge.mod,s=1e10,newx = x[test,])
#test MSE for large lambda
mean((ridge.predlarge-y.test)^2)

#ridge regression with cross validation

#first select best lambda
set.seed(1)
cv.out <- cv.glmnet(x[train,],y[train],alpha = 0)
plot(cv.out)

bestlam <- cv.out$lambda.min
bestlam

#run with best lambda
ridge.pred <- predict(ridge.mod,s=bestlam,newx = x[test,])

#what is test MSE associeated with lambda = bestlam?
mean((ridge.pred -y.test)^2)
# so the test MSE decreased by half but it is still very large

#Refit ridge regression model on the full data set with cv chosen lambda
set.seed(1)
out <- glmnet(x,y,alpha=0)
predict(out,type = "coefficients",s=bestlam)

##As expect none of the coefficients are zero and this is an highly uninterpretabe model.

