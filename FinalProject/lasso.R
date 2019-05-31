library(glmnet)

set.seed(1)


y <- as.double(as.matrix(TedRaw$views)) # Only class
TedRaw$views <- NULL
x <- as.matrix(TedClean) # Removes class


# Fitting the model (Ridge: Alpha = 0)
#select test and train data
set.seed(1)
train <- sample(1:nrow(x),nrow(x)/2)
test <- (-train)
y.test <- y[test]

lasso.mod <- glmnet(x[train,],y[train], alpha = 1)
plot(lasso.mod)

#perform cross validation with lasso
set.seed(1)
cv.outLasso <- cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.outLasso)

bestlamlasso <- cv.outLasso$lambda.min
lasso.pred <- predict(lasso.mod,bestlamlasso, newx=x[test,])
mean((lasso.pred - y.test)^2)
#lower test MSE that ridge regression with lambda chosen by cv

#refit lasso
outlasso <- glmnet(x,y,alpha=1)
lasso.coef <- predict(outlasso,type = "coefficients", s=bestlamlasso)
lasso.coef