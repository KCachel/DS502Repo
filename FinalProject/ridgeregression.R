library(glmnet)

set.seed(1)

y <- as.double(as.matrix(TedClean$views)) # Only class
TedClean$views <- NULL
x <- as.matrix(TedClean) # Removes class


# Fitting the model (Ridge: Alpha = 0)

mod.ridge <- cv.glmnet(x, y, alpha=0, lambda = grid, standardize=TRUE, type.measure='auc')
mod.ridge$lambda

grid <- 10^seq(10,-2, length =100)

mod.lasso <- cv.glmnet(x,y, alpha = 1, lambda = grid)

plot(mod.lasso)