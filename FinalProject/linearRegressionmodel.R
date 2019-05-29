#cite : https://stackoverflow.com/questions/46493011/r-loop-for-variable-names-to-run-linear-regression-model


##Full linear regression model
set.seed(1)
summary(lm(views~., TedClean))



data <- TedClean
col10 <- names(TedClean)[-1]

for(i in seq_along(col10)){
  lm.test[[i]] <- lm(reformulate(col10[i], "views"), data = data)
}

lm.test
smry <- lapply(lm.test, summary)
cfs <- lapply(lm.test, coef)