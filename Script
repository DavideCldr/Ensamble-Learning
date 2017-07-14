library(MASS)
data(Boston)

n = 380
m = nrow(Boston) - n

set.seed(123)
istrain = sample(c(rep(T,n),rep(F,m)))

train = Boston[istrain,]
test = Boston[!istrain,]

library("SuperLearner")
listWrappers(what = "SL")

lib <- c("SL.glm",
         "SL.gam",
         "SL.rpart",
         "SL.randomForest",
         "SL.gbm")

set.seed(123)
fit = SuperLearner(Y=train[,14], 
                   X=train[,-14], 
                   SL.library=lib, 
                   method="method.NNLS")

fit

yhats = predict(fit, newdata=test[,-14])
mean( (test[,14] - yhats$pred)^2 )

apply(yhats$library.predict, 2, function(yhat)
  mean( (test[,14] - yhat)^2 )
  )
