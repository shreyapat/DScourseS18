library(mlr)
library(glmnet)

housing <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")

names(housing)

names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")
names(housing)

#  (http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.names)
#    1. CRIM      per capita crime rate by town
#    2. ZN        proportion of residential land zoned for lots over 25,000 sq.ft.
#    3. INDUS     proportion of non-retail business acres per town
#    4. CHAS      Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
#    5. NOX       nitric oxides concentration (parts per 10 million)
#    6. RM        average number of rooms per dwelling
#    7. AGE       proportion of owner-occupied units built prior to 1940
#    8. DIS       weighted distances to five Boston employment centres
#    9. RAD       index of accessibility to radial highways
#    10. TAX      full-value property-tax rate per $10,000
#    11. PTRATIO  pupil-teacher ratio by town
#    12. B        1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
#    13. LSTAT    % lower status of the population
#    14. MEDV     Median value of owner-occupied homes in $1000's

housing$lmedv <- log(housing$medv)
housing$medv <- NULL # drop median value
formula<- as.formula(lmedv ~ .^3 +
                          poly(crim,6) +
                          poly(zn,6) +
                          poly(indus,6) +
                          poly(nox,6) +
                          poly(rm,6) +
                          poly(age,6) +
                          poly(dis,6) +
                          poly(rad,6) +
                          poly(tax,6) +
                          poly(ptratio,6) +
                          poly(b,6) +
                          poly(lstat,6))

mod_matrix <- data.frame(model.matrix(formula,housing))

mod_matrix [,1] = housing$lmedv

colnames(mod_matrix)[1] = "lmedv"

head(mod_matrix) 

n <- nrow(mod_matrix)
train <- sample(n, size = .8*n)
train.a<-as.array(train)
dim(train.a)
test <- setdiff(1:n, train)
housing.train <- mod_matrix[train,]
housing.test <- mod_matrix[test,]

housing$lmedv <- log(housing$medv)
housing$medv <- NULL
housing$dis2 <- housing$dis^2
housing$chasNOX <- housing$crim * housing$nox

n <- nrow(housing)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
housing.train <- housing[train,]
housing.test  <- housing[test, ]

theTask <- makeRegrTask(id = "taskname", data = housing.train, target = "lmedv")
print(theTask)

predAlg <- makeLearner("regr.lm")

resampleStrat <- makeResampleDesc(method = "CV", iters = 6)

sampleResults <- resample(learner = predAlg, task = theTask, resampling = resampleStrat, measures=list(rmse))

print(sampleResults$aggr)

library(glmnet)

predAlg <- makeLearner("regr.glmnet")


modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=1,upper=1))

tuneMethod <- makeTuneControlRandom(maxit = 50L)


tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         # RMSE performance measure, this can be changed to one or many
                         measures = rmse,      
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)


predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

resample(predAlg,theTask,resampleStrat,measures=list(rmse))

finalModel <- train(learner = predAlg, task = theTask)

prediction <- predict(finalModel, newdata = housing.test)

performance(prediction, measures = list(rmse))

print(head(prediction$data))

modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=0))

tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,       
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)


predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

resample(predAlg,theTask,resampleStrat,measures=list(rmse))

finalModel <- train(learner = predAlg, task = theTask)

prediction <- predict(finalModel, newdata = housing.test)

performance(prediction, measures = list(rmse))

print(prediction)
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=1))

tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,    
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

resample(predAlg,theTask,resampleStrat,measures=list(rmse))

finalModel <- train(learner = predAlg, task = theTask)

prediction <- predict(finalModel, newdata = housing.test)

performance(prediction, measures = list(rmse))

print(prediction)

