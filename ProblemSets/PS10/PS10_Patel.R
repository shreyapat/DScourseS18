install.packages(rpart)
install.packages(e1071)
install.packages(kknn)
install.packages(nnet)
install.packages(mlr)

#install.packages(class)


set.seed(100)

income <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.

######################
# Clean up the data
######################
# Drop unnecessary columns


income$native.country <- NULL
income$fnlwgt         <- NULL

# Make sure continuous variables are coded as such
income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)


# Combine levels of categorical variables that currently have too many levels
levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

# Break up the data:
n <- nrow(income)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
income.train <- income[train,]
income.test  <- income[test, ]

#5 
# Using the mlr library, create the following objects for your cross validation exercise:
#     • The classification task

task.highearner <- makeClassifTask(data = income.train, target = "high.earner")
print(task.highearner)

#     • The 3-fold cross-validation strategy

resampleStrat <- makeResampleDesc(method = "CV", iters = 3)

#     • The tuning strategy (e.g. random with 10 guesses)

tuneMethod <- makeTuneControlRandom(maxit = 10L)

#     • Each of the six “learners” (algorithms):
#         1. Trees: classif.rpart
#         2. Logistic regression: classif.glmnet
#         3. Neural network: classif.nnet
#         4. Naive Bayes: classif.naiveBayes
#         5. kNN: classif.kknn
#         6. SVM: classif.svm
#       For each algorithm, add predict.type = “response” as an additional argument
#       to the makeLearner() function.

trees <- makeLearner("classif.rpart")
logit <- makeLearner("classif.glmnet")
nn <- makeLearner("classif.nnet")
nb <- makeLearner("classif.naiveBayes")
knn <-makeLearner("classif.kknn")
svm <- makeLearner("classif.svm")

trees <- (predict.type = "response")
ogit <- makeLearner(predict.type = "response")
nn <- makeLearner(predict.type = "response")
nb <- makeLearner(predict.type = "response")
knn <-makeLearner(predict.type = "response")
svm <- makeLearner(predict.type = "response")

#6
# Now set up the hyperparameters of each algorithm that will need to be cross validated.
# If you aren’t sure what to call them, please refer to the individual package documenation.
#
#     • Tree model
#         – minsplit, which is an integer ranging from10 to 50 (governsminimum sample size for making a split)
#         
#         – minbucket, which is an integer ranging from5 to 50 (governsminimum sample size in a leaf)
#         
#         – cp, which is a real number ranging from 0.001 to 0.2 (governs complexity of the tree)

parameters.tree <- makeParamSet(
makeIntegerParam("minsplit", lower = 10, upper = 50),
makeIntegerParam("minbucket", lower = 5, upper = 50),
makeNumericParam("cp", lower = 0.001, upper = 0.2))

# 
#
#     • Logit model (this is the elastic net model from last problem set, so the two parameters
#       are l and a. For this problem set, let l 2 [0, 3] and a 2 [0, 1].
#

parameters.logit <- makeParamSet(
makeNumericParam("lambda",lower=0,upper=3),
makeNumericParam("alpha",lower=0,upper=1))

#
#     • Neural network model
#         – size, which is an integer ranging from 1 to 10 (governs number of units in hidden layer)
#         
#         – decay, which is a real number ranging from0.1 to 0.5 (acts like l in the elastic net model)
#
#         – maxit, which is an integer ranging from 1000 to 1000 (i.e. this should be
#           fixed at 1,000 ... it governs the number of iterations the neural network takes
#           when figuring out convergence)
#
parameters.nn <- makeParamSet(
makeIntegerParam("size", lower = 1, upper = 10),
makeNumericParam("decay", lower = 0.1, upper = 0.5),
makeIntegerParam("maxit", lower = 1000, upper = 1000))

#
#     • Naive Bayes
#         – There’s nothing to regularize here, so you don’t need to do any cross-validation
#               or tuning for this model
#
#     • kNN
#         – k, which is an integer ranging from 1 to 30 (governs the number of “neighbors” to consider
#
parameters.knn <- makeParamSet(
makeIntegerParam("k", lower = 1, upper = 30))

#
#     • SVM
#         – kernel, which is a string value equal to “radial” (program it as “discrete”
#           in mlr’s interface) in the set {2^−2, 2^−1, 2^0, 2^1, 2^2, 2^10}	 (governs how soft the
#           margin of classification is)
#
#         – cost,which is a real number (“discrete” in mlr’s interface) in the set {2^−2, 2^−1, 2^0, 2^1, 2^2, 2^10}	
#               (governs how soft the margin of classification is)
#
#         – gamma, which is also a real number in the set {2^−2, 2^−1, 2^0, 2^1, 2^2, 2^10	(governs the shape 
#           [variance] of the Gaussian kernel)

parameters.svm  <- makeParamSet(
makeDiscreteParam("cost", values = 2^c(-2, -1, 0, 1, 2, 10)), 
makeDiscreteParam("gamma", values = 2^c(-2, -1, 0, 1, 2, 10)))


#7 
# Now tune the models. Use the F1 score and the G-measure as criteria for tuning
# (these are f1 and gmean, respectively). Remember that you don’t need to tune the
# Naive Bayes model.

tunedModel.trees <- tuneParams(learner = trees,
  task = task.highearner,
  resampling = resampleStrat,
  measures = list(f1, gmean),
  par.set = parameters.tree,
  control = tuneMethod,
  show.info = TRUE)

tunedModel.logit <- tuneParams(learner = logit,
  task = task.highearner,
  resampling = resampleStrat,
  measures = list(f1, gmean),
  par.set = parameters.logit,
  control = tuneMethod,
  show.info = TRUE)

tunedModel.nn <- tuneParams(learner = nn,
  task = task.highearner,
  resampling = resampleStrat,
  measures = list(f1, gmean),      
  par.set = parameters.nn,
  control = tuneMethod,
  show.info = TRUE)

tunedModel.knn <- tuneParams(learner = knn,
  task = task.highearner,
  resampling = resampleStrat,
  measures = list(f1, gmean),      
  par.set = parameters.knn,
  control = tuneMethod,
  show.info = TRUE)

#no native bayes

tunedModel.svm <- tuneParams(learner = svm,
  task = task.highearner,
  resampling = resampleStrat,
  measures = list(f1, gmean),      
  par.set = parameters.svm,
  control = tuneMethod,
  show.info = TRUE)

#8 
# Once tuned, apply the optimal tuning parameters to each of the algorithms (again,
# you don’t need to do this for Naive Bayes since there was no tuning done previously).
# Then train the models, generate predictions, and assess performance.

pred.trees <- setHyperPars(learner=trees, par.vals = tunedModel.trees$x)
pred.logit <- setHyperPars(learner=logit, par.vals = tunedModel.logit$x)
pred.nn    <- setHyperPars(learner=nn, par.vals = tunedModel.nn$x)
pred.knn   <- setHyperPars(learner=knn, par.vals = tunedModel.knn$x)
pred.svm   <- setHyperPars(learner=svm, par.vals = tunedModel.svm$x)

sampleResults.tree  <- resample(learner = pred.trees, task = task.highearner, resampling = resampleStrat, measures=list(gmean))
sampleResults.logit <- resample(learner = pred.logit, task = task.highearner, resampling = resampleStrat, measures=list(gmean))
sampleResults.nn    <- resample(learner = pred.nn, task = task.highearner, resampling = resampleStrat, measures=list(gmean))
sampleResults.knn   <- resample(learner = pred.knn, task = task.highearner, resampling = resampleStrat, measures=list(gmean))
sampleResults.svm   <- resample(learner = pred.svm, task = task.highearner, resampling = resampleStrat, measures=list(gmean))

finalModel.tree  <- train(learner = pred.trees, task = task.highearner)
finalModel.logit <- train(learner = pred.logit, task = task.highearner)
finalModel.nn    <- train(learner = pred.nn, task = task.highearner)
finalModel.knn   <- train(learner = pred.knn, task = task.highearner)
finalModel.nb    <- train(learner = nb, task = task.highearner)
finalModel.svm   <- train(learner = pred.svm, task = task.highearner)

prediction.test.tree  <- predict(finalModel.tree, newdata = income.test)
prediction.test.logit <- predict(finalModel.logit, newdata = income.test)
prediction.test.nn    <- predict(finalModel.nn, newdata = income.test)
prediction.test.knn   <- predict(finalModel.knn, newdata = income.test)
prediction.test.nb    <- predict(finalModel.nb, newdata = income.train)
prediction.test.svm   <- predict(finalModel.svm, newdata = income.test)

performance(prediction.test.tree, measures = list(f1, gmean))
performance(prediction.test.logit, measures = list(f1, gmean))
performance(prediction.test.nn, measures = list(f1, gmean))
performance(prediction.test.knn, measures = list(f1, gmean))
performance(prediction.test.nb, measures = list(f1, gmean))
performance(prediction.test.svm, measures = list(f1, gmean))
