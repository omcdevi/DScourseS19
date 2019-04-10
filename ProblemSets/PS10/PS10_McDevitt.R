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
income.test <- income[test, ]
#task, cv, tune
task <- makeClassifTask(id = "taskname", data = income.train, target = 'high.earner')
fold <- makeResampleDesc(method="CV", iters =3)
tune <- makeTuneControlRandom(maxit = 10L)
#Making Learners
tree <- makeLearner("classif.rpart",predict.type = "response")
log <- makeLearner("classif.glmnet",predict.type = "response")
net <- makeLearner("classif.nnet",predict.type = "response")
bayes <- makeLearner("classif.naiveBayes",predict.type = "response")
knn <- makeLearner("classif.kknn",predict.type = "response")
svm <- makeLearner("classif.svm",predict.type = "response")
#Hyperparameters
param_set_tree <- makeParamSet(
      makeIntegerParam("minsplit", lower=10, upper=50),
      makeIntegerParam("minbucket", lower = 5, upper = 50),
      makeNumericParam("cp",lower=0.001,upper=0.2))
param_set_log <- makeParamSet(
      makeNumericParam("lambda", lower=0, upper=3),
      makeNumericParam("alpha", lower = 0, upper = 1))
param_set_net <- makeParamSet(
      makeIntegerParam("size", lower=1, upper=10),
      makeNumericParam("decay", lower = .1, upper = .5),
      makeIntegerParam("maxit",lower=1000,upper=1000))
param_set_knn <- makeParamSet(
      makeIntegerParam("k", lower=1, upper=30))
param_set_svm <- makeParamSet(
      makeDiscreteParam("kernel", values = c("radial")),
      makeDiscreteParam("cost", values = c(2^-2,2^-1,2^0,2^1,2^2,2^10)),
      makeDiscreteParam("gamma", values = c(2^-2,2^-1,2^0,2^1,2^2,2^10)))
#Tuning
tune_tree <- tuneParams(learner = tree, task = task, resampling = fold, measures = list(f1,gmean), par.set = param_set_tree, control = tune)
tune_log <- tuneParams(learner = log, task = task, resampling = fold, measures = list(f1,gmean), par.set = param_set_log, control = tune)
tune_net <- tuneParams(learner = net, task = task, resampling = fold, measures = list(f1,gmean), par.set = param_set_net, control = tune)
tune_knn <- tuneParams(learner = knn, task = task, resampling = fold, measures = list(f1,gmean), par.set = param_set_knn, control = tune)
tune_svm <- tuneParams(learner = svm, task = task, resampling = fold, measures = list(f1,gmean), par.set = param_set_svm, control = tune)
#Test models with optimal tuning
tree. <- setHyperPars(learner = tree, par.vals = tune_tree$x)
log. <- setHyperPars(learner = log, par.vals = tune_log$x)
net. <- setHyperPars(learner = net, par.vals = tune_net$x)
knn. <- setHyperPars(learner = knn, par.vals = tune_knn$x)
svm. <- setHyperPars(learner = svm, par.vals = tune_svm$x)
tree.final <- train(learner = tree.,task = task)
log.final <- train(learner = log.,task = task)
bayes.final <- train(learner = bayes, task = task)
net.final <- train(learner = net.,task = task)
knn.final <- train(learner = knn.,task = task)
svm.final <- train(learner = svm.,task=task)
tree.predict <- predict(tree.final, newdata = income.test)
log.predict <- predict(log.final, newdata = income.test)
bayes.predict <- predict(bayes.final, newdata = income.test)
net.predict <- predict(net.final, newdata = income.test)
knn.predict <- predict(knn.final, newdata = income.test)
svm.predict <- predict(svm.final, newdata = income.test)
performance.log <- (mean(as.numeric(log.predict$data$truth)) - mean(as.numeric(log.predict$data$response)))
performance.bayes <- (mean(as.numeric(bayes.predict$data$truth)) - mean(as.numeric(bayes.predict$data$response)))
performance.net <- (mean(as.numeric(net.predict$data$truth)) - mean(as.numeric(net.predict$data$response)))
performance.knn <- (mean(as.numeric(knn.predict$data$truth)) - mean(as.numeric(knn.predict$data$response)))
log.
net.
knn.
performance.log
performance.bayes
performance.net
performance.knn
