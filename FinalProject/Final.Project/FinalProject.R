library(tidyverse)
library(rpart)
library(randomForest)
library(magrittr)
library(mvtnorm)
library(nnet)
library(caret)
library(naivebayes)
library(microbenchmark)
library(ROCRdemo)
library(stargazer)
library(mlbench)

set.seed(12345)

selection <- function(N=1000){
    X <- rnorm(N)
    Z <- rnorm(N)
    err <- rmvnorm(N,mean=c(0,0),sigma=cbind(c(.5,.7),c(.7,1)))
    epsilon <- err[,1]
    eta <- err[,2]
    beta.1 <- -1
    gamma.1 <- 1.5
    gamma.2 <- -2
    Y <- beta.1*X + epsilon
    U <- gamma.1*Z + gamma.2*X + eta
    Data <- tibble(X,Z,Y,U,epsilon,eta)
    Data %<>% mutate(d=as.numeric(U>0))
    Data %<>% mutate(d=as.factor(d))
    Data %<>% mutate(obsY = Y) %>%
              mutate(obsY = replace(obsY,d==0,NA))
    Selected <- Data %>% filter(d==1) %>% select(X:eta)
    lm.1 <- lm(Y    ~ X, Data)
    lm.2 <- lm(obsY ~ X, Data)
    print(summary(lm.1))
    print(paste0("N = ",nobs(lm.1)))
    print(summary(lm.2))
    print(paste0("N = ",nobs(lm.2)))
    return(Data)
}

stargazer(lm.1,lm.2)

N <- 100000
Data <- selection(N)

#logit
est.logit <- glm(d ~ X + Z, data=Data, family=binomial(link="logit"))
Data %<>% mutate(pred.selection = predict(est.logit, newdata=Data, type="response"))
Data %<>% mutate(pred.1st.best = case_when(d==1 ~ pred.selection,d==0 ~ 1-pred.selection))
print(Data)
logit <- summary(lm(obsY ~ X + poly(pred.1st.best,3), data=Data))
logit <- lm(obsY ~ X + poly(pred.1st.best,3), data=Data)

#random forest
est.rf <- randomForest(d ~ X + Z, data = Data)
ll <- predict(est.rf, newdata=Data)
p.rf <- predict(est.rf, newdata=Data, type="prob")
p.rf <- p.rf[,-1]
Data %<>% mutate(pred.selection = p.rf)
Data %<>% mutate(pred.1st.best = case_when(d==1 ~ pred.selection,d==0 ~ 1-pred.selection))
summary(lm(obsY ~ X + poly(pred.1st.best,3), data=Data)) %>% print
random.forest <- lm(obsY ~ X + poly(pred.1st.best,3), data=Data)
pp <- Data$pred.1st.best

#ROC-AUC comparisons
pred <- prediction(est.rf, ll)
perf <- performance(pred,"tpr","fpr")
plot(perf)

#nueral net
est.nnet <- train(d ~ X + Z, data = Data, method='nnet',               trControl=trainControl(method='cv'))
p.nnet <- predict(est.nnet, newdata=Data, type='prob')
p.nnet <- p.nnet[,-1]
Data %<>% mutate(pred.selection = p.nnet)
Data %<>% mutate(pred.1st.best = case_when(d==1 ~ pred.selection,d==0 ~ 1-pred.selection))
nueral.net <- summary(lm(obsY ~ X + poly(pred.1st.best,3), data=Data)) %>% print
nueral.net <- lm(obsY ~ X + poly(pred.1st.best,3), data=Data)

#naivebayes
est.nbayes <- naive_bayes(d ~ X + Z, data = Data)
p.nbayes <- predict(est.nbayes, newdata = Data, type = "prob")
p.nbayes <- p.nbayes[,-1]
Data %<>% mutate(pred.selection = p.nbayes)
Data %<>% mutate(pred.1st.best = case_when(d==1 ~ pred.selection,d==0 ~ 1-pred.selection))
summary(lm(obsY ~ X + poly(pred.1st.best,3), data=Data)) %>% print
naive.bayes <- lm(obsY ~ X + poly(pred.1st.best,3), data=Data)

#compare computation times
times <- microbenchmark(
logit = glm(d ~ X + Z, data=Data, family=binomial(link="logit")), randomforest =
randomForest(d ~ X + Z, data = Data),
nueralnet =
train(d ~ X + Z, data = Data, method='nnet', trControl=trainControl(method='cv')),
naivebayes =
naive_bayes(d ~ X + Z, data = Data), times=100)
autoplot(times)

#Output Tables
stargazer(logit,naive.bayes,nueral.net,random.forest, font.size = "small", single.row = TRUE, column.sep.width = "1pt")
