setwd("~/Documents/git/DataAnalysis3")
rm(list = ls())
library(nnet)
library(darch)
library(deepnet)
library(caret)
library(doMC)
library(MASS)
library(h2o)
library(gbm)
library(plyr)
library(randomForest)
library(e1071)
library(ipred)
library(party)
library(mboost)
library(bst)
library(ff)
doMC::registerDoMC(cores=4)
data("Carseats")
data("Pima.tr")
data("Pima.te")
flist <- list("dig_test.RData", "dig_train.RData")
load(flist[[1]])
load(flist[[2]])
rm(flist)
dig_train <- data.frame(dig_train)
dig_train.raw <- dig_train
dig_test.raw <- dig_test

# 1.
set.seed(5262562)
trainIndex <- createDataPartition(Carseats$Sales, p=.75, list=F)
Carseats.train <- Carseats[trainIndex, ]
Carseats.test <- Carseats[-trainIndex, ]
ctrl <- trainControl(method = "cv",
                     number = 10)
nnet.grid <- expand.grid(.decay = c(0.5, 0.1, 0.005, 0.001), 
                         .size = 1:10)
nnet.fit.Carseats <- train(Sales ~ ., 
                           data = Carseats.train,
                           method = "nnet", 
                           maxit = 1000,
                           trControl = ctrl,
                           tuneGrid = nnet.grid, 
                           preProcess = c("center", "scale"),
                           linout = TRUE, 
                           trace = FALSE)
rf.fit.Carseats <- train(Sales ~ ., 
                         data = Carseats.train,
                         method = "rf", 
                         maxit = 1000,
                         trControl = ctrl,
                         tuneLength = 10,
                         preProcess = c("center", "scale"))
boost.grid <- expand.grid(.n.trees = seq(0, 1000, 200), 
                          .interaction.depth = 1:3, 
                          .shrinkage = c(0.1, 0.01, 0.001), 
                          .n.minobsinnode = c(5, 10, 20))
boost.fit.Carseats <- train(Sales ~ ., 
                            data = Carseats.train,
                            method ='gbm',
                            preProc = c('center','scale'),
                            tuneGrid = boost.grid, 
                            trControl = ctrl)
bagging.fit.Carseats <- train(Sales ~ ., 
                              data = Carseats.train,
                              method = "treebag", 
                              maxit = 1000,
                              trControl = ctrl,
                              preProcess = c("center", "scale"))
resamps <- resamples(list(ANN = nnet.fit.Carseats, 
                          randomForest = rf.fit.Carseats, 
                          GradientBoosting = boost.fit.Carseats, 
                          BaggedCART = bagging.fit.Carseats))
ANN.pred.Carseats <- predict(nnet.fit.Carseats, newdata = Carseats.test)
rf.pred.Carseats <- predict(rf.fit.Carseats, newdata = Carseats.test)
boost.pred.Carseats <- predict(boost.fit.Carseats, newdata = Carseats.test)
bagging.pred.Carseats <- predict(bagging.fit.Carseats, newdata = Carseats.test)
pred.Carseats <-matrix(c(ANN.pred.Carseats, 
                         rf.pred.Carseats, 
                         boost.pred.Carseats, 
                         bagging.pred.Carseats), 
                       ncol = 4, 
                       byrow = FALSE)
mse <- function(r){mean(r^2)}
mse.Carseats <- apply(pred.Carseats - Carseats.test$Sales, 2, mse)
names(mse.Carseats) <- c("ANN", "randomForest", "GradientBoosting", "BaggedCART")


#2
ctrl <- trainControl(method = "cv",
                     number = 10, 
                     classProbs = TRUE)
nnet.grid <- expand.grid(.decay = c(0.5, 0.1, 0.005, 0.001), 
                         .size = 1:10)
nnet.fit.pima <- train(type ~ ., 
                       data = Pima.tr,
                       method = "nnet", 
                       maxit = 1000,
                       trControl = ctrl,
                       tuneGrid = nnet.grid, 
                       preProcess = c("center", "scale"),
                       trace = FALSE)
rf.fit.pima <- train(type ~ ., 
                     data = Pima.tr,
                     method = "rf", 
                     maxit = 1000,
                     trControl = ctrl,
                     tuneLength = 10,
                     preProcess = c("center", "scale"))
boost.grid <- expand.grid(.n.trees = seq(0, 1000, 200), 
                          .interaction.depth = 1:3, 
                          .shrinkage = c(0.1, 0.01, 0.001), 
                          .n.minobsinnode = c(5, 10, 20))
boost.fit.pima <- train(type ~ ., 
                        data = Pima.tr,
                        method = 'gbm',
                        preProc = c('center','scale'),
                        tuneGrid = boost.grid, 
                        trControl = ctrl)
bagging.fit.pima <- train(type ~ ., 
                          data = Pima.tr,
                          method = "treebag", 
                          maxit = 1000,
                          trControl = ctrl,
                          preProcess = c("center", "scale"))
resamps <- resamples(list(ANN = nnet.fit.pima, 
                          randomForest = rf.fit.pima, 
                          GradientBoosting = boost.fit.pima, 
                          BaggedCART = bagging.fit.pima))
ANN.pred.pima <- predict(nnet.fit.pima, newdata = Pima.te)
rf.pred.pima <- predict(rf.fit.pima, newdata = Pima.te)
boost.pred.pima <- predict(boost.fit.pima, newdata = Pima.te)
bagging.pred.pima <- predict(bagging.fit.pima, newdata = Pima.te)
pred.pima <-matrix(c(ANN.pred.pima, 
                     rf.pred.pima, 
                     boost.pred.pima, 
                     bagging.pred.pima), 
                   ncol = 4, 
                   byrow = FALSE)
er <- function(wrong){mean(abs(wrong))}
er.pima <- apply(pred.pima - as.numeric(Pima.te$type), 2, er)
names(er.pima) <- c("ANN", "randomForest", "GradientBoosting", "BaggedCART")


#3.
#deepnet
inputs.train <- model.matrix(Sales ~ ., data = Carseats.train)
inputs.test <- model.matrix(Sales ~ ., data = Carseats.test)
models <- c()
models.mse <- c()
for (i in 1:20){
        rand_numlayers <- sample(2:5, 1)
        rand_hidden <- c(sample(10:50, rand_numlayers, T))
        rand_dropout <- runif(1, 0, 0.6)
        rand_learningrate <- runif(1, 0.6, 1)
        dnn.fit.Carseats <- dbn.dnn.train(inputs.train, 
                                          Carseats.train[, 1], 
                                          hidden = rand_hidden, 
                                          activationfun = "sigm",
                                          output = "linear",
                                          hidden_dropout = rand_dropout, 
                                          learningrate = rand_learningrate,
                                          visible_dropout = 0)
        dnn.pred.Carseats <- nn.predict(dnn.fit.Carseats,
                                        inputs.test)
        dnn.mse.Carseats <- mean((dnn.pred.Carseats - Carseats.test[, 1])^2)
        models <- c(models, dnn.pred.Carseats)
        models.mse <- c(models.mse, dnn.mse.Carseats)
}
best.err <- models.mse[1]
for (i in 1:length(models)) {
        err <- models.mse[1]
        if (err < best_err) {
                best_err <- err
                best_model <- models[[i]]
        }
}
dnn.pred.Carseats <- best_model
dnn.mse.Carseats <- best_err

# darch
darch.fit.Carseats <- newDArch(dnn.pred.Carseats$hidden, 
                               batchSize=4,
                               ff=F)
darch.fit.Carseats <- preTrainDArch(darch.fit.Carseats,
                                    inputs.train,
                                    maxEpoch = 200,
                                    numCD = 4)
layers <- getLayers(darch.fit.Carseats)
for(i in length(layers):1){
        layers[[i]][[2]] <- sigmoidUnitDerivative
}
setLayers(darch.fit.Carseats) <- layers
rm(layers)
setFineTuneFunction(darch.fit.Carseats) <- rpropagation
darch.fit.Carseats <- fineTuneDArch(darch.fit.Carseats,
                                    trainData = inputs.train,
                                    targetData = outputs.train,
                                    maxEpoch = 200,
                                    isBin = T)

# Running the darch
darch.pred.Carseats <- 
        darch.pred.Carseats <- 
        getExecuteFunction(darch.fit.Carseats)(darch.fit.Carseats,inputs.test)
outputs2 <- getExecOutputs(darch.pred.Carseats)








#4.


# 5
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
dig_train[, 785] <- as.factor(dig_train[, 785])
colnames(dig_train) <- 1:785
colnames(dig_test) <- 1:785
mnist.train <- as.h2o(localH2O, dig_train)
mnist.test <- as.h2o(localH2O, dig_test)
models <- c()
for (i in 1:10) {
        rand_activation <- c("TanhWithDropout", "RectifierWithDropout")[sample(1:2,1)]
        rand_numlayers <- sample(2:5,1)
        rand_hidden <- c(sample(10:50,rand_numlayers,T))
        rand_l1 <- runif(1, 0, 1e-3)
        rand_l2 <- runif(1, 0, 1e-3)
        rand_dropout <- c(runif(rand_numlayers, 0, 0.6))
        rand_input_dropout <- runif(1, 0, 0.5)
        dlmodel <- h2o.deeplearning(x = 1:784, 
                                    y = 785, 
                                    training_frame = mnist.train, 
                                    validation_frame = mnist.test,
                                    epochs = 0.1,
                                    activation = rand_activation, 
                                    hidden = rand_hidden, 
                                    l1 = rand_l1, 
                                    l2 = rand_l2,
                                    input_dropout_ratio = rand_input_dropout, 
                                    hidden_dropout_ratios = rand_dropout)
        models <- c(models, dlmodel)
}
best_err <- models[[1]]@model$valid_class_error #best model from grid search above
for (i in 1:length(models)) {
        err <- models[[i]]@model$valid_class_error
        if (err < best_err) {
                best_err <- err
                best_model <- models[[i]]
        }
}
best_model



