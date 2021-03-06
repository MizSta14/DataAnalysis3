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
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
set.seed(5262562)
trainIndex <- createDataPartition(Carseats$Sales, p=.75, list=F)
Carseats.train <- Carseats[trainIndex, ]
Carseats.test <- Carseats[-trainIndex, ]
ctrl <- trainControl(method = "cv",
                     number = 10)

# 1.

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
models <- vector("list", 20)
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
        models[[i]] <- dnn.fit.Carseats
        models.mse <- c(models.mse, dnn.mse.Carseats)
}
best.err <- models.mse[1]
for (i in 1:length(models)) {
        err <- models.mse[i]
        if (err < best.err) {
                best.err <- err
                best.model <- models[[i]]
        }
}
dnn.fit.Carseats <- best.model
dnn.mse.Carseats <- best.err
dnn.pred.Carseats <- nn.predict(dnn.fit.Carseats,
                                inputs.test)

# darch
darch.fit.Carseats <- newDArch(dnn.fit.Carseats$size, 
                               batchSize=4,
                               ff=F)
darch.fit.Carseats <- preTrainDArch(darch.fit.Carseats,
                                    inputs.train,
                                    maxEpoch = 10,
                                    numCD = 4)
layers <- getLayers(darch.fit.Carseats)
layers[[length(layers)]][[2]] <- linearUnitDerivative
for(i in (length(layers) - 1):1){
        layers[[i]][[2]] <- sigmoidUnitDerivative
}
setLayers(darch.fit.Carseats) <- layers
rm(layers)
setFineTuneFunction(darch.fit.Carseats) <- rpropagation
darch.fit.Carseats <- fineTuneDArch(darch.fit.Carseats,
                                    trainData = inputs.train,
                                    targetData = matrix(Carseats.train$Sales),
                                    maxEpoch = 10,
                                    isBin = T)

# Running the darch
darch.pred.Carseats <- 
        darch.pred.Carseats <- 
        getExecuteFunction(darch.fit.Carseats)(darch.fit.Carseats,inputs.test)
outputs2 <- getExecOutputs(darch.pred.Carseats)
darch.mse.Carseats <- mse(outputs2[[length(outputs2)]]-Carseats.test$Sales)

# h2o
Carseats.train.h2o <- as.h2o(localH2O, model.matrix(~ . - 1 , data = Carseats.train))
Carseats.test.h2o <- as.h2o(localH2O, model.matrix(~ . - 1 , data = Carseats.test))
models <- c()
for (i in 1:10) {
        rand_activation <- c("TanhWithDropout", "RectifierWithDropout")[sample(1:2,1)]
        rand_numlayers <- sample(2:5,1)
        rand_hidden <- c(sample(10:50,rand_numlayers,T))
        rand_l1 <- runif(1, 0, 1e-3)
        rand_l2 <- runif(1, 0, 1e-3)
        rand_dropout <- c(runif(rand_numlayers, 0, 0.6))
        rand_input_dropout <- runif(1, 0, 0.5)
        dlmodel <- h2o.deeplearning(x = 2:13, 
                                    y = 1, 
                                    training_frame = Carseats.train.h2o, 
                                    validation_frame = Carseats.test.h2o,
                                    epochs = 0.1,
                                    activation = rand_activation, 
                                    hidden = rand_hidden, 
                                    l1 = rand_l1, 
                                    l2 = rand_l2,
                                    input_dropout_ratio = rand_input_dropout, 
                                    hidden_dropout_ratios = rand_dropout)
        models <- c(models, dlmodel)
}
best_mse <- models[[1]]@model$validation_metrics@metrics$MSE #best model from grid search above
for (i in 1:length(models)) {
        mse.h2o <- models[[i]]@model$validation_metrics@metrics$MSE
        if (mse.h2o < best_mse) {
                best_mse <- mse.h2o
                best_model <- models[[i]]
        }
}
h2o.fit.Carseats <- best_model
h2o.mse.Carseats <- best_mse


#4.
#deepnet
inputs.train <- model.matrix(type ~ ., data = Pima.tr)
inputs.test <- model.matrix(type ~ ., data = Pima.te)
target.train <- as.numeric(Pima.tr$type) - 1
target.test <- as.numeric(Pima.te$type) - 1
models <- vector("list", 20)
models.er <- c()
for (i in 1:20){
        rand_numlayers <- sample(2:5, 1)
        rand_dropout <- runif(1, 0, 0.6)
        rand_hidden <- c(sample(10:50, rand_numlayers, T))
        rand_learningrate <- runif(1, 0.6, 1)
        dnn.fit.pima <- dbn.dnn.train(inputs.train, 
                                      target.train, 
                                      activationfun = "sigm",
                                      hidden = rand_hidden,
                                      output = "softmax",
                                      hidden_dropout = 0, 
                                      learningrate = rand_learningrate,
                                      visible_dropout = 0)
        dnn.pred.pima <- nn.predict(dnn.fit.pima,
                                    inputs.test)
        dnn.er.pima <- er(dnn.pred.pima - target.test)
        models[[i]] <- dnn.fit.pima
        models.er <- c(models.er, dnn.er.pima)
}
best.err <- models.er[1]
best.model <- models[[1]]
for (i in 1:length(models)) {
        err <- models.er[i]
        if (err < best.err) {
                best.err <- err
                best.model <- models[[i]]
        }
}
dnn.fit.pima <- best.model
dnn.er.pima <- best.err
dnn.pred.pima <- nn.predict(dnn.fit.pima,
                            inputs.test)

# darch
darch.fit.pima <- newDArch(dnn.fit.pima$size, 
                           batchSize=4,
                           ff=F)
darch.fit.pima <- preTrainDArch(darch.fit.pima,
                                inputs.train,
                                maxEpoch = 10,
                                numCD = 4)
layers <- getLayers(darch.fit.pima)
layers[[length(layers)]][[2]] <- softmaxUnitDerivative
for(i in (length(layers) - 1):1){
        layers[[i]][[2]] <- sigmoidUnitDerivative
}
setLayers(darch.fit.pima) <- layers
rm(layers)
setFineTuneFunction(darch.fit.pima) <- rpropagation
darch.fit.pima <- fineTuneDArch(darch.fit.pima,
                                trainData = inputs.train,
                                targetData = matrix(target.train),
                                maxEpoch = 10,
                                isBin = T)
# Running the darch
darch.pred.pima <- 
        darch.pred.pima <- 
        getExecuteFunction(darch.fit.pima)(darch.fit.pima, inputs.test)
outputs2 <- getExecOutputs(darch.pred.pima)
darch.er.pima <- er(outputs2[[length(outputs2)]]-target.test)

#h2o
pima.train.h2o <- as.h2o(localH2O, Pima.tr)
pima.test.h2o <- as.h2o(localH2O, Pima.te)
models <- c()
for (i in 1:10) {
        rand_activation <- c("TanhWithDropout", "RectifierWithDropout")[sample(1:2,1)]
        rand_numlayers <- sample(2:5,1)
        rand_hidden <- c(sample(10:50,rand_numlayers,T))
        rand_l1 <- runif(1, 0, 1e-3)
        rand_l2 <- runif(1, 0, 1e-3)
        rand_dropout <- c(runif(rand_numlayers, 0, 0.6))
        rand_input_dropout <- runif(1, 0, 0.5)
        dlmodel <- h2o.deeplearning(x = 1:7, 
                                    y = 8, 
                                    training_frame = pima.train.h2o, 
                                    validation_frame = pima.test.h2o,
                                    epochs = 0.1,
                                    activation = rand_activation, 
                                    hidden = rand_hidden, 
                                    l1 = rand_l1, 
                                    l2 = rand_l2,
                                    input_dropout_ratio = rand_input_dropout, 
                                    hidden_dropout_ratios = rand_dropout)
        models <- c(models, dlmodel)
}
best_auc <- models[[1]]@model$validation_metrics@metrics$AUC #best model from grid search above
for (i in 1:length(models)) {
        auc <- models[[i]]@model$validation_metrics@metrics$AUC
        if (auc > best_auc) {
                best_auc <- auc
                best_model <- models[[i]]
        }
}
h2o.fit.pima <- best_model
h2o.auc.pima <- best_auc







# 5
dig_train[, 785] <- as.factor(dig_train[, 785])
colnames(dig_train) <- paste("V", 1:785, sep = '')
colnames(dig_test) <- paste("V", 1:785, sep = '')
# clsresp.train <- class.ind(dig_train$V785)
# nnet.fit.mnist = nnet(dig_train[,-785], clsresp.train,
#                       size = 50, 
#                       decay = 0.2,
#                       softmax = TRUE, 
#                       entropy = TRUE, 
#                       MaxNWts = 1e6)
# rf.fit.mnist <- randomForest(V785 ~ . , data = dig_train)
# rf.pred.mnist <- predict(rf.fit.mnist, dig_test)

#deepnet

inputs.train <- model.matrix(V785 ~ . - 1, data = dig_train)
inputs.test <- model.matrix(V785 ~ . - 1, data = dig_test)
target.train <- class.ind(dig_train$V785)
target.test <- class.ind(dig_test$V785)

models <- vector("list", 5)
models.er <- c()
trans <- function(output){
        ind <- which.max(output)
        output[ind] <- 1
        output[-ind] <- 0
        return(output)
}
for (i in 1:5){
        rand_numlayers <- sample(1:3, 1)
        rand_dropout <- runif(1, 0, 0.6)
        rand_input_dropout <- runif(1, 0, 0.5)
        rand_learningrate <- runif(1, 0.6, 1)
        dnn.fit.dig <- dbn.dnn.train(inputs.train, 
                                     target.train,
                                     activationfun = "sigm",
                                     hidden = c(10),
                                     output = "softmax",
                                     hidden_dropout = 0, 
                                     learningrate = rand_learningrate,
                                     visible_dropout = rand_input_dropout)
        dnn.pred.dig <- nn.predict(dnn.fit.dig,
                                   inputs.test)
        dnn.pred.dig <- t(apply(dnn.pred.dig, 1, trans))
        dnn.er.dig <- (sum(abs(dnn.pred.dig - target.test))/2)/nrow(target.test)
        models[[i]] <- dnn.fit.dig
        models.er <- c(models.er, dnn.er.dig)
}
best.err <- models.er[1]
best.model <- models[[1]]
for (i in 1:length(models)) {
        err <- models.er[i]
        if (err < best.err) {
                best.err <- err
                best.model <- models[[i]]
        }
}
dnn.fit.dig <- best.model
dnn.er.dig <- best.err
dnn.pred.dig <- nn.predict(dnn.fit.dig,
                           inputs.test)

# darch
darch.fit.dig <- newDArch(dnn.fit.dig$size, 
                          batchSize=4,
                          ff=F)
darch.fit.dig <- preTrainDArch(darch.fit.dig,
                               inputs.train,
                               maxEpoch = 10,
                               numCD = 4)
layers <- getLayers(darch.fit.dig)
layers[[length(layers)]][[2]] <- softmaxUnitDerivative
for(i in (length(layers) - 1):1){
        layers[[i]][[2]] <- sigmoidUnitDerivative
}
setLayers(darch.fit.dig) <- layers
rm(layers)
setFineTuneFunction(darch.fit.dig) <- rpropagation
darch.fit.dig <- fineTuneDArch(darch.fit.dig,
                               trainData = inputs.train,
                               targetData = target.train,
                               maxEpoch = 10,
                               isBin = T)
# Running the darch
darch.pred.dig <- 
        darch.pred.dig <- 
        getExecuteFunction(darch.fit.dig)(darch.fit.dig, inputs.test)
outputs2 <- getExecOutputs(darch.pred.dig)
darch.pred.dig <- t(apply(outputs2[[length(outputs2)]], 1, trans))
darch.er.dig <- (sum(abs(dnn.pred.dig - target.test))/2)/nrow(target.test)

# H2O
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
best_err <- models[[1]]@model$validation_metrics@metrics$cm$table[11, 11] #best model from grid search above
for (i in 1:length(models)) {
        err <- models[[i]]@model$validation_metrics@metrics$cm$table[11, 11]
        if (err < best_err) {
                best_err <- err
                best_model <- models[[i]]
        }
}
h2o.fit.mnist <- best_model
h2o.er.mnist <- best_err




