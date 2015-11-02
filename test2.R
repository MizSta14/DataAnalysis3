setwd("~/Documents/git/DataAnalysis3")
rm(list = ls())
set.seed(1)
library(e1071)
library(ISLR)
library(penalized)
library(caret)
library(caTools)
library(ROCR)
library(doMC)
library(glmnet)
attach(OJ)
doMC::registerDoMC(cores=4)
Cost <- c(0.01, 0.05, 0.1, 0.5, 1, 5, 10)
Gamma <- c(0.001, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 100)
l1 <- seq(0, 1, 0.25)
l2 <- seq(0, 1, 0.25)
Degree <- 2
train.ind <- sample(1:nrow(OJ), 800)
train <- OJ[train.ind, ]
test <- OJ[-train.ind, ]
ctrl <- tune.control(sampling = "cross", 
                     cross = 10)

# 1
kernel.func <- "linear"
tune.svm.lin <- tune(svm, 
                     Purchase ~ . , 
                     data = train, 
                     ranges = list(kernel = kernel.func,
                                   cost = Cost),
                     tunecontrol = ctrl
)
cver.svm.lin <- tune.svm.lin$performances
svm.lin <- svm(Purchase ~ . ,
               data = train, 
               kernel = kernel.func, 
               cost = tune.svm.lin$best.parameters$cost, 
               decision.values = TRUE)
svm.pred.lin.tr <- predict(svm.lin, 
                           newdata = train, 
                           decision.values = TRUE)
svm.pred.lin.te <- predict(svm.lin, 
                           newdata = test, 
                           decision.values = TRUE)
cm.lin.tr <- confusionMatrix(svm.pred.lin.tr, train$Purchase)
cm.lin.te <- confusionMatrix(svm.pred.lin.te, test$Purchase)

# 2

kernel.func <- "polynomial"
tune.svm.poly <- tune(svm, 
                      Purchase ~ . , 
                      data = train, 
                      ranges = list(kernel = kernel.func,
                                    cost = Cost, 
                                    degree = Degree),
                      tunecontrol = ctrl
)
cver.svm.poly <- tune.svm.poly$performances
svm.poly <- svm(Purchase ~ . ,
                data = train, 
                kernel = kernel.func, 
                cost = tune.svm.poly$best.parameters$cost, 
                degree = Degree,
                decision.values = TRUE)
svm.pred.poly.tr <- predict(svm.poly, 
                            newdata = train, 
                            decision.values = TRUE)
svm.pred.poly.te <- predict(svm.poly, 
                            newdata = test, 
                            decision.values = TRUE)
cm.poly.tr <- confusionMatrix(svm.pred.poly.tr, train$Purchase)
cm.poly.te <- confusionMatrix(svm.pred.poly.te, test$Purchase)

# 3
kernel.func <- "radial"
tune.svm.rbf <- tune(svm, 
                     Purchase ~ . , 
                     data = train, 
                     ranges = list(kernel = kernel.func,
                                   cost = Cost, 
                                   gamma = Gamma),
                     tunecontrol = ctrl
)
cver.svm.rbf <- tune.svm.rbf$performances
svm.rbf <- svm(Purchase ~ . ,
               data = train, 
               kernel = kernel.func, 
               cost = tune.svm.rbf$best.parameters$cost, 
               gamma = tune.svm.rbf$best.parameters$gamma, 
               decision.values = TRUE)
svm.pred.rbf.tr <- predict(svm.rbf, 
                           newdata = train, 
                           decision.values = TRUE)
svm.pred.rbf.te <- predict(svm.rbf, 
                           newdata = test, 
                           decision.values = TRUE)
cm.rbf.tr <- confusionMatrix(svm.pred.rbf.tr, train$Purchase)
cm.rbf.te <- confusionMatrix(svm.pred.rbf.te, test$Purchase)


# 4
kernel.func <- "sigmoid"
tune.svm.sigm <- tune(svm, 
                      Purchase ~ . , 
                      data = train, 
                      ranges = list(kernel = kernel.func,
                                    cost = Cost, 
                                    gamma = Gamma),
                      tunecontrol = ctrl
)
cver.svm.sigm <- tune.svm.sigm$performances
svm.sigm <- svm(Purchase ~ . ,
                data = train, 
                kernel = kernel.func, 
                cost = tune.svm.sigm$best.parameters$cost, 
                gamma = tune.svm.sigm$best.parameters$gamma, 
                decision.values = TRUE)
svm.pred.sigm.tr <- predict(svm.sigm, 
                            newdata = train, 
                            decision.values = TRUE)
svm.pred.sigm.te <- predict(svm.sigm, 
                            newdata = test, 
                            decision.values = TRUE)
cm.sigm.tr <- confusionMatrix(svm.pred.sigm.tr, train$Purchase)
cm.sigm.te <- confusionMatrix(svm.pred.sigm.te, test$Purchase)

# 5
logis.fit <- glm(Purchase ~ . , 
                 data = train, 
                 family = binomial(logit))
logis.pred.tr <- predict.glm(logis.fit, 
                             newdata = train, 
                             type = "response")
logis.pred.te <- predict.glm(logis.fit, 
                             newdata = test, 
                             type = "response")
cm.logis.tr <- confusionMatrix(as.factor(ifelse(logis.pred.tr >= 0.5, 
                                                "MM", 
                                                "CH")),
                               train$Purchase)
cm.logis.te <- confusionMatrix(as.factor(ifelse(logis.pred.te >= 0.5, 
                                                "MM", 
                                                "CH")),
                               test$Purchase)


# 5 
op <- par(mfrow = c(1, 2), 
          oma = c(0, 0, 2, 0))
rocplot=function(pred, truth, ...){
        predob = prediction (pred, truth)
        perf = performance (predob , "tpr", "fpr") 
        plot(perf ,...)
}
rocplot(attributes(svm.pred.lin.tr)$decision.values, 
        train$Purchase, 
        main="Training Data")
rocplot(attributes(svm.pred.lin.te)$decision.values, 
        test$Purchase, 
        main="Test Data")
mtext("ROC for Linear SVM", outer = TRUE, cex = 1.5)
rocplot(attributes(svm.pred.poly.tr)$decision.values, 
        train$Purchase, 
        main="Training Data")
rocplot(attributes(svm.pred.poly.te)$decision.values, 
        test$Purchase, 
        main="Test Data")
mtext("ROC for Polynomial SVM", outer = TRUE, cex = 1.5)
rocplot(attributes(svm.pred.rbf.tr)$decision.values, 
        train$Purchase, 
        main="Training Data")
rocplot(attributes(svm.pred.rbf.te)$decision.values, 
        test$Purchase, 
        main="Test Data")
mtext("ROC for Radial Basis SVM", outer = TRUE, cex = 1.5)
rocplot(attributes(svm.pred.sigm.tr)$decision.values, 
        train$Purchase, 
        main="Training Data")
rocplot(attributes(svm.pred.sigm.te)$decision.values, 
        test$Purchase, 
        main="Test Data")
mtext("ROC for NN SVM", outer = TRUE, cex = 1.5)
rocplot(logis.pred.tr, 
        train$Purchase, 
        main="Training Data")
rocplot(logis.pred.te, 
        test$Purchase, 
        main="Test Data")
mtext("ROC for Logistic Regression", outer = TRUE, cex = 1.5)
auc.lin.te <- colAUC(attributes(svm.pred.lin.te)$decision.values, test$Purchase)
auc.poly.te <- colAUC(attributes(svm.pred.poly.te)$decision.values, test$Purchase)
auc.rbf.te <- colAUC(attributes(svm.pred.rbf.te)$decision.values, test$Purchase)
auc.sigm.te <- colAUC(attributes(svm.pred.sigm.te)$decision.values, test$Purchase)
auc.logis.te <- colAUC(logis.pred.te, test$Purchase)
auc.lin.tr <- colAUC(attributes(svm.pred.lin.tr)$decision.values, train$Purchase)
auc.poly.tr <- colAUC(attributes(svm.pred.poly.tr)$decision.values, train$Purchase)
auc.rbf.tr <- colAUC(attributes(svm.pred.rbf.tr)$decision.values, train$Purchase)
auc.sigm.tr <- colAUC(attributes(svm.pred.sigm.tr)$decision.values, train$Purchase)
auc.logis.tr <- colAUC(logis.pred.tr, train$Purchase)
aucs <- matrix(c(auc.lin.tr, auc.poly.tr, auc.rbf.tr, auc.sigm.tr, auc.logis.tr, 
                 auc.lin.te, auc.poly.te, auc.rbf.te, auc.sigm.te, auc.logis.te), 
               ncol = 2, 
               dimnames = list(c("Linear SVM", 
                                 "Polynomial SVM", 
                                 "Radial Basis SVM", 
                                 "NN SVM", 
                                 "Logistic Regression"), 
                               c("Training", "Test")
               )
)


# 6
train.input <- model.matrix(Purchase ~ ., data = train)
test.input <- model.matrix(Purchase ~ ., data = test)
penal.grid <- expand.grid(l1 = c(0.01, 0.05, 0.1, 0.5, 1, 5, 10), 
                          l2 = c(0.01, 0.05, 0.1, 0.5, 1, 5, 10))
cver <- numeric(length(penal.grid))
for (i in 1:nrow(penal.grid)){
        cver[i] <- cvl(response = train$Purchase, 
                       penalized = train.input,
                       lambda1 = penal.grid[i, 1],
                       lambda2 = penal.grid[i, 2],
                       model = "logistic", 
                       trace = FALSE, 
                       fold = 10)$cvl
}
ind <- which.min(cver)
penlogis.fit <- penalized(response = train$Purchase, 
                          penalized = train.input, 
                          model = "logistic", 
                          lambda1 = penal.grid[ind, 1], 
                          lambda2 = penal.grid[ind, 1])
penlogis.pred.tr <- predict(penlogis.fit, 
                            penalized = train.input)
cm.penlogis.tr <- confusionMatrix(as.factor(ifelse(penlogis.pred.tr >= 0.5, 
                                                   "MM", 
                                                   "CH")),
                                  train$Purchase)
penlogis.pred.te <- predict(penlogis.fit, 
                            penalized = test.input)
cm.penlogis.te <- confusionMatrix(as.factor(ifelse(penlogis.pred.te >= 0.5, 
                                                   "MM", 
                                                   "CH")),
                                  test$Purchase)
rocplot(penlogis.pred.tr, 
        train$Purchase, 
        main="Training Data")
rocplot(penlogis.pred.te, 
        test$Purchase, 
        main="Test Data")
mtext("ROC for Penalized Logistic Regression", outer = TRUE, cex = 1.5)
auc.penlogis.tr <- colAUC(penlogis.pred.tr, train$Purchase)
auc.penlogis.te <- colAUC(penlogis.pred.te, test$Purchase)

# glmnet
a <- seq(0, 1, 0.01)
l <- seq(0, 1, 0.01)
cverr <- numeric(length(a))
lambda <- numeric(length(a))
for (i in 1:length(a)){
        cvglm <- cv.glmnet(train.input, 
                           train$Purchase, 
                           family = "binomial", 
                           type.measure = "class", 
                           lambda = l, 
                           alpha = a[i], 
                           parallel = TRUE)
        lambda[i] <- cvglm$lambda.min
        cverr[i] <- min(cvglm$cvm)
}
lambda <- lambda[which.min(cverr)]
a <- a[which.min(cverr)]
glmnet.fit <- glmnet(train.input, 
                     train$Purchase, 
                     family = "binomial", 
                     lambda = lambda, 
                     alpha = a)
glmnet.pred.tr <- predict(glmnet.fit, 
                          train.input,
                          type = "response")
cm.glmnet.tr <- confusionMatrix(as.factor(ifelse(penlogis.pred.tr >= 0.5, 
                                                   "MM", 
                                                   "CH")),
                                  train$Purchase)
glmnet.pred.te <- predict(glmnet.fit, 
                          test.input,
                          type = "response")
cm.glmnet.te <- confusionMatrix(as.factor(ifelse(penlogis.pred.te >= 0.5, 
                                                 "MM", 
                                                 "CH")),
                                test$Purchase)
rocplot(glmnet.pred.tr, 
        train$Purchase, 
        main="Training Data")
rocplot(glmnet.pred.te, 
        test$Purchase, 
        main="Test Data")
mtext("ROC for Penalized Logistic Regression", outer = TRUE, cex = 1.5)
auc.glmnet.tr <- colAUC(glmnet.pred.tr, train$Purchase)
auc.glmnet.te <- colAUC(glmnet.pred.te, test$Purchase)





























par(op)



