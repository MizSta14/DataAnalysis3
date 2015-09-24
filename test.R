setwd("~/Documents/git/DataAnalysis3")
rm(list = ls())
library(ISLR)
library(ggplot2)
library(boot)
library(glmnet)
library(leaps)
library(pls)
library(MASS)
library(plyr)
library(dplyr)

# 9
set.seed(1)
attach(College)
# (a)
set.seed(1)
dim(College)
train.ind <- sample(1:nrow(College), nrow(College)/2)
test.ind <- -train.ind
College.train <- College[train.ind, ]
College.test <- College[test.ind, ]

# (b).
set.seed(1)
lm.fit_1 <- lm(Apps ~ ., data = College.train)
lm.pred <- predict(lm.fit_1, newdata = College.test)
mean((lm.pred - College.test$Apps)^2)

# (c).
set.seed(1)
ridge.cv.out <- cv.glmnet(x = model.matrix(Apps ~ .,College.train)[,-2], 
                          y = College.train$Apps, 
                          alpha = 0)
ridge.bestlam <- ridge.cv.out$lambda.min
ridge.mod <- glmnet(x = model.matrix(Apps ~ .,College.train)[,-2], 
                    y = College.train$Apps, 
                    alpha = 0, 
                    lambda = ridge.bestlam)
ridge.pred <- predict(ridge.mod,
                      s = ridge.bestlam,
                      newx = model.matrix(Apps ~ ., College.test)[,-2])
mean((ridge.pred - College.test$Apps)^2)

# (d).
set.seed(1)
lasso.cv.out <- cv.glmnet(x = model.matrix(Apps ~ ., College.train)[,-2], 
                          y = College.train$Apps, 
                          alpha = 1)
lasso.bestlam <- lasso.cv.out$lambda.min
lasso.mod <- glmnet(x = model.matrix(Apps ~ ., College.train)[,-2], 
                    y = College.train$Apps, 
                    alpha = 1, 
                    lambda = lasso.bestlam)
lasso.pred <- predict(lasso.mod,
                      s = lasso.bestlam,
                      newx = model.matrix(Apps ~ ., College.test)[,-2])
mean((lasso.pred - College.test$Apps)^2)
lasso.mod.full <- glmnet(x = model.matrix(Apps ~ ., College)[,-2], 
                         y = College$Apps, 
                         alpha = 1, 
                         lambda = lasso.bestlam)
lasso.coef <- predict(lasso.mod.full,
                      type="coefficients",
                      s=lasso.bestlam)[1:18,]
lasso.coef <- lasso.coef[lasso.coef!=0]
lasso.coef


# (e)
set.seed(1)
pcr.fit <- pcr(Apps ~ ., 
               data = College.train, 
               scale = TRUE, 
               validation = "CV")
validationplot(pcr.fit, 
               val.type = "MSEP")
pcr.pred <- predict(pcr.fit, 
                    model.matrix(Apps ~ ., College.test)[,-2], 
                    ncomp = 16)
mean((pcr.pred - College.test$Apps)^2)
pcr.fit.full <- pcr(Apps ~ ., 
                    data = College,  
                    scale = TRUE, 
                    ncomp = 16)
summary(pcr.fit.full)


# (f)
set.seed(1)
plsr.fit <- plsr(Apps ~ ., 
                 data = College.train, 
                 scale = TRUE, 
                 validation = "CV")
validationplot(plsr.fit, 
               val.type = "MSEP")
plsr.pred <- predict(plsr.fit, 
                     model.matrix(Apps ~ ., College.test)[,-2], 
                     ncomp = 10)
mean((plsr.pred - College.test$Apps)^2)
plsr.fit.full <- pcr(Apps ~ ., 
                     data = College,  
                     scale = TRUE, 
                     ncomp = 10)
summary(plsr.fit)


detach(College)

# 2
data(Boston)
attach(Boston)
# (a)

set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Boston),rep=TRUE)
train.Boston <- Boston[train, ]
test.Boston <- Boston[!train, ]
# (b)

lm.fit_2 <- lm(crim ~ ., 
               data = train.Boston)
lm.pred_2 <- predict(object = lm.fit_2, 
                     newdata = subset(test.Boston, select = -crim))
mean((lm.pred_2 - test.Boston$crim)^2)

# (c)
predict.regsubsets <- function(object,newdata,id,...){
        form <- as.formula(object$call[[2]])
        mat <- model.matrix(form,newdata)
        coefi <- coef(object, id = id)
        xvars <- names(coefi)
        mat[, xvars]%*%coefi
}
k <- 10    #set number of fold
set.seed(1)
folds=sample(1:k, 
             nrow(train.Boston), 
             replace = TRUE)
cv.errors <- matrix(NA, 
                    k, 
                    13, 
                    dimnames=list(NULL, paste(1:13)))
for(j in 1:k){
        best.fit <- regsubsets(crim ~ .,
                               data=train.Boston[folds != j, ],
                               nvmax=13)
        for(i in 1:13){
                pred <- predict(best.fit,
                                train.Boston[folds == j, ], 
                                id = i)
                cv.errors[j,i] <- mean((train.Boston$crim[folds == j] - pred)^2)
        }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
best.ind <- which.min(mean.cv.errors)
reg.best=regsubsets(crim ~ . , 
                    data=train.Boston, 
                    nvmax=13)
best.lm <- lm(as.formula(paste("crim ~ ", 
                               paste(attr(coef(reg.best, best.ind), 
                                          "names")[-1], 
                                     collapse = " + "))), 
              data = train.Boston)
best.predict <- predict(best.lm, 
                        newdata = train.Boston)
list("Number of Variables" = length(coef(reg.best, best.ind)), 
     "Name of Variables" = attr(coef(reg.best, best.ind), "names"), 
     "Coefficients of Variables" = coef(reg.best, best.ind),
     "MSE" = mean((best.pred - test.Boston$crim)^2)))


# (d).
set.seed(1)
ridge.cv.out <- cv.glmnet(x = model.matrix(crim ~ ., train.Boston)[,-1], 
                          y = train.Boston$crim, 
                          alpha = 0)
ridge.bestlam <- ridge.cv.out$lambda.min
ridge.mod <- glmnet(x = model.matrix(crim ~ ., train.Boston)[,-1], 
                    y = train.Boston$crim, 
                    alpha = 0, 
                    lambda = ridge.bestlam)
ridge.pred <- predict(ridge.mod,
                      s = ridge.bestlam,
                      newx = model.matrix(crim ~ ., test.Boston)[,-1])
list("Lambda" = ridge.bestlam, 
     "MSE" = mean((ridge.pred - test.Boston$crim)^2))

# (e).

set.seed(1)
lasso.cv.out <- cv.glmnet(x = model.matrix(crim ~ ., train.Boston)[,-1], 
                          y = train.Boston$crim, 
                          alpha = 1)
lasso.bestlam <- lasso.cv.out$lambda.min
lasso.mod <- glmnet(x = model.matrix(crim ~ ., train.Boston)[,-1], 
                    y = train.Boston$crim, 
                    alpha = 1, 
                    lambda = lasso.bestlam)
lasso.pred <- predict(lasso.mod,
                      s = lasso.bestlam,
                      newx = model.matrix(crim ~ ., test.Boston)[,-1])
lasso.coef <- predict(lasso.mod, 
                      type = "coefficients", 
                      s = lasso.bestlam)[1:14,]
list("Lambda" = lasso.bestlam, 
     "MSE" = mean((lasso.pred - test.Boston$crim)^2), 
     "Non-zero Coefficient Estimates" = lasso.coef[lasso.coef != 0], 
     "Name of Variables with Zero Coefficient Estimates" = 
             names(lasso.coef)[which(lasso.coef == 0)])


# (f).

set.seed(1)
pcr.fit <- pcr(crim ~ ., 
               data = train.Boston, 
               scale = TRUE, 
               validation = "CV")
validationplot(pcr.fit, 
               val.type = "MSEP")
M.ind <- which.min(RMSEP(pcr.fit)$val[1, 1, ])
best.pcr.fit <- attr(RMSEP(pcr.fit)$val, "dimnames")$model[M.ind]
M <- as.numeric(gsub("([0-9]+).*$", "\\1", best.pcr.fit))
pcr.pred <- predict(pcr.fit, 
                    model.matrix(crim ~ ., test.Boston)[,-1], 
                    ncomp = M)
mean((pcr.pred - test.Boston$crim)^2)

# (g)
set.seed(1)
plsr.fit <- plsr(crim ~ ., 
                 data = train.Boston, 
                 scale = TRUE, 
                 validation = "CV")
validationplot(plsr.fit, 
               val.type = "MSEP")
M.ind <- which.min(RMSEP(plsr.fit)$val[1, 1, ])
best.plsr.fit <- attr(RMSEP(plsr.fit)$val, "dimnames")$model[M.ind]
M <- as.numeric(gsub("([0-9]+).*$", "\\1", best.plsr.fit))
plsr.pred <- predict(plsr.fit, 
                     model.matrix(crim ~ ., test.Boston)[,-1], 
                     ncomp = M)
mean((plsr.pred - test.Boston$crim)^2)

# 3
if (!file.exists("./forestfires.csv"))
        download.file(url = "http://www.dsi.uminho.pt/~pcortez/forestfires/forestfires.csv", 
                      destfile = "./forestfires.csv")
forestfires <- read.csv(file = "./forestfires.csv", 
                        header = TRUE)
Data <- subset(mutate(forestfires, log_area = log(area + 1)), 
               select = -area)
N <- 12 - 2 + (12 - 1) + (7 - 1)
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Data),rep=TRUE)
train.Data <- Data[train, ]
test.Data <- Data[!train, ]

# (d).
set.seed(1)
ridge.cv.out <- cv.glmnet(x = model.matrix(log_area ~ ., train.Data)[,-1], 
                          y = train.Data$log_area, 
                          alpha = 0)
ridge.bestlam <- ridge.cv.out$lambda.min
ridge.mod <- glmnet(x = model.matrix(log_area ~ ., train.Data)[,-1], 
                    y = train.Data$log_area, 
                    alpha = 0, 
                    lambda = ridge.bestlam)
ridge.pred <- predict(ridge.mod,
                      s = ridge.bestlam,
                      newx = model.matrix(log_area ~ ., test.Data)[,-1])
list("Lambda" = ridge.bestlam, 
     "MSE" = mean((ridge.pred - test.Data$log_area)^2))

# (e).

set.seed(1)
lasso.cv.out <- cv.glmnet(x = model.matrix(log_area ~ ., train.Data)[,-1], 
                          y = train.Data$log_area, 
                          alpha = 1)
lasso.bestlam <- lasso.cv.out$lambda.min
lasso.mod <- glmnet(x = model.matrix(log_area ~ ., train.Data)[,-1], 
                    y = train.Data$log_area, 
                    alpha = 1, 
                    lambda = lasso.bestlam)
lasso.pred <- predict(lasso.mod,
                      s = lasso.bestlam,
                      newx = model.matrix(log_area ~ ., test.Data)[,-1])
lasso.coef <- predict(lasso.mod, 
                      type = "coefficients", 
                      s = lasso.bestlam)[1:14,]
list("Lambda" = lasso.bestlam, 
     "MSE" = mean((lasso.pred - test.Data$log_area)^2), 
     "Non-zero Coefficient Estimates" = lasso.coef[lasso.coef != 0], 
     "Name of Variables with Zero Coefficient Estimates" = 
             names(lasso.coef)[which(lasso.coef == 0)])


# (f).

set.seed(1)
pcr.fit <- pcr(log_area ~ ., 
               data = train.Data, 
               scale = FALSE, 
               validation = "CV")
validationplot(pcr.fit, 
               val.type = "MSEP")
M.ind <- which.min(RMSEP(pcr.fit)$val[1, 1, ])
best.pcr.fit <- attr(RMSEP(pcr.fit)$val, "dimnames")$model[M.ind]
M <- as.numeric(gsub("([0-9]+).*$", "\\1", best.pcr.fit))
pcr.pred <- predict(pcr.fit, 
                    model.matrix(log_area ~ ., test.Data)[, -1], 
                    ncomp = M)
mean((pcr.pred - test.Data$log_area)^2)

# (g)
set.seed(1)
plsr.fit <- plsr(log_area ~ ., 
                 data = train.Data, 
                 scale = TRUE, 
                 validation = "CV")
validationplot(plsr.fit, 
               val.type = "MSEP")
M.ind <- which.min(RMSEP(plsr.fit)$val[1, 1, ])
best.plsr.fit <- attr(RMSEP(plsr.fit)$val, "dimnames")$model[M.ind]
M <- as.numeric(gsub("([0-9]+).*$", "\\1", best.plsr.fit))
plsr.pred <- predict(plsr.fit, 
                     model.matrix(log_area ~ ., test.Data[,-1], 
                                  ncomp = M))
mean((plsr.pred - test.Data$log_area)^2)




