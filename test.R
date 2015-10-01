setwd("~/Documents/git/DataAnalysis3")
rm(list = ls())
library(ISLR)
library(ggplot2)
library(gam)
library(akima)
library(splines)
library(MASS)
library(plyr)
library(dplyr)
library(gdata)
library(boot)
library(leaps)
library(mgcv)
library(gamclass)
library(glmnet)
par(mfrow = c(1, 1))
folds <- 10
# 2 (a).
attach(Boston)
poly.fit <- lm(nox ~ poly(dis, degree = 3, raw = TRUE), 
               data = Boston)
summary(poly.fit)
dislims <- range(dis)
dis.grid <- seq(from = dislims[1], to = dislims[2], by = 0.01)
preds <- predict(poly.fit, newdata = list(dis = dis.grid), se = T)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
plot(dis, 
     nox, 
     xlim=dislims, 
     pch = 20,
     cex = .5, 
     col = "darkgrey")
title("Degree-3 Polynomial Regression for nox",outer=T)
lines(dis.grid, 
      preds$fit, 
      lwd = 2, 
      col = "blue")
matlines(dis.grid, 
         se.bands, 
         lwd = 1.5, 
         col = "red",
         lty = 3)

# (b). 
max.degree <- 10
RSS <- numeric(max.degree)
names(RSS) <- rep(NA, max.degree)
models <- rep(list(NULL), max.degree)
names(models) <- rep(NA, max.degree)
preds <- rep(list(numeric(length = length(dis.grid))), 
             max.degree)
names(preds) <- rep(NA, max.degree)
par(mfrow = c(3, 4))
par(mar = c(5.5, 4.5, 1.5, 1))
for (ind in 1:max.degree){
        models[[ind]] <- glm(nox ~ poly(dis, degree = ind, raw = TRUE), 
                             data = Boston)
        attr(models, "names")[ind] <- paste("Degree -", ind)
        RSS[ind] <- sum(models[[ind]]$residuals^2)
        attr(RSS, "names")[ind] <- paste("Degree -", ind)
        preds[[ind]] <- predict(models[[ind]], 
                                newdata = list(dis = dis.grid))
        names(preds)[ind] <- paste("Degree -", ind)
        plot(dis, 
             nox, 
             xlim=dislims, 
             pch = 20,
             cex = .5, 
             col = "darkgrey", 
             main = paste("Degree -", 
                          ind, 
                          "Polynomial"),
             sub = paste("RSS for Degree -", 
                         ind, "Polynomial", 
                         "is", 
                         round(RSS[ind],3)))
        lines(dis.grid, 
              preds[[ind]], 
              lwd = 2)
}


# (c)
best.model <- rep("s", 10)
least.cv.error <- numeric(10)
best.model.no <- numeric(10)
for (k in 1:10){
        set.seed(k)
        cv.error <- numeric(max.degree)
        for (i in 1:max.degree){
                cv.error[i] <- cv.glm(data = Boston, 
                                      glmfit = models[[i]], 
                                      K = folds)$delta[1]
                attr(cv.error, "names")[i] <- paste("Degree -", i)
        }
        best.model.no[k] <- which.min(cv.error)
        least.cv.error[k] <- cv.error[best.model.no[k]]
        best.model[k] <- paste(names(cv.error[best.model.no[k]]), "Polynomial Regression")
}

# (d)
min.spline.degree <- 3
max.spline.degree <- 6
cv = NULL
# for (i in 1:10){
set.seed(1)
bs.models <- rep(list(NULL), max.spline.degree - min.spline.degree +1)
names(bs.models) <- rep(NA, max.spline.degree - min.spline.degree +1)
cv.rss <- numeric(max.spline.degree - min.spline.degree +1)
names(cv.rss) <- rep(NA, max.spline.degree - min.spline.degree +1)
for (df.spline in min.spline.degree:max.spline.degree){
        bs.models[[df.spline -min.spline.degree + 1]] <- glm(nox ~ bs(dis, df = df.spline), 
                                                             data = Boston)
        attr(bs.models, "names")[df.spline -min.spline.degree + 1] <- paste("Degree -", df.spline)
        cv.rss[df.spline -min.spline.degree + 1] <- 
                cv.glm(data = Boston, 
                       glmfit = bs.models[[df.spline -min.spline.degree + 1]], 
                       K = folds, 
                       cost = function(y, yhat) sum((y - yhat)^2))$delta[1]
        attr(cv.rss, "names")[df.spline -min.spline.degree + 1] <- 
                paste("Degree -", df.spline)
}
best.model.no <- which.min(cv.rss)
least.cv.rss <- cv.rss[best.model.no]
best.model <- paste(names(least.cv.rss), "Spline Regression")
cv <- rbind(cv, cbind(i,matrix(cv.rss, nrow = 1, dimnames = list(NULL, 
                                                                 names(cv.rss)))))
# }
bs.pred <- predict(object = bs.models[[best.model.no]], 
                   list(dis = dis.grid))
par(mfrow = c(1, 1))
plot(dis, 
     nox, 
     xlim=dislims, 
     pch = 20,
     cex = .5, 
     col = "darkgrey", 
     main = paste(best.model, 
                  "for nox"),
     sub = paste("RSS for", 
                 best.model, 
                 "is", 
                 round(RSS[ind],3)))
lines(dis.grid, 
      bs.pred,
      lwd = 2)



# (e)
min.spline.degree <- 3
max.spline.degree <- 6
cv.ns = NULL
set.seed(1)
ns.models <- rep(list(NULL), max.spline.degree - min.spline.degree +1)
names(ns.models) <- rep(NA, max.spline.degree - min.spline.degree +1)
cv.ns.rss <- numeric(max.spline.degree - min.spline.degree +1)
names(cv.ns.rss) <- rep(NA, max.spline.degree - min.spline.degree +1)
for (df.spline in min.spline.degree:max.spline.degree){
        ns.models[[df.spline -min.spline.degree + 1]] <- glm(nox ~ ns(dis, df = df.spline), 
                                                             data = Boston)
        attr(ns.models, "names")[df.spline -min.spline.degree + 1] <- paste("Degree -", df.spline)
        cv.ns.rss[df.spline -min.spline.degree + 1] <- 
                cv.glm(data = Boston, 
                       glmfit = ns.models[[df.spline -min.spline.degree + 1]], 
                       K = folds, 
                       cost = function(y, yhat) sum((y - yhat)^2))$delta[1]
        attr(cv.ns.rss, "names")[df.spline -min.spline.degree + 1] <- 
                paste("Degree -", df.spline)
}
best.model.no <- which.min(cv.ns.rss)
least.cv.ns.rss <- cv.ns.rss[best.model.no]
best.model <- paste(names(least.cv.ns.rss), "Spline Regression")
cv.ns <- rbind(cv.ns, cbind(i,matrix(cv.ns.rss, nrow = 1, dimnames = list(NULL, 
                                                                          names(cv.ns.rss)))))

ns.pred <- predict(object = ns.models[[best.model.no]], 
                   list(dis = dis.grid))
par(mfrow = c(1, 1))
plot(dis, 
     nox, 
     xlim=dislims, 
     pch = 20,
     cex = .5, 
     col = "darkgrey", 
     main = paste(best.model, 
                  "for nox"),
     sub = paste("RSS for", 
                 best.model, 
                 "is", 
                 round(RSS[ind],3)))
lines(dis.grid, 
      ns.pred,
      lwd = 2)


# (f)

ss.fit=smooth.spline(nox ,dis, cv = T)
smoothing.level <- ss.fit$lambda


# (g)
set.seed(1)
spans <- seq(0.2, 0.8, 0.01)
fold.no=sample(1:folds, 
               nrow(Boston), 
               replace = TRUE)
cv.errors <- matrix(NA, 
                    folds, 
                    length(spans), 
                    dimnames=list(NULL, paste(spans)))
for(j in 1:length(spans)){
        for(i in 1:folds){
                loess.fit=loess(nox ~ dis, 
                                span = spans[j], 
                                data = Boston[fold.no != i, ])
                pred <- predict(object = loess.fit,
                                newdata = Boston[fold.no == i, ])
                cv.errors[i,j] <- mean((Boston$nox[fold.no == i] - pred)^2,
                                       na.rm = TRUE)
        }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
best.loess.no <- which.min(mean.cv.errors)
least.loess.cv <- mean.cv.errors[best.loess.no]
best.span <- as.numeric(names(least.loess.cv))
detach(Boston)



# 3
if (!file.exists("./pages.dat"))
        download.file(url = "http://files.figshare.com/2290841/pages.dat", 
                      destfile = "./pages.dat")
pages <- read.table(file = "./pages.dat")
names(pages) <- c("DATE",
                  "PAGES",
                  "COUNT_LINERS", 
                  "INCHES_LINERS", 
                  "LINES_DISPLAY")
folds <- 5
attach(pages)
set.seed(1)
fold.no=sample(1:folds, 
               nrow(pages), 
               replace = TRUE)
cv.errors <- matrix(NA, 
                    folds, 
                    3^3, 
                    dimnames=list(NULL, paste(1:3^3)))
forms <- expand.grid(c("I", "lo", "s"), 
                     c("I", "lo", "s"), 
                     c("I", "lo", "s"))
for(j in 1:26){
        formula.fit <- formula(paste("PAGES ~ ", 
                                     forms[j, 1], 
                                     "(COUNT_LINERS) + ", 
                                     forms[j, 2], 
                                     "(INCHES_LINERS) + ",
                                     forms[j, 3], 
                                     "(LINES_DISPLAY)", 
                                     collapse = ""))
        for(i in 1:folds){
                gam.fit=mgcv::gam(formula.fit, 
                                  data = pages[fold.no != i, ])
                pred <- mgcv::predict.gam(object = gam.fit,
                                      newdata = pages[fold.no == i, ])
                cv.errors[i,j] <- mean((pages$PAGES[fold.no == i] - pred)^2,
                                       na.rm = TRUE)
        }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
best.gam.no <- which.min(mean.cv.errors)
least.gam.cv <- mean.cv.errors[best.gam.no]
best.model.form <- formula(paste("PAGES ~ ", 
                                 forms[best.gam.no, 1], 
                                 "(COUNT_LINERS) + ", 
                                 forms[best.gam.no, 2], 
                                 "(INCHES_LINERS) + ",
                                 forms[best.gam.no, 3], 
                                 "(LINES_DISPLAY)", 
                                 collapse = ""))
linear.gam.model <- formula(paste("PAGES ~ ", 
                                  forms[1, 1], 
                                  "(COUNT_LINERS) + ", 
                                  forms[1, 2], 
                                  "(INCHES_LINERS) + ",
                                  forms[1, 3], 
                                  "(LINES_DISPLAY)", 
                                  collapse = ""))
best.gam.fit=mgcv::gam(best.model.form, 
                       data = pages)
linear.gam.fit=mgcv::gam(linear.gam.model, 
                         data = pages)
cllims <- range(`COUNT_LINERS`)
illims <- range(`INCHES_LINERS`)
ldlims <- range(`LINES_DISPLAY`)
cl.grid <- seq(from = cllims[1], 
               to = cllims[2], 
               by = (cllims[2] - cllims[1])/100 )
il.grid <- seq(from = illims[1], 
               to = illims[2], 
               by = (illims[2] - illims[1])/100 )
ld.grid <- seq(from = ldlims[1], 
               to = ldlims[2], 
               by = (ldlims[2] - ldlims[1])/100 )
pages.grid <- expand.grid(`COUNT_LINERS` = cl.grid, 
                          `INCHES_LINERS` = il.grid, 
                          `LINES_DISPLAY` = ld.grid)
preds <- mgcv::predict.gam(best.gam.fit, newdata = pages.grid)
anova(linear.gam.fit, best.gam.fit, test="F")


# predict.regsubsets <- function(object,newdata,id,...){
#         form <- as.formula(object$call[[2]])
#         mat <- model.matrix(form,newdata)
#         coefi <- coef(object, id = id)
#         xvars <- names(coefi)
#         mat[, xvars]%*%coefi
# }
# set.seed(1)
# fold.no=sample(1:folds, 
#              nrow(pages), 
#              replace = TRUE)
# cv.errors <- matrix(NA, 
#                     folds, 
#                     3, 
#                     dimnames=list(NULL, paste(1:3)))
# for(j in 1:folds){
#         best.fit <- regsubsets(PAGES ~ .,
#                                data=pages[fold.no != j, ],
#                                nvmax=3)
#         for(i in 1:3){
#                 pred <- predict(best.fit,
#                                 pages[fold.no == j, ], 
#                                 id = i)
#                 cv.errors[j,i] <- mean((pages$PAGES[fold.no == j] - pred)^2)
#         }
# }
# mean.cv.errors <- apply(cv.errors, 2, mean)
# best.ind <- which.min(mean.cv.errors)
# reg.best <- regsubsets(PAGES ~ . , 
#                        data=pages, 
#                        nvmax=3)
# best.formula <- as.formula(paste("PAGES ~ ", 
#                                  paste(attr(coef(reg.best, best.ind), 
#                                             "names")[-1], 
#                                        collapse = " + ")))
# best.lm <- lm(best.formula, 
#               data = pages)
# best.pred <- predict(best.lm, 
#                      newdata = pages)


# 4
if (!file.exists("./lmr_levee.dat"))
        download.file(url = "http://www.stat.ufl.edu/~winner/data/lmr_levee.dat", 
                      destfile = "./lmr_levee.dat")
lmr_levee <- read.table(file = "./lmr_levee.dat")
names(lmr_levee) <- c("Failure", 
                      "year",  
                      "river mile",   
                      "sediments",   
                      "borrow pit",    
                      "meander",     
                      "channel width",
                      "floodway width",  
                      "constriction factor", 
                      "land cover",     
                      "veg width",   
                      "sinuosity",   
                      "dredging",  
                      "revetement")
lmr_levee$sediments <- as.factor(lmr_levee$sediments)
lmr_levee$`borrow pit` <- as.factor(lmr_levee$`borrow pit`)
lmr_levee$meander <- as.factor(lmr_levee$meander)
lmr_levee$`land cover` <- as.factor(lmr_levee$`land cover`)
lmr_levee$revetement <- as.factor(lmr_levee$revetement)
lmr_levee$Failure <- as.factor(lmr_levee$Failure)
Data <- data.frame(model.matrix(~ ., data = lmr_levee))
attach(Data)
p.values <- numeric(11)
fit.1 <- mgcv::gam(Failure1 ~ sediments1, family = "binomial")
p.values[1] <- summary(fit.1)$p.table[2, 4]
names(p.values)[1] <- names(lmr_levee)[1+3]
fit.2 <- mgcv::gam(Failure1 ~ X.borrow.pit.1, family = "binomial")
p.values[2] <- summary(fit.2)$p.table[2, 4]
names(p.values)[2] <- names(lmr_levee)[2 + 3]
fit.3 <- mgcv::gam(Failure1 ~ meander2 + meander3 + meander4, family = "binomial")
p.values[3] <- min(summary(fit.3)$p.table[2:4, 4])
names(p.values)[3] <- names(lmr_levee)[3 + 3]
fit.4 <- mgcv::gam(Failure1 ~ s(X.channel.width.), family = "binomial")
p.values[4] <- summary(fit.4)$s.table[1, 4]
names(p.values)[4] <- names(lmr_levee)[4 + 3]
fit.5 <- mgcv::gam(Failure1 ~ s(X.floodway.width.), family = "binomial")
p.values[5] <- summary(fit.5)$s.table[1, 4]
names(p.values)[5] <- names(lmr_levee)[5 + 3]
fit.6 <- mgcv::gam(Failure1 ~ s(X.constriction.factor.), family = "binomial")
p.values[6] <- summary(fit.6)$s.table[1, 4]
names(p.values)[6] <- names(Data)[12]
fit.7 <- mgcv::gam(Failure1 ~ X.land.cover.2 + X.land.cover.3 + X.land.cover.4, family = "binomial")
p.values[7] <- min(summary(fit.7)$p.table[2:4, 4])
names(p.values)[7] <- names(lmr_levee)[7 + 3]
fit.8 <- mgcv::gam(Failure1 ~ s(X.veg.width.), family = "binomial")
p.values[8] <- summary(fit.8)$s.table[1, 4]
names(p.values)[8] <- names(lmr_levee)[8 + 3]
fit.9 <- mgcv::gam(Failure1 ~ s(sinuosity), family = "binomial")
p.values[9] <- summary(fit.9)$s.table[1, 4]
names(p.values)[9] <- names(Data)[17]
fit.10 <- mgcv::gam(Failure1 ~ s(dredging), family = "binomial")
p.values[10] <- summary(fit.6)$s.table[1, 4]
names(p.values)[10] <- names(Data)[18]
fit.11 <- mgcv::gam(Failure1 ~ revetement1, family = "binomial")
p.values[11] <- summary(fit.11)$p.table[2, 4]
names(p.values)[11] <- names(lmr_levee)[11 + 3]
predictors <- names(p.values[p.values < 0.05])


cv.gam.bin <- function(formula, data, nfold, response, seed.no = 1){
        set.seed(seed.no)
        fold.no=sample(1:nfold, 
                       nrow(data), 
                       replace = TRUE)
        cv.errors <- numeric(nfold)
        for(i in 1:nfold){
                gam.fit <- mgcv::gam(formula, 
                                     data = data[fold.no != i, ], 
                                     family = "binomial")
                pred <- mgcv::predict.gam(object = gam.fit,
                                          newdata = data[fold.no == i, ])
                cv.errors[i] <- mean((data[fold.no == i,response] != (pred>0.5)),
                                     na.rm = TRUE)
        }
        cv.error <- mean(cv.errors)
        list(cv.error, cv.errors)
}
model.no <- expand.grid(c(TRUE, FALSE),
                        c(TRUE, FALSE), 
                        c(TRUE, FALSE))
cvmse <- numeric(7)
for (i in 1:7){
        gam.formula <- as.formula(paste("Failure1~", 
                                        paste("s(",
                                              predictors[as.logical(model.no[i, ])],
                                              ")",
                                              collapse = "+", 
                                              sep = "")))
        cvmse[i] <- cv.gam.bin(gam.formula, Data, nfold = 5, response = "Failure1")[[1]]
}
i <- which.min(cvmse)
gam.formula <- as.formula(paste("Failure1~", 
                                paste("s(",
                                      predictors[as.logical(model.no[i, ])],
                                      ")",
                                      collapse = "+", 
                                      sep = "")))




detach(Data)

# 5
if (!file.exists("./forestfires.csv"))
        download.file(url = "http://www.dsi.uminho.pt/~pcortez/forestfires/forestfires.csv", 
                      destfile = "./forestfires.csv")
forestfires <- read.csv(file = "./forestfires.csv", 
                        header = TRUE)
Data <- subset(mutate(forestfires, log_area = log(area + 1)), 
               select = -area)
attach(Data)
# Ridge Regression
set.seed(1)
k=5
ridge.cv.out <- cv.glmnet(x = model.matrix(log_area ~ ., Data)[,-1], 
                          y = Data$log_area, 
                          alpha = 0,
                          nfolds = k)
ridge.bestlam <- ridge.cv.out$lambda.min
min(ridge.cv.out$cvm)



cv.mse <- numeric(10)
cv.mse[1] <- CVgam(log_area ~ s(X,Y) + day + s(FFMC) + s(DMC) + s(DC) +
                           s(ISI) + s(temp) + s(RH) + s(wind) + rain, 
                   data = Data, 
                   nfold = 5)[[3]]
cv.mse[2] <- CVgam(log_area ~ s(X,Y) + day + s(FFMC) + s(DMC) + s(DC) +
                           s(ISI) + s(temp) + s(RH) + s(wind), 
                   data = Data, 
                   nfold = 5)[[3]]
cv.mse[3] <- CVgam(log_area ~ s(X,Y), 
                   data = Data, 
                   nfold = 5)[[3]]
cv.mse[4] <- CVgam(log_area ~ s(X,Y) + s(temp) + s(wind) + rain, 
                   data = Data, 
                   nfold = 5)[[3]]
cv.mse[5] <- CVgam(log_area ~ s(X,Y) + s(FFMC) + s(DMC) + s(DC) +
                           s(ISI) + s(RH), 
                   data = Data, 
                   nfold = 5)[[3]]
cv.mse[6] <- CVgam(log_area ~ s(X,Y) + s(FFMC), 
                   data = Data, 
                   nfold = 5)[[3]]
cv.mse[7] <- CVgam(log_area ~ s(X,Y) + s(DMC), 
                   data = Data, 
                   nfold = 5)[[3]]
cv.mse[8] <- CVgam(log_area ~ s(X,Y) + s(DC), 
                   data = Data, 
                   nfold = 5)[[3]]
cv.mse[9] <- CVgam(log_area ~ s(X,Y) + s(DC) + s(ISI), 
                   data = Data, 
                   nfold = 5)[[3]]
cv.mse[10] <- CVgam(log_area ~ s(X,Y) + s(DC) + s(RH), 
                   data = Data, 
                   nfold = 5)[[3]]

