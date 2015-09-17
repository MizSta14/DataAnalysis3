rm(list = ls())
library(ISLR)
library(ggplot2)
library(boot)
#8
#a
set.seed(1)
y <- rnorm(100)
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)
fun.y <- function(x) return(x - 2 * x^2)

#b
data.xy <- data.frame(x, y)
ggplot(data = data.xy, aes(x = x, y = y)) +
    geom_point() + 
    stat_function(fun = fun.y, linetype = 2)

#c
set.seed(2)
cv.error <- rep(0, 4)
for (i in 1:4){
    glm.fit <- glm(y ~ poly(x,i), data = data.xy)
    cv.error[i] <- cv.glm(data.xy, glm.fit)$delta[1]
}
cv.error

#d
set.seed(3)
cv.error2 <- rep(0, 4)
for (i in 1:4){
    glm.fit <- glm(y ~ poly(x,i), data = data.xy)
    cv.error2[i] <- cv.glm(data.xy, glm.fit)$delta[1]
}
cv.error2

#f
lm.fit <- list()
coefs <- list()
coefs <- rep(NA, 4)
for (i in 1:4){
    lm.fit <- lm(y ~ poly(x,i), data = data.xy)
    coefs[i] <- summary(lm.fit)[4]
}
coefs


#9
library(MASS)
attach(Boston)
#a
mean <- mean(medv)
#b
mean.std <- sd(medv) / sqrt(length(medv))
mean.std

#c
set.seed(1)
mean.fn <- function(data, index) 
    return(c(mean(data[index]), 
             var(data[index]) / length(data)))
library(boot)
mean.boot <- boot(medv, mean.fn, R = 1000)
mean.boot
#d
ci.t <- t.test(medv)$conf.int
attr(ci.t, "conf.level") <- NULL
ci.boot <- boot.ci(mean.boot)$normal[c(2,3)]
ci.t
ci.boot


#e
median.medv <- median(medv)
median.medv

#f
median.fn <- function(data, index) 
    return(median(data[index]))
median.boot <- boot(medv, median.fn, R = 1000)
median.boot

#g
q.10.medv <- quantile(medv, probs = 0.1)
q.10.medv

#h
q.10.fn <- function(data, index) 
    return(quantile(data[index], probs = 0.1))
q.10.boot <- boot(medv, q.10.fn, R = 1000)
q.10.boot
p <- seq(0, 1, 0.01)
k <- sqrt(p * (1 - p)) / (sqrt(506) * dnorm(qnorm(p)))
plot(p,k)


#3
pages <- read.table(file = "./pages.dat")
names(pages) <- c("date",
                  "pages",
                  "countliners", 
                  "inchesliners", 
                  "linersdisplay")
par(mar = c(2, 2, 2, 2))
pairs(pages)
correlation <- cor(pages)
regressor <- c(names(pages)[3], 
               names(pages)[4], 
               names(pages)[5], 
               paste(names(pages)[3], 
                     names(pages)[4], 
                     sep = " * "), 
               paste(names(pages)[3], 
                     names(pages)[5], 
                     sep = " * "), 
               paste(names(pages)[4], 
                     names(pages)[5], 
                     sep = " * "), 
               paste(names(pages)[3], 
                     names(pages)[4], 
                     names(pages)[5],
                     sep = " * "))
indices <- expand.grid(c(TRUE, FALSE), 
                       c(TRUE, FALSE), 
                       c(TRUE, FALSE),
                       c(TRUE, FALSE), 
                       c(TRUE, FALSE),
                       c(TRUE, FALSE),
                       c(TRUE, FALSE)
                       )
cvError <- rep(NA, 127)
for (n in 1:127){
    lmformula <- reformulate(regressor[as.logical(indices[n, ])], 
                             response = "pages")
    glm.fit <- glm(lmformula, data = pages)
    cvError[n] <- cv.glm(data = pages, 
                         glmfit = glm.fit, 
                         K = 5)$delta[2]
    names(cvError)[n] <- paste(regressor[as.logical(indices[n, ])], 
                               collapse = " + ")
}
best <- names(which.min(cvError))
best.lm <- lm(as.formula(paste("pages", best, sep = " ~ ")), 
              data = pages)
res <- resid(best.lm)
yhat <- predict(best.lm)
plot(yhat, res)


#4
des <- read.table(file = "./des_site1and2sp_2.dat", 
                  col.names = c("glu", 
                                "nud", 
                                "utmn", 
                                "utme", 
                                "sw", 
                                "elevation", 
                                "slope", 
                                "geology", 
                                "LTA", 
                                "ELT", 
                                "site", 
                                "subplot"))
attach(des)
glu <- as.factor(glu)
geology <- as.factor(geology)
LTA <- as.factor(LTA)
ELT <- as.factor(ELT)
site <- as.factor(site)
costFunction <- function(y, yhat) return(mean(y != (yhat > 0.5)))
cvError.inter <- rep(NA, 15)
cvError.noint <- rep(NA, 15)
n <- 1
for (i in 5:9){
    for (j in (i+1):10){
        glmformula.inter <- as.formula(
            paste("glu ~ (", 
                  paste(names(des)[c(i, j)], 
                        collapse = " + "), 
                  ")^2"
            )
        )
        glmformula.noint <- as.formula(
            paste("glu ~ ", 
                  paste(names(des)[c(i, j)], 
                        collapse = " + ")
            )
        )
        logis.fit.inter <- glm(glmformula.inter,
                               family = "binomial", 
                               data = des)
        logis.fit.noint <- glm(glmformula.noint,
                               family = "binomial", 
                               data = des)
        cvError.inter[n] <- cv.glm(data = des, 
                                   glmfit = logis.fit.inter, 
                                   cost = costFunction, 
                                   K = 10)$delta[1]
        cvError.noint[n] <- cv.glm(data = des, 
                                   glmfit = logis.fit.noint, 
                                   cost = costFunction, 
                                   K = 10)$delta[1]
        names(cvError.inter)[n] <- paste(
            as.character(glmformula.inter)[2],
            as.character(glmformula.inter)[1],
            as.character(glmformula.inter)[3],
            collapse = " ")
        names(cvError.noint)[n] <- paste(
            as.character(glmformula.noint)[2],
            as.character(glmformula.noint)[1],
            as.character(glmformula.noint)[3],
            collapse = " ")
        n <- n + 1
    }
}
best.inter <- min(cvError.inter)
best.noint <- min(cvError.noint)
names(best.inter) <- names(cvError.inter)[which.min(cvError.inter)]
names(best.noint) <- names(cvError.noint)[which.min(cvError.inter)]




