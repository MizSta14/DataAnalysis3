rm(list = ls())
attach(Boston)
subset1 <- as.numeric(abs(cor(Boston[, 1], Boston[, -1]))> 0.5)
subset1 <- as.numeric(abs(cor(Boston[, 1], Boston[, -1]))> 0.4)
subset1 <- as.numeric(abs(cor(Boston[, 1], Boston[, -1]))> 0.3)
Boston$crim01 <- as.numeric(Boston$crim > median(Boston$crim))
rands <- runif(nrow(Boston))
test <- rands > quantile(rands,0.75)
train <- !test
Boston.train <- Boston[train,]
Boston.test <- Boston[test,]
Boston.train.fact <- Boston.train
Boston.train.fact$crim01 <- factor(Boston.train.fact$crim01)
library(GGally)
ggpairs(Boston.train.fact, colour='crim01')
