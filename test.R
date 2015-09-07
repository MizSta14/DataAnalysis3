#code test
#4.10
rm(list = ls())
library(ISLR)
library(ggplot2)
attach(Weekly)
summary(Weekly)
pairs(Weekly)
cor(Weekly[, -9])
