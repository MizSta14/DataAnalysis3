rm(list = ls())
ConfusionTable <- function(ct){
    accuracy <- (ct[1, 1] + ct[2, 2]) / (sum(ct))
    TPr <- ct[2, 2] / (ct[1, 2] + ct[2, 2])
    FPr <- ct[2, 1] / (ct[1, 1] + ct[2, 1])
    precision <- ct[2, 2] / (ct[2, 1] + ct[2, 2])
    error <- 1 - accuracy
    result <- list(accuracy, TPr, FPr, precision, error)
    names(result) <- c("Accuracy", "True Positive Rate", 
                       "False Posistive Rate", "Precision", 
                       "Total Error Rate")
    return(result)
}
subset1 <- c(FALSE, abs(cor(Boston[, 1], Boston[, -1])) > 0.3, TRUE)
subset2 <- c(FALSE, abs(cor(Boston[, 1], Boston[, -1])) > 0.4, TRUE)
subset3 <- c(FALSE, abs(cor(Boston[, 1], Boston[, -1])) > 0.5, TRUE)
Boston$crim01 <- as.numeric(Boston$crim > median(Boston$crim))
set.seed(1)
rands <- runif(nrow(Boston))
test <- rands > quantile(rands, 0.75)
train <- !test
Boston.train <- Boston[train, -1]
Boston.test <- Boston[test, -1]
glm.fit=glm(crim01 ~ ., data=Boston.train, family = binomial)
summary(glm.fit)



lda.fit <- lda(crim01 ~ nox + rad + ptratio, data=Boston.train)
lda.pred=predict(lda.fit,Boston.test)$class
ct <- table(lda.pred,Boston.test$crim01)
ct
ConfusionTable(ct)