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
cor(Boston[, 1], Boston[, -1])
subset <- c(FALSE, abs(cor(Boston[, 1], Boston[, -1])) > 0.3, TRUE)
Boston$crim01 <- as.numeric(Boston$crim > median(Boston$crim))
set.seed(1)
rands <- runif(nrow(Boston))
test <- rands > quantile(rands, 0.75)
train <- !test
Boston.train <- Boston[train, subset]
Boston.test <- Boston[test, subset]
Boston.train$crim01 <- factor(Boston.train$crim01)