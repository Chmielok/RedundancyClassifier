train_set <- read.csv("human_activity/train.csv", header=TRUE)
test_set <- read.csv("human_activity/test.csv", header=TRUE)

source("redundant_predictor.R")
library(rpart)
library(nnet)

show_stats <- function(results, data) {
    confusion_matrix <- table(data$Activity, results)
    accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
    print(confusion_matrix)
    print(accuracy)
}

formula = Activity ~ .

rpart_model <- rpart(formula, data=train_set)
logit_model <- multinom(formula, data=train_set, maxit=1000, MaxNWts = 3384)
custom_model <- redundant_predictor(formula, data=train_set)

results <- list()

results[["train_rpart"]] = predict(rpart_model, train_set, type="class")
results[["train_logit"]] = predict(logit_model, train_set, type="class")
results[["train_custom"]] = predict(custom_model, train_set)
results[["test_rpart"]] = predict(rpart_model, test_set, type="class")
results[["test_logit"]] = predict(logit_model, test_set, type="class")
results[["test_custom"]] = predict(custom_model, test_set)

for (i in 1:length(results)) {
    name <- names(results)[[i]]
    result <- results[[i]]
    print(name)
    if(startsWith(name, "train")) {
        show_stats(result, train_set)
    } else {
        show_stats(result, test_set)
    }
}