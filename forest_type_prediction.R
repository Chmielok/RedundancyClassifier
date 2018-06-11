# load the data for forest cover type
train_set <- read.csv("forest/train.csv", header=TRUE)
train_set$Cover_Type <- as.factor(train_set$Cover_Type)
test_set <- read.csv("forest/test.csv", header=TRUE)

source("redundant_predictor.R")
library(rpart)
library(nnet)

show_stats <- function(results, data) {
    confusion_matrix <- table(data$Cover_Type, results)
    accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
    print(confusion_matrix)
    print(accuracy)
}

formula = Cover_Type ~ .

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
        # cannot be calculated locally - labels are visible to Kaggle only
        #show_stats(result, test_set)
    }
}