train_set <- read.csv("human_activity/train.csv", header=TRUE)
test_set <- read.csv("human_activity/test.csv", header=TRUE)

source("redundant_predictor.R")
library(rpart)
library(nnet)

show_stats <- function(model, data) {
    results <- predict(model, data, type="class")
    confusion_matrix <- table(data$Activity, results)
    accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
    print(confusion_matrix)
    print(accuracy)
}

formula = Activity ~ .

custom_model_rpart <- redundant_predictor(formula, train_set)
custom_model_logit <- redundant_predictor(formula, train_set, internal_classifier="logit")
rpart_model <- rpart(formula, data = train_set)
logit_model <- multinom(formula, data=train_set, MaxNWts =3384, maxit=200)

show_stats(custom_model_rpart, test_set)
show_stats(custom_model_logit, test_set)
show_stats(rpart_model, test_set)
show_stats(logit_model, test_set)