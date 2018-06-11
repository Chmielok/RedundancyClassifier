train_set <- read.csv("human_activity/train.csv", header=TRUE)
test_set <- read.csv("human_activity/test.csv", header=TRUE)

source("redundant_predictor.R")

model <- redundant_predictor(Activity ~ ., train_set)

results <- predict(model, test_set)
confusion_matrix <- table(test_set$Activity, results)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)