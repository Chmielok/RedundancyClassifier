# load the data for forest cover type
train_set <- read.csv("forest/train.csv", header=TRUE)
train_set$Cover_Type <- as.factor(train_set$Cover_Type)
#test_set <- read.csv("forest/test.csv", header=TRUE)

source("redundant_predictor.R")

model <- redundant_predictor(Cover_Type ~ ., train_set)

results <- predict(model, train_set)
confusion_matrix <- table(train_set$Cover_Type, results)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)