library(rpart)
library(plyr)
library(R.utils)
library(e1071)

source("redundant_coding.R")

# load the data for human activity
train_set <- read.csv("human_activity/train.csv", header=TRUE)
test_set <- read.csv("human_activity/test.csv", header=TRUE)

n_classes <- nlevels(train_set$Activity)

classifiers_count <- code_length(n_classes)
classifiers <- list()

# classic classification using rpart
#tree <- rpart(Activity ~ ., method="class", data=train_set)

# convert labels
new_classes <- bit_codes(n_classes)
levels(train_set$Activity) <- new_classes
levels(test_set$Activity) <- new_classes

train_model <- function(set, bit) {
	levels(set$Activity) <- substring(levels(set$Activity), bit, bit)
	# algorithm specific training, currently just rpart
	return(rpart(Activity ~ ., data = set))
}

predict_one_bit <- function(classifier, set) {
	return(predict(classifier, test_set, type='class'))
}

# train separate classifiers
for (i in 1:classifiers_count) {
	classifiers[[i]] <- train_model(train_set, bit = i)
}

# run separate classifiers
labels <- vector('character')
for (i in 1:classifiers_count) {
	labels <- append(labels, as.character(predict_one_bit(classifiers[[i]], test_set)))
}

labels <- matrix(labels, nrow=length(test_set[,1]), ncol=classifiers_count)
fixed_labels <- as.factor(correct_labels(labels, new_classes))

# calculate accuracy
confusion_matrix <- table(test_set$Activity, fixed_labels)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)