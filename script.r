library(rpart)
library(plyr)

classifiers_count <- 3
classifiers <- list()

# load the data for human activity
train_set <- read.csv("human_activity/train.csv", header=TRUE)
test_set <- read.csv("human_activity/test.csv", header=TRUE)

# classic classification using rpart
tree <- rpart(Activity ~ ., method="class", data=train_set)

# convert labels
train_set$Activity <- mapvalues(train_set$Activity,
	from = c("SITTING", "STANDING", "LAYING", "WALKING", "WALKING_DOWNSTAIRS", "WALKING_UPSTAIRS"),
	to = c("000", "001", "010", "011", "100", "101"))
test_set$Activity <- mapvalues(test_set$Activity,
	from = c("SITTING", "STANDING", "LAYING", "WALKING", "WALKING_DOWNSTAIRS", "WALKING_UPSTAIRS"),
	to = c("000", "001", "010", "011", "100", "101"))

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
	labels <- append(labels, predict_one_bit(classifiers[[i]], test_set))
}
labels <- matrix

code_matrix <- function(nClasses) {
  n <- '^' (2, nClasses - 1) - 1
  
  vct <- c()
  for (i in 1:n-1) {
    vct <- c(vct, number2binary(i + n, nClasses))
  }
  
  return(matrix(vct, nrow = nClasses, ncol = n))
}

number2binary = function(number, noBits) {
  binary_vector = rev(as.numeric(intToBits(number)))
  if(missing(noBits)) {
    return(binary_vector)
  } else {
    binary_vector[-(1:(length(binary_vector) - noBits))]
  }
}