library(rpart)
library(plyr)
library(R.utils)
library(e1071)
source("redundant_coding.R")

# Internal function, trains one classifier.
# Used by: redundant_predictor
# Requires:
#   formula - formula, specifies what to predict and what attibutes should be used to construct model
#   classifier - string, specifies the algorithm used inside; currently supported - 'rpart' and 'logit'
#   set - data.frame, training set to train on
#   bit - integer, specifies the bit to look at in labels
train_model <- function(formula, classifier, set, bit) {
  attribute_to_predict <- all.vars(formula)[1]
  # simplifies labels to just 0 and 1
	levels(set[[attribute_to_predict]]) <- substring(levels(set[[attribute_to_predict]]), bit, bit)
	# algorithm specific training
  if(classifier == "rpart") {
	  return(rpart(formula, data = set))
  } else if(classifier == "logit") {
    return(glm(formula, family=binomial(link='logit'), data=set))
  } else {
    stop("Unknown classifier")
  }
}

# Internal function, predicts one bit of the label using one classifier
# Used by: predict.redundant_predictor
# Requires:
#   classifier - type implementing predict method, specifies model used for prediction
#   set - data.frame, data set to run prediction on
predict_one_bit <- function(classifier, set) {
	return(predict(classifier, set, type='class'))
}

# Trains redundant_predictor classifier.
# Requires:
#   formula - formula, specifies what to predict and what attibutes should be used to construct model
#   data - data.frame, training set to train on
#   internal_classifier - string, specifies the algorithm used inside; currently supported - 'rpart' and 'logit'
redundant_predictor <- function(formula, data, internal_classifier="rpart") {
  # get predicted attribute name
  attribute_to_predict <- all.vars(formula)[1]

  labels <- data[[attribute_to_predict]]
  classes_count <- nlevels(labels)
  classes <- levels(labels)

  classifiers_count <- code_length(classes_count)
  classifiers <- list()

  # generate new classes, mapping between new and old and replaces old classes
  bit_classes <- bit_codes(classes_count)
  mapping <- list()
  levels(data[[attribute_to_predict]]) <- bit_classes
  for (i in 1:classes_count) {
    mapping[[ bit_classes[[i]] ]] <- classes[[i]]
  }

  # trains every internal classifier
  for (i in 1:classifiers_count) {	  
    classifiers[[i]] <- train_model(formula, internal_classifier, data, bit = i)
  }

  model = structure(list(classifiers = classifiers, mapping = mapping), class = "redundant_predictor")
  return(model)
}

# Predicts labels using the supplied classifier.
# Requires:
#   model - redundant_classifer, a trained model
#   data - data.frame, data to run prediction on
predict.redundant_predictor = function(model, data) {
  classifiers <- model[["classifiers"]]
  classifiers_count <- length(classifiers)
  mapping <- model[["mapping"]]

  # creates one long string of predictions (characters)
  labels <- vector('character')
  for (i in 1:classifiers_count) {
    labels <- append(labels, as.character(predict_one_bit(classifiers[[i]], data)))
  }

  # convert the list to a matrix and assign the result rows to closest possible classes
  labels <- matrix(labels, nrow=length(data[,1]), ncol=classifiers_count)
  fixed_labels <- as.factor(correct_labels(labels, names(mapping)))
  return(unlist(mapping[fixed_labels], use.names=FALSE))
} 