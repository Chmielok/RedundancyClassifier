library(rpart)
library(plyr)
library(R.utils)
library(e1071)
source("redundant_coding.R")

train_model <- function(formula, classifier, set, bit) {
  attribute_to_predict <- all.vars(formula)[1]
	levels(set[[attribute_to_predict]]) <- substring(levels(set[[attribute_to_predict]]), bit, bit)
	# algorithm specific training
  if(classifier == "rpart") {
	  return(rpart(formula, data = set))
  } else if(classifier == "logr") {
    return(glm(formula, family=binomial(link='logit'), data=set))
  }
}

predict_one_bit <- function(classifier, set) {
	return(predict(classifier, set, type='class'))
}

redundant_predictor <- function(formula, data, internal_classifier="rpart") {
  # get predicted attribute name
  attribute_to_predict <- all.vars(formula)[1]

  labels <- data[[attribute_to_predict]]
  classes_count <- nlevels(labels)
  classes <- levels(labels)

  classifiers_count <- code_length(classes_count)
  classifiers <- list()

  bit_classes <- bit_codes(classes_count)
  mapping <- list()

  levels(data[[attribute_to_predict]]) <- bit_classes
  
  for (i in 1:classes_count) {
    mapping[[ bit_classes[[i]] ]] <- classes[[i]]
  }

  for (i in 1:classifiers_count) {	  
    classifiers[[i]] <- train_model(formula, internal_classifier, data, bit = i)
  }

  model = structure(list(classifiers = classifiers, mapping = mapping), class = "redundant_predictor")
  return(model)
}

predict.redundant_predictor = function(model, data) {
  classifiers <- model[["classifiers"]]
  classifiers_count <- length(classifiers)
  mapping <- model[["mapping"]]

  labels <- vector('character')
  for (i in 1:classifiers_count) {
    labels <- append(labels, as.character(predict_one_bit(classifiers[[i]], data)))
  }

  labels <- matrix(labels, nrow=length(data[,1]), ncol=classifiers_count)
  fixed_labels <- as.factor(correct_labels(labels, names(mapping)))
  return(unlist(mapping[fixed_labels], use.names=FALSE))
} 