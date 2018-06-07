code_matrix <- function(nClasses) {
  n <- code_length(nClasses)
  
  vct <- c()
  for (i in 1:n) {
    vct <- c(vct, number2binary(i + n, nClasses))
  }
  mat = matrix(vct, nrow = nClasses, ncol = n)
  return(mat[do.call(order, as.data.frame(mat)),])
}

bit_codes = function(nClasses) {
  mtrx = code_matrix(nClasses)
  return(apply(mtrx, 1, paste, collapse=''))
}

number2binary = function(number, noBits) {
  binary_vector = rev(as.numeric(intToBits(number)))
  if(missing(noBits)) {
    return(binary_vector)
  } else {
    binary_vector[-(1:(length(binary_vector) - noBits))]
  }
}

code_length = function(nClasses) {
  return(2 ^ (nClasses - 1) - 1)
}

correct_labels <- function(labels, classes) {
  indices <- apply(labels, 1, find_closest, code_matrix(length(classes)))
  return(classes[indices])
}

find_closest <- function(label, code_matrix) {
  neighbors <- apply(code_matrix, 1, hamming.distance, unlist(label, use.names=FALSE))
  return(which.min(neighbors))
}