code_matrix <- function(nClasses) {
  n <- code_length(nClasses)
  
  vct <- c()
  for (i in 1:n) {
    vct <- c(vct, number2binary(i + n, nClasses))
  }
  mat = matrix(vct, nrow = nClasses, ncol = n)
  
  if(nClasses > 6) {
    mat = choose_random_columns(mat, code_length(6))
  }

    return(mat[do.call(order, as.data.frame(mat)),])
}

bit_codes = function(nClasses) {
  mtrx = code_matrix(nClasses)
  return(apply(mtrx, 1, paste, collapse=''))
}

choose_random_columns <- function(mat, size) {
  j <- sample(seq_len(ncol(mat)), size=size)
  return(mat[, j, drop=FALSE])
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
  if(nClasses > 6) {
    return(code_length(6))
  }
  return(2 ^ (nClasses - 1) - 1)
}

correct_labels <- function(labels, classes) {
  classes_matrix <- unlist(strsplit(classes, ''))
  classes_matrix <- matrix(as.numeric(classes_matrix), ncol=length(classes))
  classes_matrix <- t(classes_matrix)
  indices <- apply(labels, 1, find_closest, classes_matrix)
  return(classes[indices])
}

find_closest <- function(label, code_matrix) {
  neighbors <- apply(code_matrix, 1, hamming.distance, unlist(label, use.names=FALSE))
  return(which.min(neighbors))
}