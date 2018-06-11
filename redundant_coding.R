# Returns matrix of bit_codes (shrinked if necessary)
# Requires:
#   nClasses - number of unique classes
code_matrix <- function(nClasses) {
  n <- code_length(nClasses)
  
  vct <- c()
  for (i in 1:n) {
    vct <- c(vct, number2binary(i + n, nClasses))
  }
  mat = matrix(vct, nrow = nClasses, ncol = n)
  
  # shrink by random if necessary
  if(nClasses > 6) {
    mat = choose_random_columns(mat, code_length(6))
  }

  return(mat[do.call(order, as.data.frame(mat)),])
}

# Returns list of bit_codes.
# Used by redundancy_predictor.
# Requires:
#   nClasses - number of unique classes
bit_codes = function(nClasses) {
  mtrx = code_matrix(nClasses)
  return(apply(mtrx, 1, paste, collapse=''))
}

# Shrinks matrix by returning random columns.
# Requires:
#   mat - matrix to be shrinked
#   size - desired number of columns
choose_random_columns <- function(mat, size) {
  j <- sample(seq_len(ncol(mat)), size=size)
  return(mat[, j, drop=FALSE])
}

# Converts decimal number to bit vector
# Requires:
#   number - number to be converted
#   noBits - desired length of vector
number2binary = function(number, noBits) {
  binary_vector = rev(as.numeric(intToBits(number)))
  if(missing(noBits)) {
    return(binary_vector)
  } else {
    binary_vector[-(1:(length(binary_vector) - noBits))]
  }
}

# Returns length of bit code used in redundancy_predictor
# Requires:
#   nClasses - number of unique classes
code_length = function(nClasses) {
  if(nClasses > 6) {
    return(code_length(6))
  }
  return(2 ^ (nClasses - 1) - 1)
}

# Returns corrected bit codes given by prediction
# Requires:
#   labels - labels to be corrected
#   classes - all proper bit codes
correct_labels <- function(labels, classes) {
  classes_matrix <- unlist(strsplit(classes, ''))
  classes_matrix <- matrix(as.numeric(classes_matrix), ncol=length(classes))
  classes_matrix <- t(classes_matrix)
  indices <- apply(labels, 1, find_closest, classes_matrix)
  return(classes[indices])
}

# Find closest bit code. Hamming distance is used to calculate difference.
# Requires:
#   label - bit code to be corrected
#   code_matrix - matrix of all bit codes
find_closest <- function(label, code_matrix) {
  neighbors <- apply(code_matrix, 1, hamming.distance, unlist(label, use.names=FALSE))
  return(which.min(neighbors))
}