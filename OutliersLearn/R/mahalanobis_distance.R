#'mahalanobis_distance
#'
#'Calculates the mahalanobis_distance given the input data
#'
#'@param point Point to calculate the mahalanobis_distance
#'@param sample_mean Sample mean
#'@param sample_covariance_matrix Sample Covariance Matrix
#'
#'@return Mahalanobis distance associated to the point
#'
#'@examples
#'
#'@export
mahalanobis_distance <- function(value, sample_mean, sample_covariance_matrix) {
  print(value);
  # Calculate the difference vector
  diff_vector <- value - sample_mean

  # Calculate the transposed difference vector
  transposed_diff_vector <- t(diff_vector)

  # Calculate the inverse of the covariance matrix
  inverted_covariance_matrix <- solve(sample_covariance_matrix)

  # Calculate the Mahalanobis distance
  mahalanobis_dist <- sqrt(transposed_diff_vector %*% inverted_covariance_matrix %*% diff_vector)

  return(mahalanobis_dist)
}
