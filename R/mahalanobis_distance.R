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
#'@export
mahalanobis_distance <- function(value, sample_mean, sample_covariance_matrix) {
  # Ensure value and sample_mean are vectors
  value <- as.matrix(value)
  sample_mean <- as.matrix(sample_mean)

  if (ncol(value) != ncol(sample_mean)) {
    stop("Dimensions of value and sample_mean do not match")
  }

  # Calculate the difference vector
  diff_vector <- value - sample_mean

  # Calculate the inverse of the covariance matrix
  inverted_covariance_matrix <- solve(sample_covariance_matrix)

  # Calculate the Mahalanobis distance
  mahalanobis_dist <- sqrt(t(diff_vector) %*% inverted_covariance_matrix %*% diff_vector)

  return(as.numeric(mahalanobis_dist))
}
