#' mahalanobis_distance
#'
#' Calculates the mahalanobis_distance given the input data
#'
#' @author Andres Missiego Manjon
#' @param value Point to calculate the mahalanobis_distance
#' @param sample_mean Sample mean
#' @param sample_covariance_matrix Sample Covariance Matrix
#'
#' @return Mahalanobis distance associated to the point
#'
#' @examples
#' inputData = t(matrix(c(3,2,3.5,12,4.7,4.1,5.2,
#' 4.9,7.1,6.1,6.2,5.2,14,5.3),2,7,dimnames=list(c("r","d"))));
#' inputData = data.frame(inputData);
#' inputData = as.matrix(inputData);
#' sampleMeans = c();
#' for(i in 1:ncol(inputData)){
#'   column = inputData[,i];
#'   calculatedMean = sum(column)/length(column);
#'   print(sprintf("Calculated mean for column %d: %f", i, calculatedMean))
#'   sampleMeans = c(sampleMeans, calculatedMean);
#' }
#' covariance_matrix = cov(inputData);
#' distance = mahalanobis_distance(inputData[3,], sampleMeans, covariance_matrix);
#'
#' @export

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
