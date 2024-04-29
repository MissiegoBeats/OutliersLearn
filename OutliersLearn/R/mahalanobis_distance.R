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
mahalanobis_distance <- function(value, sample_mean, sample_covariance_matrix){
  #See https://repository.upb.edu.co/bitstream/handle/20.500.11912/6582/Técnicas%20para%20detección%20de%20outliers%20multivariantes.pdf?sequence=1&isAllowed=y
  #For the mahalanobis_distance formula
  #TODO: make this work
  diff_vector = value-sample_mea;
  transposed_diff_vector = t(diff_vector);
  inverted_covariance_matrix = solve(sample_covariance_matrix);
  return(sqrt(transposed_diff_vector - inverted_covariance_matrix %*% diff_vector));
}
