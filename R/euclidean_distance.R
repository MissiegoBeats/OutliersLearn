#'
#' euclidean_distance
#'
#' This function calculates the euclidean distance between 2 points. They must have the same number of dimensions
#'
#' @author Andres Missiego Manjon
#' @param p1 One of the points that will be used by the algorithm with N dimensions
#' @param p2 The other point that will be used by the algorithm with N dimensions
#'
#' @return Euclidean Distance calculated between the two N-dimensional points
#'
#' @examples
#' inputData = t(matrix(c(3,2,3.5,12,4.7,4.1,5.2,4.9,7.1,6.1,6.2,5.2,14,5.3),2,7,dimnames=list(c("r","d"))));
#' inputData = data.frame(inputData);
#' point1 = inputData[1,];
#' point2 = inputData[4,];
#' distance = euclidean_distance(point1, point2);
#' print(distance)
#'
#' distance = euclidean_distance(c(1,2,3.4,1.2,7), c(3,2.1,3,5.6,12));
#' print(distance)
#'
#' @export

euclidean_distance <- function(p1, p2) {
  if (length(p1) != length(p2)) {
    stop("Points must have the same number of components")
  }

  # Calculate the squared differences for each component
  squared_differences <- (p2 - p1)^2

  # Sum the squared differences and take the square root
  distance <- sqrt(sum(squared_differences))

  return(distance)
}
