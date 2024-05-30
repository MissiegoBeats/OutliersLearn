#'
#' manhattan_dist
#'
#' Calculates the manhattan distance between two 2D points
#'
#' @author Andres Missiego Manjon
#' @param A One of the 2D points
#' @param B The other 2D point
#'
#' @return Manhattan distance calculated between point A and B
#'
#' @examples
#' inputData = t(matrix(c(3,2,3.5,12,4.7,4.1,5.2,4.9,7.1,6.1,6.2,5.2,14,5.3),2,7,dimnames=list(c("r","d"))));
#' inputData = as.matrix(inputData);
#' distance = manhattan_dist(inputData[3,], inputData[4,]);
#' print(distance);
#'
#' distance = manhattan_dist(c(1,2), c(3,4));
#' print(distance);
#'
#' @export

manhattan_dist <- function(A,B){
	#Calculate the manhattan distance between 2 points
	#Points format: c(Ax,Ay), c(Bx,By)
	distance = abs(A[1]-B[1]) + abs(A[2]-B[2]);
	return(distance);
}
