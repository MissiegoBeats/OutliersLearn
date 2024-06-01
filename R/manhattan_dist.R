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
#' distance = manhattan_dist(c(1,2), c(3,4));
#'
#' @export

manhattan_dist <- function(A,B){
	#Calculate the manhattan distance between 2 points
	#Points format: c(Ax,Ay), c(Bx,By)
	distance = abs(A[1]-B[1]) + abs(A[2]-B[2]);
	return(distance);
}
