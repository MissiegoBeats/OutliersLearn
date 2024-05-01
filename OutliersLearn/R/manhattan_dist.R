manhattan_dist <- function(A,B){
	#Calculate the manhattan distance between 2 points
	#Points format: c(Ax,Ay), c(Bx,By)
	distance = abs(A[1]-B[1]) + abs(A[2]-B[2]);
	return(distance);
}
