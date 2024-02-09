euclidean_distance <- function(p1,p2){
	#distance = sqrt((x2 - x1)^2 + (y2-y1)^2) -> This is the formula of the next line of code
	distance = sqrt( (p2[1] - p1[1])^2 + (p2[2]-p1[2])^2 )
	return(distance)
}
