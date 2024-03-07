# euclidean_distance <- function(p1,p2){
# 	#distance = sqrt((x2 - x1)^2 + (y2-y1)^2) -> This is the formula of the next line of code
# 	distance = sqrt( (p2[1] - p1[1])^2 + (p2[2]-p1[2])^2 )
# 	return(distance)
# }
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
