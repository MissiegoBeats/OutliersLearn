distancia_euclidea <- function(p1,p2){
	#distancia = sqrt((x2 - x1)^2 + (y2-y1)^2) -> aclaración de la siguiente línea
	distancia = sqrt( (p2[1] - p1[1])^2 + (p2[2]-p1[2])^2 )
	return(distancia)
}
