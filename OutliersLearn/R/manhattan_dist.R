manhattan_dist <- function(A,B){
	#Calcular la distancia manhattan entre dos puntos
	#Formato de los puntos: c(Ax,Ay), c(Bx,By)
	distancia = abs(A[1]-B[1]) + abs(A[2]-B[2]);
	return(distancia);
}
