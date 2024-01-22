knn <- function(datos, d, K){

  #TODO: traducir a inglés y añadir opción de explicación paso por paso

  #Conversión a matriz si es un data frame:
  if(is.data.frame(datos)){
    datos = as.matrix(datos);
  }
  #Primero tenemos que calcular las distancias
  distancias = c();
  #euclídeas entre cada uno de los puntos
  for(i in 1:dim(datos)[1]){ #dim(datos)[1] es el número de filas
    #Punto sobre el que vamos a comparar:
    puntoA = c(datos[i,1],datos[i,2]);
    for(j in 1:dim(datos)[1]){
      #Punto sobre el que se compara puntoA
      puntoB = c(datos[j,1],datos[j,2]);
      distancias = c(distancias,distancia_euclidea(puntoA,puntoB));
    }
  }
  distancias = matrix(distancias,dim(datos)[1],dim(datos)[1]);
  #print("Distancias obtenidas");
  #print(distancias);
  #Ordenamos las distancias por columnas y mostramos los outliers:
  for(i in 1:dim(distancias)[2]){ #dim(datos)[2] es el número de columnas
    distancias[,i] = sort(distancias[,i]);
    if(distancias[K,i] > d){
      print("El suceso");
      print(i);
      print("Es un outlier");
    }
  }
}
