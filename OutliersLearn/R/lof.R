lof <- function(datos,K,threshold,tutorialMode){

  #TODO: añadir descripción

  #TODO: traducir todo al inglés (se ha desarrollado así por comodidad inicial)
  #y añadir opción de modo paso a paso (para la explicación)

  #Primero calculamos la distancia entre cada punto y el resto de puntos

  if(tutorialMode){

  }else{
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
        distancias = c(distancias,dist_manhattan(puntoA,puntoB));
      }
    }
    distancias = matrix(distancias,dim(datos)[1],dim(datos)[1]);
    distanciasOriginal = distancias; #Se utilizará más adelante (lo necesitamos sin ordenar por columnas)
    #Calculadas las distancias entre los puntos, calculamos el cardinal para cada punto.
    #Para ello tenemos que ordenar la matriz de distancias (por columnas)
    for(i in 1:dim(distancias)[2]){ #dim(datos)[2] es el número de columnas
      distancias[,i] = sort(distancias[,i]);
    }
    #Obtenemos un vector de los cardinales
    cardinales = c();
    for(i in 1:dim(distancias)[2]){
      columna = distancias[,i];
      #Llegamos hasta K (podría hacerse con un bucle pero así es más eficiente)
      pos = K;
      #Comprobamos cuantos valores hay iguales al de la "posición" K
      while(columna[pos] == columna[pos+1]){
        pos = pos+1;
      }
      #Los cardinales obtenidos tienen sumado un 1 ya que cuenta la distancia con él mismo
      #Por ello le restamos 1
      cardinales = c(cardinales,pos-1);
    }
    #Con los cardinales obtenidos, obtenemos las densidades de cada punto:
    densidades = c();
    puntos_usados = c(); #Se trata de una estructura de datos para almacenar los puntos que se han utilizado para cada
    #uno de los puntos para calcular su densidad.
    for(i in 1:dim(distancias)[2]){
      columna = distancias[,i];
      cardinal = cardinales[i];
      limite_bucle = cardinal+1; #De esta manera el bucle llega hasta cardinal incluido
      sumatorio = 0;
      puntosTemp = c();
      for(j in 2:limite_bucle){ #Saltamos la primera posición ya que es la distancia con él mismo
        sumatorio = sumatorio + columna[j];
        for(k in 1:dim(distanciasOriginal)[1]){ #dim(distanciasOriginal)[1] -> número de filas, es decir, número de puntos
          if(distanciasOriginal[k,i] == columna[j] && !is.element(k, puntosTemp)){
            puntosTemp = c(puntosTemp,k); #Añadimos el punto a los puntos usados para el punto i
          }
        }
      }
      densidad = (sumatorio/cardinal)^(-1);
      densidades = c(densidades,densidad);
      puntos_usados = c(puntos_usados, list(puntosTemp));
    }
    #Con las densidades calculadas, vamos a calcular la densidad relativa media (drm) para cada uno de los puntos:
    drms = c();
    for(i in 1:length(densidades)){
      sumatorioDensidades = 0;
      densidadPunto = densidades[i];
      for(j in 1:length(puntos_usados[[i]])){
        sumatorioDensidades = sumatorioDensidades + densidades[puntos_usados[[i]][[j]]];
      }
      drm = densidadPunto/(sumatorioDensidades / cardinales[i]);
      drms = c(drms, drm);

      #TODO: esto es de forma temporal:

      if(drm < threshold){
        print("El punto");
        print(i);
        print("Es un outlier");
      }
    }
  }

}
