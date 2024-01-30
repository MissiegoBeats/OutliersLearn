lof <- function(inputData,K,threshold,tutorialMode){

  #Primero calculamos la distancia entre cada punto y el resto de puntos

  if(tutorialMode){ #Caso en el que se quiera recibir la explicacion
    #Conversion a matriz si es un data frame:
    if(is.data.frame(inputData)){
      inputData = as.matrix(inputData);
    }
    message("Calculo de las distancias euclideas entre todos los puntos:")
    #Primero tenemos que calcular las distancias
    distancias = c();
    #euclideas entre cada uno de los puntos
    for(i in 1:dim(inputData)[1]){ #dim(inputData)[1] es el numero de filas
      #Punto sobre el que vamos a comparar:
      message("\tCalculando distancia entre los puntos:")
      puntoA = c(inputData[i,1],inputData[i,2]);
      print(puntoA);
      for(j in 1:dim(inputData)[1]){
        #Punto sobre el que se compara puntoA
        puntoB = c(inputData[j,1],inputData[j,2]);
        print(puntoB);
        distancias = c(distancias,dist_manhattan(puntoA,puntoB));
        message("\tDistancia calculada:");
        print(dist_manhattan(puntoA,puntoB));
      }
    }
    message("La matriz de distancias euclideas queda de la siguiente manera:");
    distancias = matrix(distancias,dim(inputData)[1],dim(inputData)[1]);
    print(distancias);
    distanciasOriginal = distancias; #Se utilizara mas adelante (lo necesitamos sin ordenar por columnas)
    message("Calculadas las distancias entre los puntos, calculamos el cardinal para cada punto");
    message("Para ello tenemos que ordenar la matriz de distancias (por columnas)");
    for(i in 1:dim(distancias)[2]){ #dim(inputData)[2] es el numero de columnas
      distancias[,i] = sort(distancias[,i]);
    }
    message("La matriz de distancias ordenada por columnas es la siguiente");
    print(distancias);
    message("Obtenemos un vector de los cardinales");
    cardinales = c();
    for(i in 1:dim(distancias)[2]){
      columna = distancias[,i];
      #Llegamos hasta K (podria hacerse con un bucle pero asi es mas eficiente)
      pos = K;
      #Comprobamos cuantos valores hay iguales al de la "posicion" K
      while(columna[pos] == columna[pos+1]){
        pos = pos+1;
      }
      #Los cardinales obtenidos tienen sumado un 1 ya que cuenta la distancia con el mismo
      #Por ello le restamos 1
      cardinales = c(cardinales,pos-1);
    }
    print(cardinales);
    #Con los cardinales obtenidos, obtenemos las densidades de cada punto:
    densidades = c();
    puntos_usados = c(); #Se trata de una estructura de inputData para almacenar los puntos que se han utilizado para cada
    #uno de los puntos para calcular su densidad.
    for(i in 1:dim(distancias)[2]){
      columna = distancias[,i];
      cardinal = cardinales[i];
      limite_bucle = cardinal+1; #De esta manera el bucle llega hasta cardinal incluido
      sumatorio = 0;
      puntosTemp = c();
      for(j in 2:limite_bucle){ #Saltamos la primera posicion ya que es la distancia con el mismo
        sumatorio = sumatorio + columna[j];
        for(k in 1:dim(distanciasOriginal)[1]){ #dim(distanciasOriginal)[1] -> numero de filas, es decir, numero de puntos
          if(distanciasOriginal[k,i] == columna[j] && !is.element(k, puntosTemp)){
            puntosTemp = c(puntosTemp,k); #Agnadimos el punto a los puntos usados para el punto i
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
  }else{
    #Conversion a matriz si es un data frame:
    if(is.data.frame(inputData)){
      inputData = as.matrix(inputData);
    }
    #Primero tenemos que calcular las distancias
    distancias = c();
    #euclideas entre cada uno de los puntos
    for(i in 1:dim(inputData)[1]){ #dim(inputData)[1] es el numero de filas
      #Punto sobre el que vamos a comparar:
      puntoA = c(inputData[i,1],inputData[i,2]);
      for(j in 1:dim(inputData)[1]){
        #Punto sobre el que se compara puntoA
        puntoB = c(inputData[j,1],inputData[j,2]);
        distancias = c(distancias,dist_manhattan(puntoA,puntoB));
      }
    }
    distancias = matrix(distancias,dim(inputData)[1],dim(inputData)[1]);
    distanciasOriginal = distancias; #Se utilizara mas adelante (lo necesitamos sin ordenar por columnas)
    #Calculadas las distancias entre los puntos, calculamos el cardinal para cada punto.
    #Para ello tenemos que ordenar la matriz de distancias (por columnas)
    for(i in 1:dim(distancias)[2]){ #dim(inputData)[2] es el numero de columnas
      distancias[,i] = sort(distancias[,i]);
    }
    #Obtenemos un vector de los cardinales
    cardinales = c();
    for(i in 1:dim(distancias)[2]){
      columna = distancias[,i];
      #Llegamos hasta K (podria hacerse con un bucle pero asi es mas eficiente)
      pos = K;
      #Comprobamos cuantos valores hay iguales al de la "posicion" K
      while(columna[pos] == columna[pos+1]){
        pos = pos+1;
      }
      #Los cardinales obtenidos tienen sumado un 1 ya que cuenta la distancia con el mismo
      #Por ello le restamos 1
      cardinales = c(cardinales,pos-1);
    }
    #Con los cardinales obtenidos, obtenemos las densidades de cada punto:
    densidades = c();
    puntos_usados = c(); #Se trata de una estructura de inputData para almacenar los puntos que se han utilizado para cada
    #uno de los puntos para calcular su densidad.
    for(i in 1:dim(distancias)[2]){
      columna = distancias[,i];
      cardinal = cardinales[i];
      limite_bucle = cardinal+1; #De esta manera el bucle llega hasta cardinal incluido
      sumatorio = 0;
      puntosTemp = c();
      for(j in 2:limite_bucle){ #Saltamos la primera posicion ya que es la distancia con el mismo
        sumatorio = sumatorio + columna[j];
        for(k in 1:dim(distanciasOriginal)[1]){ #dim(distanciasOriginal)[1] -> numero de filas, es decir, numero de puntos
          if(distanciasOriginal[k,i] == columna[j] && !is.element(k, puntosTemp)){
            puntosTemp = c(puntosTemp,k); #Agnadimos el punto a los puntos usados para el punto i
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
