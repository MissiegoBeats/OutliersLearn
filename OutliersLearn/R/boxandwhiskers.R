boxandwhiskers <- function(datos,d){

  #TODO: traducir a inglés y añadir opción de explicación paso por paso

  #Aplicamos el algoritmo que hemos visto en clase
  cuantil1 = cuantil(datos,0.25);
  cuantil3 = cuantil(datos,0.75);
  limites = c(cuantil1 - d*(cuantil3-cuantil1), cuantil3 + d*(cuantil3-cuantil1))
  #Mostramos los límites:
  print("Límites: ");
  print(limites);
  #Obtenemos los outlier (los mostramos por pantalla)
  for(i in 1:length(datos)){
    if(datos[i] < limites[1] || datos[i] > limites[2]){
      print("El suceso");
      print(i);
      print("Con valor");
      print(datos[i]);
      print("Es un outlier");
    }
  }
}
