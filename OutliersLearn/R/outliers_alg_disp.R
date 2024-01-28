outliers_alg_disp <- function(datos, d)
{

  #TODO: traducir a ingles y agnadir opcion de explicacion paso por paso
  #TODO: cambiar el nombre al algoritmo

  #Declaramos las variables temporales que se van a utilizar en la funcion
  sumatorio = 0;
  sumatorioD = 0;

  #Calculamos la media:
  for(i in 1:length(datos)){
    sumatorio = sumatorio + datos[i];
  }
  media = sumatorio/length(datos);

  #Calculamos la desviacion tipica
  for(i in 1:length(datos)){
    sumatorioD = sumatorioD + ((datos[i]-media)^2);
  }
  desviacion = sqrt(sumatorioD/length(datos));

  #Calculamos los limites de la siguiente forma:
  limites = c(media - desviacion * d, media + desviacion * d)
  #Mostramos los limites
  print("Limites: ");
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
