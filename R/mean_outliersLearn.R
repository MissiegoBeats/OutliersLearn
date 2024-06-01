#'
#' mean_outliersLearn
#'
#' Calculates the mean of the given data vector
#'
#' @author Andres Missiego Manjon
#' @param data Input Data that will be processed to calculate the mean. It must be a vector
#'
#' @return Mean of the input data
#'
#' @examples
#' mean = mean_outliersLearn(c(2,3,2.3,7.8));
#'
#' @export

mean_outliersLearn <- function(data){
  #Declare the temp variables
  sum = 0; #Will be used to calculate the mean
  for(i in 1:length(data)){
    sum = sum + data[i];
  }
  mean = sum/length(data);
  return(mean);
}
