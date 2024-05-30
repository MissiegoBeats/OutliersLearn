#'
#' sd_outliersLearn
#'
#' Calculates the standard deviation of the input data given the mean.
#'
#' @author Andres Missiego Manjon
#' @param data Input Data that will be used to calculate the standard deviation. Must be a vector
#' @param mean Mean of the input data vector of the function.
#'
#' @return Standard Deviation of the input data
#'
#' @examples
#' inputData = c(1,2,3,4,5,6,1);
#' mean = mean_outliersLearn(inputData);
#' sd = sd_outliersLearn(inputData, mean);
#' print(sd);
#'
#' @export

sd_outliersLearn <- function(data, mean){
  sumD = 0; #Will be used to calculate the standard deviation
  for(i in 1:length(data)){
    sumD = sumD + ((data[i]-mean)^2);
  }
  stddev = sqrt(sumD/length(data));
  return(stddev);
}
