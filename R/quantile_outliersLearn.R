#'
#' quantile_outliersLearn
#'
#' Function that obtains the 'v' quantile
#'
#' @author Andres Missiego Manjon
#' @param data Input Data
#' @param v Goes from 0 to 1 (e.g. 0.25). Indicates the quantile that wants to be obtained
#'
#' @return Quantile v calculated
#'
#' @examples
#' q = quantile(c(12,2,3,4,1,13), 0.60)
#'
#' @export

quantile_outliersLearn <- function(data,v){
  #We will preprocess the data to convert it to a vector
  #Just in case the data param. is not a vector
  data = transform_to_vector(data);
	data = sort(data);
	#Apply the formula to obtain the quantile
	nc = length(data)*v;
	if (is.integer(nc)) {
    		x = (data[nc] + data[nc+1])/2;
  	} else {
    		x = data[floor(nc)+1];
	}
	return(x)
}
