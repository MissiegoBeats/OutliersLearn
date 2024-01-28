quantile <- function(data,v){
  #OBJ: Obtain the 'v' quantile.
  #Params:
  #   data -> data we want to process
  #   v -> the value of the quantile (e.g. 0.25,0.75)
	#Sort the data
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
