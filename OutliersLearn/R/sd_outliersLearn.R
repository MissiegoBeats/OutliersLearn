# TODO: documentation for this function
sd_outliersLearn <- function(data){
  sumD = 0; #Will be used to calculate the standard deviation
  for(i in 1:length(data)){
    sumD = sumD + ((data[i]-mean)^2);
  }
  stddev = sqrt(sumD/length(data));
  return(stddev);
}
