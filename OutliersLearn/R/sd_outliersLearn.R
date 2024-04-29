# TODO: documentation for this function
sd_outliersLearn <- function(data){
  for(i in 1:length(data)){
    sumD = sumD + ((data[i]-mean)^2);
  }
  stddev = sqrt(sumD/length(data));
  return(stddev);
}
