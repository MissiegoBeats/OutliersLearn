#TODO: documentation for this function
mean_outliersLearn <- function(data){
  for(i in 1:length(data)){
    sum = sum + data[i];
  }
  mean = sum/length(data);
  return(mean);
}
