#TODO: documentation for this function
mean_outliersLearn <- function(data){
  #Declare the temp variables
  sum = 0; #Will be used to calculate the mean
  for(i in 1:length(data)){
    print(sum);
    print(data[i]);
    sum = sum + data[i];
  }
  mean = sum/length(data);
  return(mean);
}
