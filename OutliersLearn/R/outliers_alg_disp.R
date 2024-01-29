outliers_alg_disp <- function(data, d, tutorialMode)
{

  if(tutorialMode){

  }else{
    #Declare the temp variables
    sum = 0; #Will be used to calculate the mean
    sumD = 0; #Will be used to calculate the standard deviation

    #Mean calculation:
    for(i in 1:length(data)){
      sum = sum + data[i];
    }
    mean = sum/length(data);

    #Standard deviation calculation:
    for(i in 1:length(data)){
      sumD = sumD + ((data[i]-mean)^2);
    }
    stddev = sqrt(sumD/length(data));

    #Calculate the limits:
    limits = c(mean - stddev * d, mean + stddev * d)
    print("Limits: ");
    print(limits);

    #Obtain outliers using the limits obtained earlier
    for(i in 1:length(data)){
      if(data[i] < limits[1]){
        print(sprintf("The value in position %d with value %.3f has been detected as an outlier", i, data[i]))
        print(sprintf("It was detected as an outlier because it's value is lower than the low limit %.3f",limits[1]))
        print("--------------------------------------------------------------------------------------------")
      }else if(data[i] > limits[2]){
        print(sprintf("The value in position %d with value %.3f has been detected as an outlier", i, data[i]))
        print(sprintf("It was detected as an outlier because it's value is higher than the top limit %.3f",limits[2]))
        print("--------------------------------------------------------------------------------------------")
      }
    }
  }
}
