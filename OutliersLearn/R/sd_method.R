#' sd_method
#'
#' This function implements the outlier detection algorithm using standard deviation and mean
#'
#' @author Andres Missiego Manjon
#' @param data Input Data
#' @param d Degree of outlier or distance at which an event is considered an outlier
#' @param tutorialMode if TRUE the tutorial mode is activated (the algorithm will include an explanation detailing the theory behind the outlier detection algorithm and a step by step explanation of how is the data processed to obtain the outliers following the theory mentioned earlier)
#'
#' @examples
#' inputData = t(matrix(c(3,2,3.5,12,4.7,4.1,5.2,4.9,7.1,6.1,6.2,5.2,14,5.3),2,7,dimnames=list(c("r","d"))))
#' inputData = data.frame(inputData)
#' sd_method(inputData,2,TRUE)
#'
#' sd_method(c(1,2,3,4,1,23,4),2,FALSE)
#'
#' @export
sd_method <- function(data, d, tutorialMode)
{
  #TODO: comprobar si tendriamos que hacer source de las funciones que vamos a usar (en principio no)

  #Now we must preprocess the data to "standardize" it.
  #It's important to understand that we must transform N dimensional arrays to 1D arrays (e.g. to a vector)
  #because of the nature of this algorithm (it does not process multidimensional arrays)
  #We make use of the transform_to_vector() function included in this package
  data <- transform_to_vector(data)

  if(tutorialMode){
    message("The tutorial mode has been activated for the  algorithm (outlier detection)")
    message("Before processing the data, we must understand the algorithm and the 'theory' behind it.")
    message("Identification of outliers using Statistics and Standard Deviation involves the following steps:")
    message("\t1. Determination of the degree of outlier (We will call it 'd')")
    message("\t2. Obtain the arithmetic mean with the following formula:")
    message("\t\tmean = sum(x) / N")
    message("\t We calculate the mean adding all the values from the data and dividing for the length of the data")
    message("\t3. Obtain the standard deviation with the following formula:")
    message("\t\tsd = sqrt(sum((x - mean)^2) / N)")
    message("\tWe calculate the sum of every single element of the data minus the mean elevated to 2. Then we divide it for the data length")
    message("\t4. Calculate the interval limits using the following equation:")
    message("\t\t(mean - d * sd, mean + d * sd)")
    message("\t5. Identification of outliers as values that fall outside the interval calculated in step 4.")
    message("Now that we know how to apply this algorithm, we are going to see how it works with the given data:")
    print(data)
    #Declare the temp variables
    sum = 0; #Will be used to calculate the mean
    sumD = 0; #Will be used to calculate the standard deviation

    message("The degree of outlier selected ('d') selected is: ")
    print(d)

    message("First we calculate the mean using the formula described before: ")
    #Mean calculation:
    for(i in 1:length(data)){
      sum = sum + data[i];
    }
    mean = sum/length(data);
    print(mean)

    message("Now we calculate the standard deviation using the formula described before:")
    #Standard deviation calculation:
    for(i in 1:length(data)){
      sumD = sumD + ((data[i]-mean)^2);
    }
    stddev = sqrt(sumD/length(data));
    print(stddev)

    message("With those values calculated, we obtain the limits: ")
    message("First we calculate the lower limit")
    message("\tmean-stddev * d")
    print(mean-stddev*d)
    message("Now we calculate the top limit")
    message("\tmean+stddev*d")
    print(mean+stddev*d)
    #Calculate the limits:
    limits = c(mean - stddev * d, mean + stddev * d)
    message("This are the obtained limits")
    print(limits);

    message("Now that we have calculated the limits, we will check if every single value is 'inside' those boundaries obtained.")
    message("If the value is not included inside the limits, it will be detected as an outlier")
    for(i in 1:length(data)){
      print(sprintf("Checking value in the position %d. It's value is %.3f", i, data[i]))
      if(data[i] < limits[1]){
        print(sprintf("The value in position %d with value %.3f has been detected as an outlier", i, data[i]))
        print(sprintf("It was detected as an outlier because it's value is lower than the low limit %.3f",limits[1]))
      }else if(data[i] > limits[2]){
        print(sprintf("The value in position %d with value %.3f has been detected as an outlier", i, data[i]))
        print(sprintf("It was detected as an outlier because it's value is higher than the top limit %.3f",limits[2]))
      }else{
        print("Not an outlier, it's inside the limits")
      }
      print("--------------------------------------------------------------------------------------------")
    }
    message("The algorithm has ended")
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
