#'mahalanobis_distance
#'
#'Detect outliers using the Mahalanobis Distance
#'
#'@author Andres Missiego Manjon
#'@param inputData Input Data dataset
#'@param alpha Significance level alpha
#'@param tutorialMode if TRUE the tutorial mode is activated (the algorithm will include an explanation detailing the theory behind the outlier detection algorithm and a step by step explanation of how is the data processed to obtain the outliers following the theory mentioned earlier)
#'@examples
#'
#'@export

mahalanobis_method <- function(inputData, alpha){
  #Conversion to matrix if it is a data frame (this is common to both options)
  #and it's not relevant for the explanation
  if (is.data.frame(inputData)) {
    inputData = as.matrix(inputData);
  } else {
    stop("inputData must be a dataframe")
  }
  if(tutorialMode){

    #TODO: add theorical explanation and step by step explanation
    #TODO: add examples to documentation

    #Check alpha value:
    if(alpha <= 1){

      #Calculate the sample mean for each variable and store it in an array:
      sampleMeans = c();
      for(i in 1:ncol(inputData)){
        column = inputData[,i]; #We get the column
        calculatedMean = sum(column)/length(column); #Calculate the mean
        sampleMeans = c(sampleMeans, calculatedMean); #Add it to the array
      }

      #Calculate the covariance matrix:
      covariance_matrix = cov(inputData);
      message("Cov mat");
      print(covariance_matrix);

      #Calculate the mahalanobis distance matrix:
      mahalanobis_mat_vector = c();
      for(i in 1:nrow(inputData)){
        mahalanobis_mat_vector = c(mahalanobis_mat_vector, mahalanobis_distance(inputData[i,], sampleMeans, covariance_matrix));
      }
      distances = mahalanobis_mat_vector;

      #Obtain the squared distances
      distances_squared = distances^2;

      #Calculate the critical value
      num_dimensions = ncol(inputData);
      message("Critical Value: ")
      critical_value <- qchisq(1 - alpha, num_dimensions); #To calculate the critical value
      print(critical_value);

      for(i in 1:length(distances)){
        if(distances[i] > critical_value){
          message(sprintf("The observation %d is an outlier", i));
          message(sprintf("The values of the observation are: "));
          print(inputData[i,]);
        }
      }
    }else{
      stop("Alpha can't be higher than 1");
    }
  }else{
    #Check alpha value:
    if(alpha <= 1){

      #Calculate the sample mean for each variable and store it in an array:
      sampleMeans = c();
      for(i in 1:ncol(inputData)){
        column = inputData[,i]; #We get the column
        calculatedMean = sum(column)/length(column); #Calculate the mean
        sampleMeans = c(sampleMeans, calculatedMean); #Add it to the array
      }

      #Calculate the covariance matrix:
      covariance_matrix = cov(inputData);
      message("Cov mat");
      print(covariance_matrix);

      #Calculate the mahalanobis distance matrix:
      mahalanobis_mat_vector = c();
      for(i in 1:nrow(inputData)){
        mahalanobis_mat_vector = c(mahalanobis_mat_vector, mahalanobis_distance(inputData[i,], sampleMeans, covariance_matrix));
      }
      distances = mahalanobis_mat_vector;

      #Obtain the squared distances
      distances_squared = distances^2;

      #Calculate the critical value
      num_dimensions = ncol(inputData);
      message("Critical Value: ")
      critical_value <- qchisq(1 - alpha, num_dimensions); #To calculate the critical value
      print(critical_value);

      for(i in 1:length(distances)){
        if(distances[i] > critical_value){
          message(sprintf("The observation %d is an outlier", i));
          message(sprintf("The values of the observation are: "));
          print(inputData[i,]);
        }
      }
    }else{
      stop("Alpha can't be higher than 1");
    }
  }
}
