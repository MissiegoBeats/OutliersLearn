#'mahalanobis_method
#'
#'Detect outliers using the Mahalanobis Distance method
#'
#'@author Andres Missiego Manjon
#'@param inputData Input Data dataset that will be processed (with or not the step by step explanation) to obtain the underlying outliers. It must be a data.frame type.
#'@param alpha Significance level alpha. This value indicates the proportion that it is expected to be outliers out of the dataset. It has to be in the range from 0 to 1
#'@param tutorialMode if TRUE the tutorial mode is activated (the algorithm will include an explanation detailing the theory behind the outlier detection algorithm and a step by step explanation of how is the data processed to obtain the outliers following the theory mentioned earlier)
#'@examples
#'inputData = t(matrix(c(3,2,3.5,12,4.7,4.1,5.2,4.9,7.1,6.1,6.2,5.2,14,5.3),2,7,dimnames=list(c("r","d"))));
#'inputData = data.frame(inputData);
#'mahalanobis_method(inputData, 0.7, FALSE);
#'
#'inputData = t(matrix(c(3,2,3.5,12,4.7,4.1,5.2,4.9,7.1,6.1,6.2,5.2,14,5.3),2,7,dimnames=list(c("r","d"))));
#'inputData = data.frame(inputData);
#'mahalanobis_method(inputData, 0.2, TRUE);
#'
#'@export

mahalanobis_method <- function(inputData, alpha, tutorialMode){
  #Conversion to matrix if it is a data frame (this is common to both options)
  #and it's not relevant for the explanation
  if (is.data.frame(inputData)) {
    inputData = as.matrix(inputData);
  } else {
    stop("inputData must be a dataframe")
  }
  if(tutorialMode){
    message("The tutorial mode has been activated for the Mahalanobis Distance Outlier Detection Method");
    message("Before processing the data, we must understand the algorithm and the 'theory' behind it.");
    message("The algorithm is made up with 6 steps: ");

    message("\t1)Check if the input value 'alpha' is in the desired range");
    message("\t\tIf this is true (between 0 and 1), then continue to the next step. If the value is greater than 1 or smaller than 0, end the algorithm.");
    message("\t\tThe concept of the input parameter alpha is the proportion of observations used for the estimation of the critical value (distance value calculated with a chi-squared distribution using alpha)");

    message("\t2)Calculate the mean for each column of the dataset.");
    message("\t\tIn other words, calculate the mean value for each 'dimension' of the dataset.");
    message("\t\tThis is done by adding all the values in every single column and then dividing by the number of elements that have been added.");
    message("\t\tWith this step, the algorithm now has available a vector of means (each position is the mean of the column of the vector/array position).");

    message("\t3)Calculate the covariance matrix.");
    message("\t\tThe covariance matrix is a square matrix with diagonal elements that represent the variance and the non-diagonal components that express covariance.");
    message("\t\tThe covariance of a variable can take any real value (a positive covariance suggests that the two variables have a positive relationship. On the other hand, a negative value indicates that they don't have a positive relationship. If they don't vary together, they have a zero value).");
    message("\t\tThe implementation chosen for this algorithm due to the fact that it's not relevant the implementation of this function is with a R native function.");
    message("\t\tIt's important to know what is the covariance matrix but, because of the nature of the Outliers Learn R package, it's not crucial to implement this function from scratch (it's one of the only 2 functions that have not been implemented from scratch in the R package).");

    message("\t4)Obtain the Mahalanobis squared distances vector.");
    message("\t\tThis is one of the most 'crucial' steps of the Mahalanobis distance method for outlier detection.");
    message("\t\tIt's important to highlight that the Mahalanobis distance function has been implemented from scratch due to the importance of it for the algorithm.");
    message("\t\tEven though there is an implementation to obtain the Mahalanobis squared distances from a dataset in R, this function has been implemented because it's a really important key concept the reader has to be able to 'see' implemented and be able to use it.");
    message("\t\tThe implementation calculates the Mahalanobis distance from a point to the mean using the covariance matrix using this formula:");
    message("\t\t\tD = sqrt((X-means)'*inverted_cov_matrix*(X-means))");
    message("\t\tGoing back to what to do in this step: calculate the Mahalanobis distance between each point and the 'center' using the mean vector and the covariance matrix calculated in steps 2) and 3) with the previous formula.");
    message("\t\tWith the distances calculated, elevate them to square so that the distances vector is D^2.");

    message("\t5)Calculate the critical value");
    message("\t\tWith the Mahalanobis squared distances calculated, the next step is to calculate the critical value.");
    message("\t\tThis is done with a chi-squared distribution.");
    message("\t\tThe function used in the implementation is an R native function due to the complexity of it.");
    message("\t\tThe corresponding function returns the critical value such that the probability of a chi-squared random variable with degrees of freedom equal to the dimensions of the input dataset exceeding this value is alpha (explained briefly in the first step).");

    message("\t6)Classify the points using the critical value");
    message("\t\tWith the critical value calculated, the last step is to check every single distance calculated and if the value is greater than the critical value, the point associated with the distance is classified as an outlier.");
    message("\t\tIf not, the point associated with the distance is classified as an inlier (not an outlier).");

    message("With the theory understood, we will apply this knowledge to the data given to obtain the outliers")
    message("----------------------------------------------------------")

    message("Check if the input value alpha is smaller or equal to 1.");
    message("If this is true, then continue to the next step. If the value is greater than 1, end the algorithm.");
    if(alpha <= 1 && alpha >= 0){
      # Calculate the sample mean for each variable and store it in an array:
      message("Calculate the mean for each column of the dataset.")
      sampleMeans = c();
      for(i in 1:ncol(inputData)){
        column = inputData[,i]; # We get the column
        calculatedMean = sum(column)/length(column); # Calculate the mean
        print(sprintf("Calculated mean for column %d: %f", i, calculatedMean))
        sampleMeans = c(sampleMeans, calculatedMean); # Add it to the array
      }
      message("Mean vector calculated: ");
      print(sampleMeans);

      # Calculate the covariance matrix:
      message("Calculate the covariance matrix.")
      covariance_matrix = cov(inputData);
      message("Covariance Matrix calculated:")
      print(covariance_matrix);

      # Calculate the Mahalanobis distance matrix:
      message("Obtain the Mahalanobis squared distances vector.")
      mahalanobis_mat_vector = c();
      for(i in 1:nrow(inputData)){
        print(sprintf("Mahalanobis distance for point %d: %f", i, mahalanobis_distance(inputData[i,], sampleMeans, covariance_matrix)));
        mahalanobis_mat_vector = c(mahalanobis_mat_vector, mahalanobis_distance(inputData[i,], sampleMeans, covariance_matrix));
      }
      message("The distances vector (D) is:");
      distances = mahalanobis_mat_vector;
      print(distances);

      # Obtain the squared distances
      message("Square the Mahalanobis distances.");
      message("The squared_distance vector (D^2) is:");
      distances_squared = distances^2;
      print(distances_squared);

      # Calculate the critical value
      message("Calculate the critical value.")
      num_dimensions = ncol(inputData);
      print(sprintf("Degrees of freedom: %d", num_dimensions));
      print(sprintf("Alpha value: %f", alpha));
      print(sprintf("1-alpha = %f", 1-alpha));
      message("Critical Value: ")
      critical_value <- qchisq(1 - alpha, num_dimensions); # To calculate the critical value
      print(critical_value);

      # Classify points based on the critical value
      message("Classify points based on the critical value")
      for(i in 1:length(distances_squared)){
        if(distances_squared[i] > critical_value){
          message(sprintf("The observation %d is an outlier (squared distance %f is greater than the critical value %f", i, distances_squared[i],critical_value));
          message(sprintf("The values of the observation are: "));
          print(inputData[i,]);
        }
      }
      message("The algorithm has ended");
    }else{
      stop("Alpha can't be higher than 1 or smaller than 0");
    }
  }else{
    #Check alpha value:
    if(alpha <= 1 && alpha >= 0){

      #Calculate the sample mean for each variable and store it in an array:
      sampleMeans = c();
      for(i in 1:ncol(inputData)){
        column = inputData[,i]; #We get the column
        calculatedMean = sum(column)/length(column); #Calculate the mean
        sampleMeans = c(sampleMeans, calculatedMean); #Add it to the array
      }

      #Calculate the covariance matrix:
      covariance_matrix = cov(inputData);

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

      for(i in 1:length(distances_squared)){
        if(distances_squared[i] > critical_value){
          message(sprintf("The observation %d is an outlier", i));
          message(sprintf("The values of the observation are: "));
          print(inputData[i,]);
        }
      }
    }else{
      stop("Alpha can't be higher than 1 or smaller than 0");
    }
  }
}
