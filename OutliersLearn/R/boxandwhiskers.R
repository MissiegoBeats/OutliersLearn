#' Box And Whiskers
#'
#' This function implements the box & whiskers algorithm to detect outliers
#'
#' @author Andres Missiego Manjon
#' @param data Input data.
#' @param d Degree of outlier or distance at which an event is considered an outlier
#' @param tutorialMode if TRUE the tutorial mode is activated (the algorithm will include an explanation detailing the theory behind the outlier detection algorithm and a step by step explanation of how is the data processed to obtain the outliers following the theory mentioned earlier)
#' @examples
#' inputData = t(matrix(c(3,2,3.5,12,4.7,4.1,5.2,4.9,7.1,6.1,6.2,5.2,14,5.3),2,7,dimnames=list(c("r","d"))))
#' inputData = data.frame(inputData)
#' print(inputData)
#' inputData <- transform_to_vector(inputData)
#' print(inputData)
#' boxandwhiskers(inputData,2,TRUE)
#' boxandwhiskers(inputData,2,FALSE)
#'
#' @export
boxandwhiskers <- function(data,d,tutorialMode){
  #TODO: comprobar si tendriamos que hacer source de las funciones que vamos a usar (en principio no)

  #Now we must preprocess the data to "standardize" it.
  #It's important to understand that we must transform N dimensional arrays to 1D arrays (e.g. to a vector)
  #because of the nature of this algorithm (it does not process multidimensional arrays)
  #We make use of the transform_to_vector() function included in this package
  data <- transform_to_vector(data)

  if(tutorialMode){ #Case that has the tutorialMode activated
    message("The tutorial mode has been activated for the box and whiskers algorithm (outlier detection)")
    message("Before processing the data, we must understand the algorithm and the 'theory' behind it.")
    message("The algorithm is made up with 4 steps: ")
    message("\tStep 1: Determine the degree of outlier or distance at which an event is considered an outlier (arbitrary). We will name it 'd'")
    message("\tStep 2: Sort the data and obtain quartiles")
    message("\tStep 3: Calculate the interval limits for outliers using the equation:")
    message("\t\t(Q_1 - d * (Q_3 - Q_1), Q_3 + d * (Q_3 - Q_1))")
    message("\tBeing Q_1 and Q_3 the 1st and 3rd cuartile. Notice that here we use the value 'd' (it affects on the results so it must be carefully chosen)")
    message("\tStep 4: Identify outliers as values that fall outside the interval calculated in step 3")
    keepAsking = TRUE
    while(keepAsking){
      learnMoreQuartiles = readline("Do you want to learn more about how to calculate quartiles? (Y/N) \n")
      learnMoreQuartiles = toupper(learnMoreQuartiles)
      if(learnMoreQuartiles == 'Y'){
        keepAsking = FALSE
        message("Quantiles are elements that allow dividing an ordered set of data into equal-sized parts.")
        message("\t-Quartiles: 4 equal parts")
        message("\t-Deciles: 10 equal parts")
        message("\t-Percentiles: 100 equal parts")
        message("The function quantile.R that has been developed gives a closer look into how quantiles are calculated:")
        dput(quantile_outliersLearn)
      }else if(learnMoreQuartiles == 'N'){
        keepAsking = FALSE
      }else{
        warning("The input must be Y or N")
      }
    }
    message("Now we will apply this knowledge to the data given to obtain the outliers")
    message("Calculating the quantiles with the function quantile() (available on this package)")
    message("First we calculate the 1st quartile (quantile(data,0.25))")
    quantile1 = quantile_outliersLearn(data,0.25);
    print(quantile1)
    message("Now we calculate the 3rd quartile (quantile(data, 0.75))")
    quantile3 = quantile_outliersLearn(data,0.75);
    print(quantile3)
    message("Using the formula given before, we obtain the interval limits:")
    limits = c(quantile1 - d*(quantile3-quantile1), quantile3 + d*(quantile3-quantile1))
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
  }else{ #Case tutorial mode is deactivated
    quantile1 = quantile_outliersLearn(data,0.25);
    quantile3 = quantile_outliersLearn(data,0.75);
    limits = c(quantile1 - d*(quantile3-quantile1), quantile3 + d*(quantile3-quantile1))
    print("Obtained limits: ");
    print(limits);
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
