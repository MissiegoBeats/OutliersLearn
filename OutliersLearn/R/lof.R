#' lof
#'
#' Local Outlier Factor algorithm to detect outliers
#'
#' @author Andres Missiego Manjon
#' @param inputData Input Data (must be a data.frame)
#' @param K
#' @param threshold
#' @param tutorialMode if TRUE the tutorial mode is activated (the algorithm will include an explanation detailing the theory behind the outlier detection algorithm and a step by step explanation of how is the data processed to obtain the outliers following the theory mentioned earlier)
#'
#' @examples
#'
#' @export

lof <- function(inputData, K, tutorialMode) {

  #Conversion to matrix if it is a data frame (this is common to both options)
  #and it's not relevant for the explanation
  if (is.data.frame(inputData)) {
    inputData = as.matrix(inputData);
  } else {
    stop("inputData must be a dataframe")
  }

  if (tutorialMode) {#Case the user wants the full explanation

    #TODO: add theorical explanation & complete the explanation of the execution

    message("Calculate Euclidean distances between all points:")
    #First we have to calculate the distances
    distances = c();
    #Euclidean distances between each pair of points
    for (i in 1:dim(inputData)[1]) {
      #Point we are going to compare:
      message("\tCalculating distance between points:")
      pointA = c(inputData[i, 1], inputData[i, 2]);
      print(pointA);
      for (j in 1:dim(inputData)[1]) {
        #Point to be compared with pointA
        pointB = c(inputData[j, 1], inputData[j, 2]);
        print(pointB);
        distances = c(distances, manhattan_dist(pointA, pointB));
        message("\tCalculated distance:");
        print(manhattan_dist(pointA, pointB));
      }
    }
    message("The matrix of Euclidean distances is as follows:");
    distances = matrix(distances, dim(inputData)[1], dim(inputData)[1]);
    print(distances);
    distancesOriginal = distances; #Will be used later (we need it unsorted by columns)
    message("After calculating the distances between points, we calculate the cardinal for each point");
    message("To do this, we need to sort the distance matrix (by columns)");
    for (i in 1:dim(distances)[2]) {
      distances[, i] = sort(distances[, i]);
    }
    message("The distance matrix sorted by columns is as follows:");
    print(distances);
    message("We obtain a vector of the cardinals");
    cardinals = c();
    for (i in 1:dim(distances)[2]) {
      column = distances[, i];
      #We reach up to K (could be done with a loop but this way is more efficient)
      pos = K;
      #We check how many values are equal to the one at position K
      while (column[pos] == column[pos + 1]) {
        pos = pos + 1;
      }
      #The obtained cardinals have 1 added since they count the distance with itself
      #So we subtract 1
      cardinals = c(cardinals, pos - 1);
    }
    print(cardinals);
    #With the obtained cardinals, we get the densities of each point:
    densities = c();
    used_points = c(); #It is a structure of inputData to store the points that have been used for each
    #point to calculate its density.
    for (i in 1:dim(distances)[2]) {
      column = distances[, i];
      cardinal = cardinals[i];
      loop_limit = cardinal + 1; #This way the loop goes up to cardinal inclusive
      sum = 0;
      tempPoints = c();
      for (j in 2:loop_limit) { #We skip the first position since it is the distance with itself
        sum = sum + column[j];
        for (k in 1:dim(distancesOriginal)[1]) { #dim(distancesOriginal)[1] -> number of rows, that is, number of points
          if (distancesOriginal[k, i] == column[j] && !is.element(k, tempPoints)) {
            tempPoints = c(tempPoints, k); #We add the point to the points used for point i
          }
        }
      }
      density = (sum / cardinal)^(-1);
      densities = c(densities, density);
      used_points = c(used_points, list(tempPoints));
    }
    #With the calculated densities, we are going to calculate the average relative density (ard) for each point:
    ards = c();
    for (i in 1:length(densities)) {
      sum_densities = 0;
      point_density = densities[i];
      for (j in 1:length(used_points[[i]])) {
        sum_densities = sum_densities + densities[used_points[[i]][[j]]];
      }
      ard = point_density / (sum_densities / cardinals[i]);
      ards = c(ards, ard);
    }
    #Now we calculate the the mean and the standard deviation of the calculated ards (average relative densities)
    mean = mean_outliersLearn(ards);
    sd = sd_outliersLearn(ards, mean);

    #With that calculated, we calculate the top and low limits of the ards so that every single point that has a ard out of
    #this limits is considered an outlier.
    low_limit = mean - 2*sd;
    top_limit = mean + 2*sd;

    for(i in 1:length(ards)){
      if(ards[i] < low_limit || ards[i] > top_limit){
        message(sprintf("The point %d is an outlier", i))
      }
    }

  } else {
    #First we have to calculate the distances
    distances = c();
    #Euclidean distances between each pair of points
    for (i in 1:dim(inputData)[1]) {
      #Point we are going to compare:
      pointA = c(inputData[i, 1], inputData[i, 2]);
      for (j in 1:dim(inputData)[1]) {
        #Point to be compared with pointA
        pointB = c(inputData[j, 1], inputData[j, 2]);
        distances = c(distances, manhattan_dist(pointA, pointB));
      }
    }
    distances = matrix(distances, dim(inputData)[1], dim(inputData)[1]);
    distancesOriginal = distances; #Will be used later (we need it unsorted by columns)
    #After calculating the distances between points, we calculate the cardinal for each point.
    #To do this, we need to sort the distance matrix (by columns)
    for (i in 1:dim(distances)[2]) {
      distances[, i] = sort(distances[, i]);
    }
    #We obtain a vector of the cardinals
    cardinals = c();
    for (i in 1:dim(distances)[2]) {
      column = distances[, i];
      #We reach up to K (could be done with a loop but this way is more efficient)
      pos = K;
      #We check how many values are equal to the one at position K
      while (column[pos] == column[pos + 1]) {
        pos = pos + 1;
      }
      #The obtained cardinals have 1 added since they count the distance with itself
      #So we subtract 1
      cardinals = c(cardinals, pos - 1);
    }
    #With the obtained cardinals, we get the densities of each point:
    densities = c();
    used_points = c(); #It is a structure of inputData to store the points that have been used for each
    #point to calculate its density.
    for (i in 1:dim(distances)[2]) {
      column = distances[, i];
      cardinal = cardinals[i];
      loop_limit = cardinal + 1; #This way the loop goes up to cardinal inclusive
      sum = 0;
      tempPoints = c();
      for (j in 2:loop_limit) { #We skip the first position since it is the distance with itself
        sum = sum + column[j];
        for (k in 1:dim(distancesOriginal)[1]) { #dim(distancesOriginal)[1] -> number of rows, that is, number of points
          if (distancesOriginal[k, i] == column[j] && !is.element(k, tempPoints)) {
            tempPoints = c(tempPoints, k); #We add the point to the points used for point i
          }
        }
      }
      density = (sum / cardinal)^(-1);
      densities = c(densities, density);
      used_points = c(used_points, list(tempPoints));
    }
    #With the calculated densities, we are going to calculate the average relative density (ard) for each point:
    ards = c();
    for (i in 1:length(densities)) {
      sum_densities = 0;
      point_density = densities[i];
      for (j in 1:length(used_points[[i]])) {
        sum_densities = sum_densities + densities[used_points[[i]][[j]]];
      }
      ard = point_density / (sum_densities / cardinals[i]);
      ards = c(ards, ard);
    }

    #Now we calculate the the mean and the standard deviation of the calculated ards (average relative densities)
    mean = mean_outliersLearn(ards);
    sd = sd_outliersLearn(ards, mean);

    #With that calculated, we calculate the top and low limits of the ards so that every single point that has a ard out of
    #this limits is considered an outlier.
    low_limit = mean - 2*sd;
    top_limit = mean + 2*sd;

    for(i in 1:length(ards)){
      if(ards[i] < low_limit || ards[i] > top_limit){
        message(sprintf("The point %d is an outlier", i))
      }
    }
  }
}
