#' lof
#'
#' Local Outlier Factor algorithm to detect outliers
#'
#' @author Andres Missiego Manjon
#' @param inputData Input Data (must be a data.frame)
#' @param K This number represents the nearest neighbor to use to calculate the density of each point. This value is chosen arbitrarily and is responsibility of the data scientist/user to select a number adequate to the dataset.
#' @param threshold Value that is used to classify the points comparing it to the calculated ARDs of the points in the dataset. If the ARD is smaller, the point is classified as an outliers. If not, the point is classified as a normal point (inlier)
#' @param tutorialMode if TRUE the tutorial mode is activated (the algorithm will include an explanation detailing the theory behind the outlier detection algorithm and a step by step explanation of how is the data processed to obtain the outliers following the theory mentioned earlier)
#'
#' @examples
#' inputData = t(matrix(c(3,2,3.5,12,4.7,4.1,5.2,
#' 4.9,7.1,6.1,6.2,5.2,14,5.3),2,7,dimnames=list(c("r","d"))));
#' inputData = data.frame(inputData);
#' lof(inputData,3,0.5,FALSE) #Can be changed to TRUE
#'
#' @importFrom graphics points
#' @export

lof <- function(inputData, K, threshold, tutorialMode) {

  #Conversion to matrix if it is a data frame (this is common to both options)
  #and it's not relevant for the explanation
  if (is.data.frame(inputData)) {
    inputData = as.matrix(inputData);
  } else {
    stop("inputData must be a dataframe")
  }

  if (tutorialMode) {#Case the user wants the full explanation
    message("The tutorial mode has been activated for the simplified LOF algorithm (outlier detection)");
    message("Before processing the data, we must understand the algorithm and the 'theory' behind it.");
    message("This is a simplified version of the LOF algorithm. This version detects outliers going though this steps:");
    message("\t1) Calculate the degree of outlier of each point by obtaining the density of each point. This has 4 substeps:");
    message("\t\ta. Determine the 'order number' (K) or closest neighbor that will be used to calculate the density of each number (arbitrary)");
    message("\t\tb. Calculate the distance between each point and the resto of the points, this distance is calculated with the Manhattan distance equation/function:")
    message("\t\t\tThe equation is this: manhattanDistance(A,B) = |A_x - B_x| + |A_y - B_y|");
    message("\t\tc.	Calculate the cardinal for each point: N is the set that contains the neighbors which distance xi is the same or less than the K nearest neighbor.");
    message("\t\td.	Calculate the density for each point. This is a technique very close to the proximity.");
    message("\t\t\tThe function to calculate the density is this: density*italic(x[i], K) == (frac(sum(italic(x[j]) %in% N(italic(x[i], K)), distance(italic(x[i]), italic(x[j]))), cardinalN(italic(x[i], K)))^-1");
    message("\t2) Calculate the average relative density for each point using the next equation: ");
    message("\t\tard*italic(x[i], K) == frac(density*italic(x[i], K), frac(sum(italic(x[j]) %in% N(italic(x[i], K)), density*italic(x[j], K)), cardinalN(italic(x[i], K))))");
    message("\t\tThis calculates the proportion between a point and the average mean of the densities of the set N that defines that point using the order number K. The average distance will tend to 0 on the outliers.")
    message("\t3)	Obtain the outliers: will classify a point as an outlier when the average relative density is significantly smaller than the rest of the elements in the sample");
    message("\t\t In the current LOF simplified implemented algorithm, it has been chosen to implement this last step with a threshold specified by the user");
    message("\t\t This threshold value is compared to each ARD calculated for each point. If the value is smaller than the threshold, then the point is classified as an outlier");
    message("\t\t On the other hand, if the value is greater or equal to the threshold, the point is classified as an  inlier (a normal point)")
    message("Now that we understand how the algorithm works, it will be executed to the input data with the parameters that have been set");
    message("Calculate Euclidean distances between all points:");
    #First we have to calculate the distances
    distances = c();
    #Euclidean distances between each pair of points
    for (i in 1:dim(inputData)[1]) {
      #Point we are going to compare:
      pointA = c(inputData[i, 1], inputData[i, 2]);
      for (j in 1:dim(inputData)[1]) {
        #Point to be compared with pointA
        pointB = c(inputData[j, 1], inputData[j, 2]);
        message("Calculating distance between points (manhattan distance):")
        print(i);
        print(j);
        distances = c(distances, manhattan_dist(pointA, pointB));
        print(sprintf("Calculated distance: %.4f",manhattan_dist(pointA, pointB)));
      }
    }
    message("The calculated matrix of distances is:");
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
      message("In column");
      print(i);
      message("Cardinal calculated:");
      print(pos - 1);
      cardinals = c(cardinals, pos - 1);
    }
    message("The cardinals vector resulting is:");
    print(cardinals);
    message("With the obtained cardinals, we get the densities of each point:");
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
      message("For point");
      print(i);
      density = (sum / cardinal)^(-1);
      message("Value of density: ");
      print(density);
      densities = c(densities, density);
      used_points = c(used_points, list(tempPoints));
    }
    message("All densities calculated: ");
    print(densities);
    message("With the calculated densities, we are going to calculate the average relative density (ard) for each point:");
    ards = c();
    for (i in 1:length(densities)) {
      sum_densities = 0;
      point_density = densities[i];
      for (j in 1:length(used_points[[i]])) {
        sum_densities = sum_densities + densities[used_points[[i]][[j]]];
      }
      message("For point: ");
      print(i);
      ard = point_density / (sum_densities / cardinals[i]);
      message("Average Relative Density calculated: ");
      print(ard);
      ards = c(ards, ard);
    }
    message("All the ards calculated: ");
    print(ards);

    message("The last step is to classify the outliers comparing the ards calculated with the threshold");
    print(sprintf("Threshold selected: %f", threshold))
    plot(1, type="n", main="Result", xlab="X Value", ylab="Y Value", xlim=c(0, length(inputData) + 1), ylim=range(inputData));
    for(i in 1:length(ards)){
      if(ards[i] < threshold){
        message(sprintf("The point %d is an outlier because its ard is lower than %f", i, threshold));
        message(sprintf("The point %d has an average relative density of %.4f", i, ards[i]));
        points(inputData[i,1],inputData[i,2],col="red",pch=16);
      }else{
        points(inputData[i,1],inputData[i,2],col="blue",pch=16);
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

    #Classify the points comparing with the threshold
    print(sprintf("Threshold selected: %f", threshold))
    plot(1, type="n", main="Result", xlab="X Value", ylab="Y Value", xlim=c(0, length(inputData) + 1), ylim=range(inputData));
    for(i in 1:length(ards)){
      if(ards[i] < threshold){
        message(sprintf("The point %d is an outlier because its ard is lower than %f", i, threshold));
        message(sprintf("The point %d has an average relative density of %.4f", i, ards[i]));
        points(inputData[i,1],inputData[i,2],col="red",pch=16);
      }else{
        points(inputData[i,1],inputData[i,2],col="blue",pch=16);
      }
    }
  }
}
