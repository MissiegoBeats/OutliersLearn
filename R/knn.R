#' knn
#'
#' This function implements the knn algorithm for outlier detection
#'
#' @author Andres Missiego Manjon
#' @param data Input Data (must be a data.frame)
#' @param d Degree of outlier or distance at which an event is considered an outlier
#' @param K Nearest neighbor for which an event must have a degree of outlier to be considered an outlier
#' @param tutorialMode if TRUE the tutorial mode is activated (the algorithm will include an explanation detailing the theory behind the outlier detection algorithm and a step by step explanation of how is the data processed to obtain the outliers following the theory mentioned earlier)
#'
#' @return None, does not return any value
#'
#' @examples
#' inputData = t(matrix(c(3,2,3.5,12,4.7,4.1,5.2,
#' 4.9,7.1,6.1,6.2,5.2,14,5.3),2,7,dimnames=list(c("r","d"))))
#' inputData = data.frame(inputData)
#' knn(inputData,3,2,FALSE) #Can be changed to TRUE
#'
#' @importFrom graphics points
#' @export

knn <- function(data, d, K,tutorialMode){
  #Convert to matrix (to be able to process it easier)
  if(is.data.frame(data)){
    data = as.matrix(data);
  }else{
    stop("input data must be a data.frame")
  }

  if(tutorialMode){ #Case that tutorial mode is activated
    message("The tutorial mode has been activated for the KNN algorithm (outlier detection)")
    message("Before processing the data, we must understand the algorithm and the 'theory' behind it.")
    message("The knn algorithm to detect outliers is a method based on proximity. This algorithm has 2 main steps:")
    message("\tStep A: Determine the degree of outlier or distance at which an event is considered an outlier (arbitrary)")
    message("\t\tSubstep a: Arbitrarily determine the degree of outlier or distance at which an event is considered an outlier (we will name it 'd')")
    message("\t\tSubstep b: Arbitrarily determine the order number, or K, of the nearest neighbor for which an event must have a degree of outlier to be considered an outlier")
    message("\tStep B: Identify outliers using the k-Nearest Neighbors (k-NN) algorithm")
    message("\t\tSubstep a: Calculate Euclidean distances between all data points")
    message("\t\tSubstep b: Sort the neighbors of each point until reaching K")
    message("\t\tSubstep c: Identify outliers as events whose Kth neighbor is at a distance greater than the defined degree of outlier")
    message("We must define euclidean distance between 2 points (point A & point B for example). The formula is:")
    message("\tsqrt((B_x - A_x)^2 + (B_y-A_y)^2)")
    message("Being A_x and B_x the x components of the A and B points. A_y and B_y are the y components of the A and B points")
    message("Now that we know how the algorithm works, let's apply it to our data.")
    message("")
    message("First we must calculate the euclidean distance between every single point in the data")

    distances = c();
    for(i in 1:dim(data)[1]){ #dim(data)[1] is the number of rows in the data matrix
      #point A:
      pointA = c(data[i,1],data[i,2]);
      for(j in 1:dim(data)[1]){
        #point B:
        pointB = c(data[j,1],data[j,2]);
        distances = c(distances,euclidean_distance(pointA,pointB)); #We add the result to the distances vector
        print(sprintf("Euclidean distance between point %d (%.3f,%.3f) & point %d (%.3f,%.3f): %.3f", i, pointA[1],pointA[2], j, pointB[1],pointB[2],euclidean_distance(pointA,pointB)))
      }
    }
    distances = matrix(distances,dim(data)[1],dim(data)[1]); #We format the distance vector as a matrix
    message("The distances matrix obtained is: ")
    print(distances)

    message("We order the distances by columns and show the outliers")
    plot(1, type="n", main="Result", xlab="X Value", ylab="Y Value", xlim=c(0, length(data) + 1), ylim=range(data))
    for(i in 1:dim(distances)[2]){ #dim(data)[2] is the number of columns
      distances[,i] = sort(distances[,i]);
      message(sprintf("The distances matrix sorted in step %d is: ",i))
      print(distances)
      message(sprintf("The Kth neighbor for the point %d has a value of %.3f", i, distances[K,i]))
      x = data[i,1]
      y = data[i,2]
      if(distances[K,i] > d){
        message("The distance is greater than the value stablished in 'd' so it's an outlier.")
        message(sprintf("The point %d is an outlier", i))
        points(x,y,col="red",pch=16)
      }else{
        message("The distance is smaller than the value stablished in 'd' so it's not an outlier.")
        message(sprintf("The point %d is not an outlier", i))
        points(x,y,col="blue",pch=16)
      }
    }
  }else{ #Case tutorial mode deactivated
    distances = c();
    for(i in 1:dim(data)[1]){ #dim(data)[1] is the number of rows in the data matrix
      #point A:
      pointA = c(data[i,1],data[i,2]);
      for(j in 1:dim(data)[1]){
        #point B:
        pointB = c(data[j,1],data[j,2]);
        distances = c(distances,euclidean_distance(pointA,pointB)); #We add the result to the distances vector
      }
    }
    distances = matrix(distances,dim(data)[1],dim(data)[1]); #We format the distance vector as a matrix
    plot(1, type="n", main="Result", xlab="X Value", ylab="Y Value", xlim=c(0, length(data) + 1), ylim=range(data))
    for(i in 1:dim(distances)[2]){ #dim(data)[2] is the number of columns
      distances[,i] = sort(distances[,i]);
      x = data[i,1]
      y = data[i,2]
      if(distances[K,i] > d){
        message(sprintf("The point %d is an outlier", i))
        points(x,y,col="red",pch=16)
      }else{
        points(x,y,col="blue",pch=16)
      }
    }
  }
}
