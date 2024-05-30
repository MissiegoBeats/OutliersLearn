#' DBSCAN_method
#'
#' Outlier detection method using DBSCAN
#'
#' @author Andres Missiego Manjon
#' @param inputData Input Data (must be a data.frame)
#' @param max_distance_threshold This is used to calculate the distance between all the points and check if the euclidean distance is less than the max_distance_threshold parameter to decide if add it to the neighbors or not
#' @param min_pts the minimum number of points to form a dense region
#' @param tutorialMode if TRUE the tutorial mode is activated (the algorithm will include an explanation detailing the theory behind the outlier detection algorithm and a step by step explanation of how is the data processed to obtain the outliers following the theory mentioned earlier)
#'
#' @examples
#' inputData = t(matrix(c(3,2,3.5,12,4.7,4.1,5.2,4.9,7.1,6.1,6.2,5.2,14,5.3),2,7,dimnames=list(c("r","d"))));
#' inputData = data.frame(inputData);
#' plot(inputData);
#' eps = 4;
#' min_pts = 3;
#' DBSCAN_method(inputData, eps, min_pts, FALSE);
#'
#' inputData = t(matrix(c(3,2,3.5,12,4.7,4.1,5.2,4.9,7.1,6.1,6.2,5.2,14,5.3),2,7,dimnames=list(c("r","d"))));
#' inputData = data.frame(inputData);
#' DBSCAN_method(inputData, 4, 3, TRUE);
#'
#' @export

DBSCAN_method <- function(inputData, max_distance_threshold, min_pts, tutorialMode){
  #Conversion to matrix if it is a data frame (this is common to both options)
  #and it's not relevant for the explanation
  if (is.data.frame(inputData)) {
    inputData = as.matrix(inputData);
  } else {
    stop("inputData must be a dataframe")
  }

  if(tutorialMode){
    message("The tutorial mode has been activated for the DBSCAN algorithm (outlier detection)");
    message("Before processing the data, we must understand the algorithm and the 'theory' behind it.");
    message("The DBSCAN algorithm is based in this steps:");
    message("\tStep 1: Initializing parameters");
    message(sprintf("\tMax distance threshold: %.4f", max_distance_threshold));
    message(sprintf("\tMinPts: %.4f", min_pts));
    message("\tStep 2: Executing main loop");
    message("\t\tIf a point has already been visited, it skips to the next point.");
    message("\t\tIt then finds all neighbors of the current point within a distance of max_distance_threshold using the Euclidean distance function.");
    message("\t\tIf the number of neighbors is less than min_pts, the point is marked as noise (-1) and the loop proceeds to the next point.");
    message("\t\tOtherwise, a new cluster is created, and the current point is assigned to this cluster.");
    message("\t\tThe algorithm then iterates over the neighbors of the current point, marking them as visited and recursively expanding the neighborhood.");
    message("\t\tIf a neighbor already belongs to a cluster, it assigns the same cluster id to the current point.");
    message("\t\tAfter processing all points, the algorithm checks for outliers (points marked as -1) in the visited_array.");
    message("\tStep 3: Identifying outliers");
    message("\t\tIf a point is marked as noise (-1), it is identified as an outlier.");
    message("With this simple steps explained, let's see how this is executed over the dataset given");

    #Create the necessary data structures for the algorithm
    cluster_id = 0;
    visited_array = numeric(nrow(inputData)); #0 means undefined. -1 means noise. >0 means cluster id
    clusters = numeric(nrow(inputData));
    for(i in 1:nrow(inputData)){
      message(sprintf("Checking if the point %d has already been visited", i));
      if(visited_array[i] != 0){
        print("It has been visited, going to next point");
        message("------------------------------------------------------")
        next;
      }
      print("It has not been visited");
      message("Calculate the distance between this point and the rest of the points. This is the equivalent to the RangeQuery() functionality")
      distances = c();
      neighbors = c();
      for(j in 1:nrow(inputData)){
        message("Checking if the euclidean distance is less than the max_distance_threshold");
        if(euclidean_distance(inputData[i,],inputData[j,]) <= max_distance_threshold){
          print("Smaller, adding to neighbors");
          neighbors = c(neighbors,j);
        }else{
          print("Bigger, not adding to neighbors");
        }
      }
      message(sprintf("Point %d neighbors: ", i));
      print(neighbors);
      message("Is length of neighbors smaller than min_pts?")
      if(length(neighbors) < min_pts){
        print(sprintf("It's smaller, classifying the point %d as an outlier and skipping to next point",i));
        visited_array[i] = -1;
        message("------------------------------------------------------")
        next;
      }else{
        print(sprintf("It's bigger, adding the point %d to a cluster", i));
      }

      cluster_id = cluster_id + 1;
      message("Executing the expandCluster() functionality");
      print(sprintf("Adding point %d to cluster %d", i, cluster_id));
      clusters[i] = cluster_id; #Add P to cluster C
      for(j in 1:length(neighbors)){ #if neighbor is not visited
        print("Checking every single neighbor for the point");
        if(visited_array[neighbors[j]] != 0){
          print(sprintf("Neighbor %d has already been visited.", neighbors[j]));
          visited_array[neighbors[j]] = 1; #mark as visited
          distances_neighbors = c();
          neighbors_aux = c();
          for(k in 1:nrow(inputData)){
            if(euclidean_distance(inputData[neighbors[j],], inputData[k,]) <= max_distance_threshold){
              neighbors_aux = c(neighbors_aux,k);
            }
          }
          if(length(neighbors_aux) >= min_pts){
            neighbors = union(neighbors, neighbors_aux);
            print(sprintf("Neighbor %d has %d additional neighbors.", neighbors[j], length(neighbors_aux)));
          }
        }else if(clusters[neighbors[j]] != 0){
          print(sprintf("Neighbor %d belongs to another cluster.", neighbors[j]));
          clusters[neighbors[j]] = cluster_id;
        }
      }
      message("Process finished for this point, skipping to next point");
      message("------------------------------------------------------");
    }
    message("Checking the visited array looking for the points classified as outliers");
    for(i in 1:length(visited_array)){
      if(visited_array[i] == -1){
        message(sprintf("The point %d is an outlier", i))
      }
    }
    message("The algorithm has ended")
  }else{
    #Create the necessary data structures for the algorithm
    cluster_id = 0;
    visited_array = numeric(nrow(inputData)); #0 means undefined. -1 means noise. >0 means cluster id
    clusters = numeric(nrow(inputData));
    for(i in 1:nrow(inputData)){
      if(visited_array[i] != 0){
        next;
      }

      #Calculate the distance between this point (i) and the rest of the points. This is the
      #equivalent to the RangeQuery() functionality
      distances = c();
      neighbors = c();
      for(j in 1:nrow(inputData)){
        if(euclidean_distance(inputData[i,],inputData[j,]) <= max_distance_threshold){
          neighbors = c(neighbors,j);
        }
      }

      if(length(neighbors) < min_pts){
        visited_array[i] = -1;
        next;
      }

      cluster_id = cluster_id + 1;
      #expandCluster
      clusters[i] = cluster_id; #Add P to cluster C
      for(j in 1:length(neighbors)){ #if neighbor is not visited
        if(visited_array[neighbors[j]] != 0){
          visited_array[neighbors[j]] = 1; #mark as visited
          distances_neighbors = c();
          neighbors_aux = c();
          for(k in 1:nrow(inputData)){
            if(euclidean_distance(inputData[neighbors[j],], inputData[k,]) <= max_distance_threshold){
              neighbors_aux = c(neighbors_aux,k);
            }
          }
          if(length(neighbors_aux) >= min_pts){
            neighbors = union(neighbors, neighbors_aux);
          }
        }else if(clusters[neighbors[j]] != 0){
          clusters[neighbors[j]] = cluster_id;
        }
      }
    }

    for(i in 1:length(visited_array)){
      if(visited_array[i] == -1){
        message(sprintf("The point %d is an outlier", i))
      }
    }
  }
}


