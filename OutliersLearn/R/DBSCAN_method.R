#' DBSCAN_method
#'
#' Outlier detection method using DBSCAN
#'
#' @param inputData Input Data (must be a data.frame)
#' @param max_distance_threshold
#' @param min_pts
#' @param tutorialMode if TRUE the tutorial mode is activated (the algorithm will include an explanation detailing the theory behind the outlier detection algorithm and a step by step explanation of how is the data processed to obtain the outliers following the theory mentioned earlier)
#'
#' @examples
#'
#' @export

DBSCAN_method <- function(inputData, max_distance_threshold, min_pts, tutorialMode){
  #Algorithm pseudocode:
  #https://www.researchgate.net/figure/Pseudocode-of-DBSCAN-Algorithm-28_fig2_339165857
  #https://cse.buffalo.edu/~jing/cse601/fa13/materials/clustering_density.pdf

  #Conversion to matrix if it is a data frame (this is common to both options)
  #and it's not relevant for the explanation
  if (is.data.frame(inputData)) {
    inputData = as.matrix(inputData);
  } else {
    stop("inputData must be a dataframe")
  }

  if(tutorialMode){

    #TODO: finish the tutorial messages
    message("The tutorial mode has been activated for the DBSCAN algorithm (outlier detection)");
    message("Before processing the data, we must understand the algorithm and the 'theory' behind it.");

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
      #Pseudocode implementation of this: https://cse.buffalo.edu/~jing/cse601/fa13/materials/clustering_density.pdf
      print(neighbors);
      print(visited_array);
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
      #Pseudocode implementation of this: https://cse.buffalo.edu/~jing/cse601/fa13/materials/clustering_density.pdf
      print(neighbors);
      print(visited_array);
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


