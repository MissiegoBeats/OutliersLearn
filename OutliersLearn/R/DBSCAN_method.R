#' DBSCAN_method
#'
#' Outlier detection method using DBSCAN
#'
#' @param inputData Input Data
#'
#' @examples
#'
#' @export

DBSCAN_method <- function(inputData, max_distance_threshold, min_pts){
  #Algorithm pseudocode:
  #https://www.researchgate.net/figure/Pseudocode-of-DBSCAN-Algorithm-28_fig2_339165857

  #Conversion to matrix if it is a data frame (this is common to both options)
  #and it's not relevant for the explanation
  if (is.data.frame(inputData)) {
    inputData = as.matrix(inputData);
  } else {
    stop("inputData must be a dataframe")
  }

  #Create the necessary data structures for the algorithm
  cluster_id = 0;
  visited_array = numeric(nrow(inputData)); #0 means undefined. -1 means noise. >0 means cluster id
  clusters = c();
  for(i in 1:nrow(inputData)){

    if(visited_array[i] == 0){
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
    visited_array[i] = cluster_id;
    j = 1;
    while(j <= length(neighbors)){
      if(j!=i){
        if(visited_array[j] == -1){
          visited_array[j] = cluster_id;
        }
        if(visited_array[j] == 0){
          j = j+1;
          next;
        }

        #Calculate the distance between this point (j) and the rest of the points. This is the
        #equivalent to the RangeQuery() functionality
        distances = c();
        neighbors_aux = c();
        for(k in 1:nrow(inputData)){
          if(euclidean_distance(inputData[j,],inputData[k,]) <= max_distance_threshold){
            neighbors_aux = c(neighbors_aux,k);
          }
        }

        visited_array[j] = cluster_id;
        if(length(neighbors) < min_pts){
          j = j+1;
          next;
        }

        neighbors = c(neighbors, neighbors_aux)
        j = j+1;
      }
    }
  }

  for(i in 1:length(visited_array)){
    if(visited_array[i] == -1){
      message(sprintf("The point %d is an outlier", i))
    }
  }

}


