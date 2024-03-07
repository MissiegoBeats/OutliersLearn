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
  #Conversion to matrix if it is a data frame (this is common to both options)
  #and it's not relevant for the explanation
  if (is.data.frame(inputData)) {
    inputData = as.matrix(inputData);

  } else {
    stop("inputData must be a dataframe")
  }

  #Create the necessary data structures for the algorithm
  cluster_id = 0;
  visited_array = numeric(nrow(inputData)); #0 means not visited. 1 means visited. -1 means noise
  clusters = c();

  for(i in 1:nrow(inputData)){
    if(visited_array[i] == 0){ #Only process if not visited
      visited_array[i] = 1;
      #Calculate the distance between this point and the rest of the points
      distances = c();
      neighbors = c();
      for(j in 1:nrow(inputData)){
        if(euclidean_distance(inputData[i,],inputData[j,]) <= max_distance_threshold){
          neighbors = c(neighbors,j);
        }
      }
      if(length(neighbors) < min_pts){
        visited_array[i] = -1;
      }else{
        cluster_id = cluster_id+1;
        clusters[i] = cluster_id;
        visited_array[i] = 1;
        for(k in 1:length(neighbors)){
          if(visited_array[k]==0){
            visited_array[k] = 1;
          }
        }
      }
    }
  }
}

#Pseudocode for this algorithm:
#DBSCAN_OutlierDetection(data, eps, minPts):
# Initialize cluster_id to 0
#
# for each point p in data:
#   if p is visited:
#   continue
# mark p as visited
# neighbors = regionQuery(p, eps)
#
# if size(neighbors) < minPts:
#   mark p as noise
# else:
#   cluster_id = cluster_id + 1
# expandCluster(p, neighbors, cluster_id, eps, minPts)
#
# outliers = findNoisePoints(data)
# return outliers
#
# findNoisePoints(data):
#   outliers = empty list
# for each point p in data:
#   if p is noise:
#   add p to outliers
# return outliers
