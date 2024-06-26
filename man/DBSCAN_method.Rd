% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DBSCAN_method.R
\name{DBSCAN_method}
\alias{DBSCAN_method}
\title{DBSCAN_method}
\usage{
DBSCAN_method(inputData, max_distance_threshold, min_pts, tutorialMode)
}
\arguments{
\item{inputData}{Input Data (must be a data.frame)}

\item{max_distance_threshold}{This is used to calculate the distance between all the points and check if the euclidean distance is less than the max_distance_threshold parameter to decide if add it to the neighbors or not}

\item{min_pts}{the minimum number of points to form a dense region}

\item{tutorialMode}{if TRUE the tutorial mode is activated (the algorithm will include an explanation detailing the theory behind the outlier detection algorithm and a step by step explanation of how is the data processed to obtain the outliers following the theory mentioned earlier)}
}
\value{
None, does not return any value
}
\description{
Outlier detection method using DBSCAN
}
\examples{
inputData = t(matrix(c(3,2,3.5,12,4.7,4.1,5.2,
4.9,7.1,6.1,6.2,5.2,14,5.3),2,7,dimnames=list(c("r","d"))));
inputData = data.frame(inputData);
eps = 4;
min_pts = 3;
DBSCAN_method(inputData, eps, min_pts, FALSE); #Can be set to TRUE

}
\author{
Andres Missiego Manjon
}
