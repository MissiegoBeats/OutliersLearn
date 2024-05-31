# OutliersLearn R Package

## Overview

OutliersLearn is an R package designed to teach and demonstrate different outlier detection algorithms. The algorithms are programmed to provide informative messages while executing on real data, helping users understand the inner workings of each algorithm.

## Installation

Will be available to download/install from CRAN
To install from GitHub execute this commands in your R session:

```
install.packages("devtools")
library(devtools)
install_github("MissiegoBeats/OutliersLearn")
library(OutliersLearn)
```

To install from CRAN: 

```
install.packages("OutliersLearn")
library(OutliersLearn)
```

In case you want to install the R package using a specific CRAN Mirror:

```
install.packages("OutliersLearn", repos="<CRAN Mirror URL>")
library(OutliersLearn)
```

## Algorithms included
- Box & whiskers

  `boxandwhiskers()`
  
- Standard Deviation Method
  
  `sd_method()`
  
- K neighbors
  
  `knn()`
  
- Local Outlier Factor (Simplified Version)
  
  `lof()`

- DBSCAN

  `DBSCAN_method()`

- Mahalanobis Distance Method

  `mahalanobis_method()`


## Other functions included
- manhattan distance function
  
  `manhattan_dist()`

- euclidean distance function
  
  `euclidean_distance()`

- quantile function
  
  `quantile_outliersLearn()`

- transform to vector function
  
  `transform_to_vector()`

- Mean of a vector

  `mean_outliersLearn()`

- Standard deviation of a vector

  `sd_outliersLearn()`

- Mahalanobis distance

  `mahalanobis_distance()`

  
See more about them using the command `help()`

## Licence
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Contact me
If there is any question, feel free to open a new issue with the "question" label. If needed, i'll add a Q&A section in the repository issues
