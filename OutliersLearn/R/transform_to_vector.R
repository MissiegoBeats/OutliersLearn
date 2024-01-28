#' transform_to_vector
#'
#' Transform any type of data to a vector
#'
#' @param data Input data.
#' @return Data formatted as a vector
#' @examples
#' numeric_data <- c(1, 2, 3)
#' character_data <- c("a", "b", "c")
#' logical_data <- c(TRUE, FALSE, TRUE)
#' factor_data <- factor(c("A", "B", "A"))
#' integer_data <- as.integer(c(1, 2, 3))
#' complex_data <- complex(real = c(1, 2, 3), imaginary = c(4, 5, 6))
#' list_data <- list(1, "apple", TRUE)
#' data_frame_data <- data.frame(x = c(1, 2, 3), y = c("a", "b", "c"))
#'
#' transformed_numeric <- transform_to_vector(numeric_data)
#' transformed_character <- transform_to_vector(character_data)
#' transformed_logical <- transform_to_vector(logical_data)
#' transformed_factor <- transform_to_vector(factor_data)
#' transformed_integer <- transform_to_vector(integer_data)
#' transformed_complex <- transform_to_vector(complex_data)
#' transformed_list <- transform_to_vector(list_data)
#' transformed_data_frame <- transform_to_vector(data_frame_data)
#'
#' @export
transform_to_vector <- function(data) {
  if (is.numeric(data)) {
    return(data)
  } else if (is.character(data)) {
    return(as.character(data))
  } else if (is.logical(data)) {
    return(as.numeric(data))
  } else if (is.factor(data)) {
    return(as.character(data))
  } else if (is.integer(data)) {
    return(as.numeric(data))
  } else if (is.complex(data)) {
    return(Mod(data))
  } else if (is.list(data)) {
    return(unlist(data))
  } else if (is.data.frame(data)) {
    return(as.vector(data))
  } else if (is.matrix(data)) {
    return(as.vector(data))
  } else if (is.array(data)) {
    return(as.vector(data))
  } else if (is.table(data)) {
    return(as.vector(data))
  } else if (is.environment(data)) {
    return(ls(data))
  } else if (isS4(data)) {
    return(unclass(data))
  } else if (is.raw(data)) {
    return(as.numeric(data))
  } else {
    warning("Data type not recognized. Returning as is.")
    return(data)
  }
}
