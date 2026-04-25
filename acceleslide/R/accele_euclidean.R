#' @title accele_euclidean
#'
#' @param x Data frame with accelerometer data containing columns x, y and z.
#'
#'
#' @return Calculates Acceleration vector as the euclidean norm of three
#' acceleration coordinates (x, y and z), and appends to a new column in a data frame.
#'
#'
#' @examples
#'
#' # Create a new data frame with the column euclidean
#' df_eucl <- accel_euclidean(example_accel_df)
#'
#'


accele_euclidean <- function(x) {
  x[, "euclidean"] <- NA
  x["euclidean"] <- sqrt((x$x)^2 + (x$y)^2 + (x$z)^2)
  return(x)
}
