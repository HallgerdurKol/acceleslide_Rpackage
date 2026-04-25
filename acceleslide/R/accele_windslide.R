#' @title accele_windslide
#'
#' @description
#' Sliding window function that iteratively works its way through an accelerometer
#' time series. Uses a data frame containing the acceleration vector of
#' accelerometer data (created using accele_euclidean) and loops through each
#' index of the euclidean column in the data frame. Depending on if you use
#' accele_binary or accele_threshold as FUN, outputs either the binary
#' +1/-1 dependent on if the acceleration in the window increased or decreased,
#' or the change in euclidean norm if the acceleration vector change in the
#' window exceeds a threshold value,
#'
#' @param x Data frame containing column "euclidean" made using function accel_euclidean
#' @param window Which window size to use
#' @param FUN Which function to use, accele_binary or accele_threshold
#' @param ...
#'
#' @return Returns the output of the function provided in FUN in a new column of a data frame
#'
#' @examples
#' # If using accele_threshold, you can change the threshold from the defaulty 0.5
#' df_window <- accele_windslide(df_eucl, FUN = accele_threshold, window = 1000, threshold = 0.5)
#'
#' # If using accele_binary
#' df_window <- accele_windslide(df_eucl, FUN = accele_threshold, window = 1000)
#'


accele_windslide <- function(x, window, FUN, ...) {

  if(deparse(substitute(FUN)) == "accele_binary" | deparse(substitute(FUN)) == "accele_threshold") {
    message("acceleslide function provided. Good job :)")
  } else {
    stop("Incorrect function provided")
  }

  x[, deparse(substitute(FUN))] <- NA # Turns FUN into a name of a new column
  vec <- x$euclidean
  tmp <- c()

  for(i in 1:length(vec)) {
    if(((i + window - 1) < length(vec))){

      sub_vec <- vec[i:(i + window - 1)]

      tmp <- c(tmp, FUN(sub_vec, ...))

      i <- i + 1
    } else {

      tmp <- c(tmp, NA)

      i <- i+ 1
    }
  }
  x[[deparse(substitute(FUN))]] <- tmp
  return(x)
}
