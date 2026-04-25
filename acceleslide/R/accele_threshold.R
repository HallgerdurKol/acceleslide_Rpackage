#' @title accele_threshold
#'
#' @description
#' Should primarily be used inside the accele_windslide function after using accele_euclidean
#'
#' @param sub_vector Containing euclidean_norm values of accelerometer data
#' @param threshold Set threshold value
#'
#' @return Returns the change in euclidean norm value only if the increase or decrease in
#' acceleration exceeds a threshold value in the window (sub-vector).
#'
#' @examples
#' # Should primarily be used inside the accele_windslide function after using accele_euclidean
#' df_window <- accele_windslide(df_eucl, FUN = accele_threshold, window = 1000, threshold = 0.5)
#'


accele_threshold <- function(sub_vector, threshold = 0.5, ...) {

  neg_change <- min(sub_vector) - sub_vector[[1]] # Calculate the largest negative change in acceleration
  pos_change <- max(sub_vector) - sub_vector[[1]] # Calculate the largest positive change in acceleration

  # Determine whether the positive or negative change is larger
  if (abs(neg_change) > abs(pos_change)) {
    largest_change <- neg_change
  } else if (abs(pos_change) > abs(neg_change)) {
    largest_change <- pos_change
  } else {
    largest_change <- 0 # If the pos and neg change have the same absolute values, return 0
  }

  # Determine whether the largest change in acceleration exceeds the threshold
  if (largest_change > threshold) {
    accel_change <- largest_change # Returns acceleration change value
  } else {
    accel_change <- 0             # Returns 0 if acceleration change does not exceed threshold
  }

  return(accel_change)
}





