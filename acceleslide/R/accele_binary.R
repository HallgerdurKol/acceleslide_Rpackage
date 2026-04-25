#' @title accele_binary
#'
#'@description
#' Should primarily be used inside the accele_windslide function after using accele_euclidean
#'
#' @param sub_vector containing euclidean_norm values of accelerometer data
#' @param ...
#'
#' @return Returns binary values +1/-1 depending on whether the acceleration in the
#' sub_vector increased or decreased.
#'
#'
#' @examples
#'
#' # Should primarily be used inside the accele_windslide function after using accele_euclidean
#' df_window <- accele_windslide(df_eucl, FUN = accele_binary, window = 1000)
#'
#'



accele_binary <- function(sub_vector, ...) {

  neg_change <- min(sub_vector) - sub_vector[[1]] # Calculate the largest negative change in acceleration
  pos_change <- max(sub_vector) - sub_vector[[1]] # Calculate the largest positive change in acceleration

  # Determine whether the positive or negative change is larger
  if (abs(neg_change) > abs(pos_change)) {
    binary_accel <- -1
  } else if (abs(pos_change) > abs(neg_change)) {
    binary_accel <- +1
  } else {
    binary_accel <- 0 # If the pos and neg change have the same absolute values, return 0
  }
  return(binary_accel)
}













