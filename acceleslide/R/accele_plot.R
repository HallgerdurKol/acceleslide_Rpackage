#'@title accele_plot
#'
#'@description
#' For plotting data after using accele_euclidean followed by accele_windowslide.
#'
#'@param x Data frame made using accele_windslide. Should contain time and
#'column "accele_threshold" or "accele_binary"
#'@param thresh_or_bi Provide either "threshold" or "binary"
#'
#'

#'
#'@return
#' Returns plot with the acceleration vector and values calculated through
#' either accele_binary or accele_threshold. Use after accele_windowslide.
#'
#'@examples
#' # If the accele_threshold function was used in accele_windowslide
#' accele_plot(df_window, thresh_or_bi = "threshold")
#'
#'# If the accele_binary function was used in accele_windowslide
#' accele_plot(df_window, thresh_or_bi = "binary")



accele_plot <- function(x, thresh_or_bi, ...) {

  fun_name <- paste0("accele_", thresh_or_bi)

  longer_df <- tidyr::pivot_longer(
    x,
    cols = c("euclidean", fun_name),
    names_to = "data_type",
    values_to = "value")

  p <- ggplot2::ggplot(longer_df) +
    geom_line(aes(x = time, y = value, color = data_type), na.rm = T) +
    scale_x_datetime(date_labels = "%Y-%m-%d %H:%M:%S") +
    theme_classic() +
    labs(y = "acceleration", x = "time") +
    scale_color_manual(values = c("blue", "black"), guide = "none")

  return(p)

}


























