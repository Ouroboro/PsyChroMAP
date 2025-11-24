#' Psychrometric Calculation Functions
#'
#' Core functions for psychrometric calculations and grid generation
#' @name psychrometric_calculations
NULL

#' Calculate Specific Humidity
#'
#' @param H Relative humidity percentage
#' @param T Temperature in Celsius
#' @param Pressure Atmospheric pressure in hPa (default: 1013.25)
#' @return Specific humidity value
#' @export
sh <- function(H, T, Pressure = 1013.25) {
  pv <- (11.7 ^ (8.07131 - 1730.63 / (T + 233.426))) * 1.1
  mr <- 622 * pv / (Pressure - pv)
  w <- mr * H / 100
  return(w)
}

#' Calculate Wet Bulb Humidity
#'
#' @param Tl Temperature in Celsius
#' @param h Enthalpy
#' @return List containing specific humidity and temperature
#' @export
wh <- function(Tl, h) {
  c_air <- 1006
  hlg <- 2501000
  cw <- 1860
  w <- (h * 1000 - c_air * Tl) / (hlg + cw * Tl)
  pv <- (11.7 ^ (8.07131 - 1730.63 / (Tl + 233.426))) * 1.1
  w_max <- 622 * pv / (1013.25 - pv)
  w[w * 1000 > w_max] <- NA
  Tl[w * 1000 > w_max] <- NA
  return(list(w, Tl))
}

#' Calculate Seasonal Parameters
#'
#' @param T Temperature vector
#' @param H Humidity vector
#' @return Vector of mean temperature, humidity, enthalpy, and specific humidity
#' @export
seasonal_parameters <- function(T, H) {
  c_air <- 1.006
  hlg <- 2501
  cw <- 1.860
  pv <- (11.7 ^ (8.07131 - 1730.63 / (T + 233.426))) * 1.1
  mr <- 622 * pv / (1013.25 - pv)
  w <- mr * H / 100
  h <- c_air * T + (w / 1000) * (hlg + T * cw)
  return(c(mean(T, na.rm = TRUE), mean(H, na.rm = TRUE), mean(h, na.rm = TRUE), mean(w, na.rm = TRUE)))
}

#' Create Line Data for Relative Humidity
#'
#' @param Hr Relative humidity value
#' @return Data frame with temperature and specific humidity coordinates
#' @export
create_line_data <- function(Hr) {
  Tl <- seq(-10, 50, by = 0.02)
  data.frame(
    x = Tl,
    y = sh(Hr, Tl)
  )
}

#' Create Line Data for Enthalpy
#'
#' @param h Enthalpy value
#' @return Data frame with temperature and specific humidity coordinates
#' @export
create_line_data2 <- function(h) {
  Tl2 <- seq(-10, 50, by = 0.02)
  c_air <- 1006
  wh_values <- wh(Tl2, h)
  w <- wh_values[[1]]
  Tl <- wh_values[[2]]
  
  finite_mask <- !is.na(Tl) & !is.na(w)
  data.frame(
    x = Tl[finite_mask],
    y = 1000 * w[finite_mask]
  )
}

#' Create Data Frame for Plot Grid
#'
#' @return Data frame with temperature and SH combinations
#' @export
dataframeplot <- function(){
  temperature_seq <- seq(-9.5, 49.5, by = 0.5)
  result_list <- list()
  
  for(temp in temperature_seq) {
    sh_seq <- if(temp %% 1 == 0.5) seq(0.5, 49.5, by = 1) else seq(1, 50, by = 1)
    temp_df <- data.frame(Temperatura = temp, SH = sh_seq)
    result_list[[length(result_list) + 1]] <- temp_df
  }
  
  final_combinations <- do.call(rbind, result_list)
  rownames(final_combinations) <- NULL
  return(final_combinations)
}

#' Add Occurrence Counts to Grid
#'
#' @param combinations_df Data frame with temperature-SH combinations
#' @param data_df Original data frame with observations
#' @return Data frame with occurrence counts added
#' @export
add_occurrence_counts <- function(combinations_df, data_df) {
  combinations_df$Occurrences <- 0
  
  for(i in seq_len(nrow(combinations_df))) {
    current_temp <- combinations_df$Temperatura[i]
    current_sh <- combinations_df$SH[i]
    
    count <- sum(
      data_df$Temperatura >= (current_temp - 0.5) & 
        data_df$Temperatura <= (current_temp + 0.5) & 
        data_df$SH >= (current_sh - 0.5) & 
        data_df$SH <= (current_sh + 0.5),
      na.rm = TRUE
    )
    combinations_df$Occurrences[i] <- count
  }
  
  combinations_df <- combinations_df[combinations_df$Occurrences != 0, ]
  combinations_df$Temperatura <- combinations_df$Temperatura
  combinations_df$SH <- combinations_df$SH - 0.5
  
  return(combinations_df)
}

#' Create RH 100% Mask Polygon
#'
#' Creates a polygon that covers the area above the 100% relative humidity line
#' for masking purposes.
#'
#' @param x_limits Limits for x-axis (temperature), default: c(-10, 50)
#' @param y_limits Limits for y-axis (SH), default: c(0, 50)
#' @return Data frame with polygon coordinates
#' @export
create_rh100_mask <- function(x_limits = c(-10, 50), y_limits = c(0, 50)) {
  rh_100_line <- create_line_data(100)
  
  mask_poly <- data.frame(
    x = c(x_limits[1], rh_100_line$x, x_limits[2], x_limits[2], x_limits[1]),
    y = c(y_limits[2], rh_100_line$y, y_limits[2], y_limits[2], y_limits[2])
  )
  
  mask_poly <- mask_poly[complete.cases(mask_poly), ]
  return(mask_poly)
}

#' Create Grid Lines for Psychrometric Chart
#'
#' Creates vertical and horizontal grid lines for the psychrometric chart
#'
#' @return List containing vertical and horizontal grid line data
#' @export
maskgrid <- function() {
  temp_breaks <- seq(-10, 50, by = 10)
  vertical_lines <- data.frame(
    x = temp_breaks,
    xend = temp_breaks,
    y = 0,
    yend = 50
  )
  
  sh_breaks <- seq(0, 50, by = 10)
  horizontal_lines <- data.frame(
    x = -10,
    xend = 50,
    y = sh_breaks,
    yend = sh_breaks
  )
  
  return(list(vertical = vertical_lines, horizontal = horizontal_lines))
}