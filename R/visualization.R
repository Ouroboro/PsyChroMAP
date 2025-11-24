#' Main Psychrometric Plot Function
#'
#' Creates static or animated psychrometric charts from climate data
#'
#' @param data_df Data frame containing climate data
#' @param title Plot title
#' @param color_palette Color palette to use ("personalized_colorscale" or other)
#' @param animate Whether to create animation (TRUE) or static plot (FALSE)
#' @param interval Time interval for animation ("YEAR" or "MONTH")
#' @param date_range Optional date range as character vector c(start_date, end_date)
#' @param output_file Output file name for animation (optional)
#' @param color_limit Maximum value for color scale (if NULL, calculated automatically)
#' @param quantiles Whether to use quantile-based coloring (TRUE) or occurrence-based (FALSE)
#' @param qvec Quantile values for coloring (default: c(25, 50, 75, 100))
#' @param name Base name for output files
#' @return ggplot object (if animate=FALSE) or creates animation file (if animate=TRUE)
#' @export
psyplot <- function(data_df, title = "Temperature vs SH by Occurrences", 
                    name = "Filename.png",
                    color_palette = NULL, 
                    animate = FALSE, interval = "YEAR", 
                    date_range = NULL, output_file = NULL, color_limit = NULL,
                    quantiles = TRUE, qvec = c(25, 50, 75, 100)) {
  
  validate_inputs(interval, data_df, animate)
  data_df <- filter_by_date_range(data_df, date_range)
  
  if (animate) {
    return(create_animation(data_df, title, color_palette, interval, output_file, 
                            color_limit, quantiles, qvec, name))
  } else {
    return(create_static_plot(data_df, title, color_palette, color_limit, 
                              quantiles, qvec, name))
  }
}

#' Validate Input Parameters
#'
#' Internal function to validate input parameters
#' @param interval Time interval
#' @param data_df Data frame
#' @param animate Whether animation is requested
#' @keywords internal
validate_inputs <- function(interval, data_df, animate) {
  if (!interval %in% c("YEAR", "MONTH")) {
    stop("interval must be 'YEAR' or 'MONTH'")
  }
  
  if (animate && !any(c("DateTime", "Year") %in% colnames(data_df))) {
    stop("Data frame must contain DateTime or Year column for animation")
  }
}

#' Filter Data by Date Range
#'
#' Internal function to filter data by date range
#' @param data_df Data frame
#' @param date_range Date range vector
#' @return Filtered data frame
#' @keywords internal
filter_by_date_range <- function(data_df, date_range) {
  if (is.null(date_range)) return(data_df)
  
  if ("DateTime" %in% colnames(data_df)) {
    start_date <- as.Date(date_range[1])
    end_date <- as.Date(date_range[2])
    data_df <- data_df[data_df$DateTime >= start_date & data_df$DateTime <= end_date, ]
  } else if ("Year" %in% colnames(data_df)) {
    start_year <- as.numeric(date_range[1])
    end_year <- as.numeric(date_range[2])
    data_df <- data_df[data_df$Year >= start_year & data_df$Year <= end_year, ]
  }
  
  return(data_df)
}

#' Create Static Psychrometric Plot
#'
#' @param data_df Data frame with climate data
#' @param title Plot title
#' @param color_palette Color palette name
#' @param color_limit Maximum value for color scale
#' @param quantiles Whether to use quantile-based coloring
#' @param qvec Quantile values for coloring
#' @param name Base name for output file
#' @return ggplot object
#' @keywords internal
create_static_plot <- function(data_df, title, color_palette, color_limit = NULL, 
                               quantiles, qvec, name) {
  
  combinations_df <- dataframeplot()
  combinations_df <- add_occurrence_counts(combinations_df, data_df)
  
  if (quantiles) {
    combinations_df <- calculate_quantiles(combinations_df, qvec)
  }
  
  color_column <- if(quantiles) "Quantile" else "Occurrences"
  
  if (is.null(color_limit) && !quantiles) {
    max_occurrences <- max(combinations_df$Occurrences, na.rm = TRUE)
    color_limit <- roundmax(max_occurrences)
  }
  
  plot <- create_base_plot(combinations_df, color_column, title)
  
  # Add appropriate color scale
  if (quantiles) {
    plot <- plot + add_quantile_scale(color_palette, length(qvec))
  } else {
    plot <- plot + add_occurrence_scale(color_palette, color_limit)
  }
  
  plot <- add_psychrometric_lines(plot)
  plot <- apply_plot_theme(plot)
  
  output_name <- sprintf("%s.png", name)
  ggsave(output_name, plot, width = 50, height = 30, units = "cm", 
         dpi = 600, bg = "white")
  
  return(plot)
}

#' Calculate Quantiles for Data
#'
#' Internal function to calculate quantiles for coloring
#' @param combinations_df Data frame with occurrence counts
#' @param qvec Quantile values
#' @return Data frame with quantile information
#' @keywords internal
calculate_quantiles <- function(combinations_df, qvec) {
  combinations_df <- combinations_df[order(combinations_df$Occurrences, decreasing = TRUE), ]
  total_occurrences <- sum(combinations_df$Occurrences)
  combinations_df$Cumulative <- cumsum(combinations_df$Occurrences) / total_occurrences * 100
  
  intervals <- findInterval(combinations_df$Cumulative, qvec, rightmost.closed = TRUE)
  quantile_labels <- paste0(qvec, "%")
  
  combinations_df$Quantile <- factor(
    quantile_labels[intervals + 1],
    levels = quantile_labels,
    ordered = TRUE
  )
  
  return(combinations_df)
}

#' Create Base Plot
#'
#' Internal function to create the base ggplot
#' @param combinations_df Data frame with plot data
#' @param color_column Column to use for coloring
#' @param title Plot title
#' @return ggplot object
#' @keywords internal
create_base_plot <- function(combinations_df, color_column, title) {
  ggplot(combinations_df, aes(x = Temperatura, y = SH)) +
    geom_point(aes_string(color = color_column), size = 4, stroke = 1.1, shape = 19) +
    theme_bw() +
    labs(
      title = title,
      x = "Temperature (°C)", 
      y = "Specific Humidity (g/kg)"
    )
}

#' Add Quantile Color Scale
#'
#' Internal function to add quantile-based color scale
#' @param color_palette Color palette name
#' @param n Number of colors needed
#' @return ggplot scale object
#' @keywords internal
add_quantile_scale <- function(color_palette, n) {
  scale_color_manual(
    values = get_quantile_palette(color_palette, n),
    name = "Quantile"
  )
}

#' Add Occurrence Color Scale
#'
#' Internal function to add occurrence-based color scale
#' @param color_palette Color palette name
#' @param color_limit Maximum value for color scale
#' @return ggplot scale object
#' @keywords internal
add_occurrence_scale <- function(color_palette, color_limit) {
  scale_color_gradientn(
    colors = get_color_palette(color_palette),
    limits = c(0, color_limit),
    na.value = NA,
    name = "Occurrences"
  )
}

#' Create Animated Psychrometric Plot
#'
#' @param data_df Data frame with climate data
#' @param title Plot title
#' @param color_palette Color palette name
#' @param interval Time interval ("YEAR" or "MONTH")
#' @param output_file Output file name
#' @param color_limit Maximum value for color scale
#' @param quantiles Whether to use quantile-based coloring
#' @param qvec Quantile values for coloring
#' @param name Base name for output files
#' @return Creates animation file
#' @keywords internal
create_animation <- function(data_df, title, color_palette, interval, output_file, 
                             color_limit = NULL, quantiles, qvec, name) {
  
  if (is.null(output_file)) {
    output_file <- sprintf("%s_%s.gif", name, interval)
  }
  
  time_info <- extract_time_info(data_df, interval)
  time_labels <- time_info$labels
  
  if (is.null(color_limit) && !quantiles) {
    color_limit <- calculate_global_color_limit(data_df, interval, time_labels)
    message(sprintf("Using global color limit: %d", color_limit))
  }
  
  temp_files <- create_animation_frames(data_df, title, color_palette, color_limit, 
                                        quantiles, qvec, name, interval, time_labels)
  
  create_gif_animation(temp_files, output_file)
  cleanup_temp_files(temp_files, name)
  
  message(sprintf("Animation created: %s", output_file))
  return(invisible(output_file))
}

#' Extract Time Information
#'
#' Internal function to extract time information for animation
#' @param data_df Data frame
#' @param interval Time interval
#' @return List with time units and labels
#' @keywords internal
extract_time_info <- function(data_df, interval) {
  if ("DateTime" %in% colnames(data_df)) {
    # Ensure DateTime is POSIXct
    if (!inherits(data_df$DateTime, "POSIXct")) {
      data_df$DateTime <- as.POSIXct(data_df$DateTime, tz = "UTC")
    }
    
    if (interval == "YEAR") {
      time_units <- lubridate::year(data_df$DateTime)
    } else {
      time_units <- format(data_df$DateTime, "%Y-%m")
    }
  } else if ("Year" %in% colnames(data_df)) {
    if (interval == "YEAR") {
      time_units <- data_df$Year
    } else {
      stop("Month interval requires DateTime column")
    }
  } else {
    stop("Data frame must contain DateTime or Year column for animation")
  }
  
  return(list(units = time_units, labels = unique(time_units)))
}

#' Filter Data by Time
#'
#' Internal function to filter data by time interval
#' @param data_df Data frame
#' @param interval Time interval
#' @param time_label Current time label
#' @return Filtered data frame
#' @keywords internal
filter_data_by_time <- function(data_df, interval, time_label) {
  if (interval == "YEAR") {
    if ("DateTime" %in% colnames(data_df)) {
      return(data_df[lubridate::year(data_df$DateTime) == as.numeric(time_label), ])
    } else {
      return(data_df[data_df$Year == as.numeric(time_label), ])
    }
  } else {
    # For MONTH interval, ensure DateTime is POSIXct
    if (!inherits(data_df$DateTime, "POSIXct")) {
      data_df$DateTime <- as.POSIXct(data_df$DateTime, tz = "UTC")
    }
    return(data_df[format(data_df$DateTime, "%Y-%m") == time_label, ])
  }
}

#' Calculate Global Color Limit
#'
#' Internal function to calculate global color limit for animation
#' @param data_df Data frame
#' @param interval Time interval
#' @param time_labels Time labels
#' @return Global color limit
#' @keywords internal
calculate_global_color_limit <- function(data_df, interval, time_labels) {
  global_combinations <- dataframeplot()
  global_max_occurrences <- 0
  
  for (time_label in time_labels) {
    current_data <- filter_data_by_time(data_df, interval, time_label)
    
    if (nrow(current_data) > 0) {
      temp_combinations <- add_occurrence_counts(global_combinations, current_data)
      current_max <- max(temp_combinations$Occurrences, na.rm = TRUE)
      global_max_occurrences <- max(global_max_occurrences, current_max)
    }
  }
  
  return(roundmax(global_max_occurrences))
}

#' Create Animation Frames
#'
#' Internal function to create individual animation frames
#' @param data_df Data frame
#' @param title Plot title
#' @param color_palette Color palette
#' @param color_limit Color limit
#' @param quantiles Whether to use quantiles
#' @param qvec Quantile values
#' @param name Base name
#' @param interval Time interval
#' @param time_labels Time labels
#' @return Vector of temporary file names
#' @keywords internal
create_animation_frames <- function(data_df, title, color_palette, color_limit, 
                                    quantiles, qvec, name, interval, time_labels) {
  temp_files <- c()
  
  for (time_label in time_labels) {
    current_data <- filter_data_by_time(data_df, interval, time_label)
    
    if (nrow(current_data) == 0) next
    
    plot_title <- create_plot_title(title, interval, time_label)
    current_plot <- create_static_plot(current_data, plot_title, color_palette, 
                                       color_limit, quantiles, qvec, name)
    
    temp_file <- sprintf("temp_plot_%s.png", time_label)
    ggsave(temp_file, current_plot, width = 50, height = 30, units = "cm", 
           dpi = 100, bg = "white")
    temp_files <- c(temp_files, temp_file)
    
    message(sprintf("Created plot for: %s", time_label))
  }
  
  if (length(temp_files) == 0) {
    stop("No frames created. Check your data and interval settings.")
  }
  
  return(temp_files)
}

#' Create Plot Title
#'
#' Internal function to create plot title for animation frames
#' @param base_title Base title
#' @param interval Time interval
#' @param time_label Current time label
#' @return Formatted plot title
#' @keywords internal
create_plot_title <- function(base_title, interval, time_label) {
  if (interval == "YEAR") {
    return(paste(base_title, "- Year", time_label))
  } else {
    return(paste(base_title, "-", format(as.Date(paste0(time_label, "-01")), "%B %Y")))
  }
}

#' Create GIF Animation
#'
#' Internal function to create GIF from frames
#' @param temp_files Temporary frame files
#' @param output_file Output file name
#' @keywords internal
create_gif_animation <- function(temp_files, output_file) {
  message("Creating animated GIF...")
  gifski::gifski(temp_files, output_file, width = 1200, height = 800, delay = 1)
}

#' Clean Up Temporary Files
#'
#' Internal function to clean up temporary files
#' @param temp_files Temporary files to remove
#' @param name Base name used for static plot
#' @keywords internal
cleanup_temp_files <- function(temp_files, name) {
  file.remove(temp_files)
  file.remove(sprintf("%s.png", name))
}

#' Round Maximum Value for Color Scale
#'
#' @param x Maximum value to round
#' @return Rounded value for color scale
#' @keywords internal
roundmax <- function(x) {
  breaks <- c(10, 50, 100, 200, 300, 500)
  for (breakpoint in breaks) {
    if (x <= breakpoint) return(breakpoint)
  }
  return(ceiling(x / 100) * 100)
}

#' Get Color Palette
#'
#' @param color_palette Name of color palette or custom color vector
#' @return Color vector
#' @keywords internal
get_color_palette <- function(color_palette = NULL) {
  if (is.null(color_palette)) {
    return(c("blue4", "blue", "cyan", "green", "yellow", "red", "red3"))
  }
  
  if (is.character(color_palette) && length(color_palette) > 1) {
    return(color_palette)
  }
  
  if (is.character(color_palette) && length(color_palette) == 1) {
    return(switch(color_palette,
                  "personalized_colorscale" = c("blue4", "blue", "cyan", "green", "yellow", "red", "red3"),
                  "default" = c("blue", "cyan", "green", "yellow", "red"),
                  get_predefined_palette(color_palette)
    ))
  }
  
  return(c("blue", "cyan", "green", "yellow", "red"))
}

#' Get Predefined Color Palette
#'
#' Internal function to get predefined color palettes
#' @param palette_name Palette name
#' @return Color vector
#' @keywords internal
get_predefined_palette <- function(palette_name) {
  if (palette_name %in% c("viridis", "plasma", "inferno", "magma", "cividis")) {
    return(viridisLite::viridis(7, option = palette_name))
  }
  
  if (palette_name %in% rownames(RColorBrewer::brewer.pal.info)) {
    max_colors <- RColorBrewer::brewer.pal.info[palette_name, "maxcolors"]
    return(RColorBrewer::brewer.pal(max_colors, palette_name))
  }
  
  warning("Palette '", palette_name, "' not found. Using default palette.")
  return(c("blue", "cyan", "green", "yellow", "red"))
}

#' Add Psychrometric Chart Lines
#'
#' @param plot ggplot object
#' @return ggplot object with lines added
#' @keywords internal
add_psychrometric_lines <- function(plot) {
  # Add RH 100% mask
  rh100_mask <- create_rh100_mask()
  plot <- plot +
    geom_polygon(
      data = rh100_mask,
      aes(x = x, y = y),
      fill = "white",
      color = NA,
      alpha = 1
    )
  
  # Add grid lines
  grid_data <- maskgrid()
  plot <- plot +
    geom_segment(
      data = grid_data$vertical,
      aes(x = x, xend = xend, y = y, yend = yend),
      color = "grey90",
      linewidth = 0.2,
      alpha = 0.7
    ) +
    geom_segment(
      data = grid_data$horizontal,
      aes(x = x, xend = xend, y = y, yend = yend),
      color = "grey90",
      linewidth = 0.2,
      alpha = 0.7
    )
  
  # Add relative humidity lines
  Hr_values <- seq(10, 100, by = 10)
  for (Hr in Hr_values) {
    line_data <- create_line_data(Hr)
    plot <- plot + 
      geom_line(data = line_data, aes(x = x, y = y), color = "grey82", linewidth = 0.2)
  }
  
  # Add enthalpy lines and labels
  Hp_values <- seq(0, 180, by = 10)
  for (h in Hp_values) {
    line_data <- create_line_data2(h)
    plot <- plot + 
      geom_line(data = line_data, aes(x = x, y = y), linetype = "dotted", 
                color = "grey82", linewidth = 0.4)
    
    plot <- add_enthalpy_label(plot, line_data, h)
  }
  
  # Add RH labels
  plot <- plot +
    annotate("text", x = 48.5, y = 7.7, label = "10% RH", size = 3, color = "gray64", angle = 23) +
    annotate("text", x = 48.5, y = 42.4, label = "50% RH", size = 3, color = "gray64", angle = 62) +
    annotate("text", x = 38.5, y = 46.5, label = "100% RH", size = 3, color = "gray64", angle = 63)
  
  return(plot)
}

#' Add Enthalpy Label
#'
#' Internal function to add enthalpy labels to plot
#' @param plot ggplot object
#' @param line_data Line data for enthalpy
#' @param h Enthalpy value
#' @return ggplot object with label added
#' @keywords internal
add_enthalpy_label <- function(plot, line_data, h) {
  i <- 1006 * h / 1000
  i_label <- sprintf("%.2f", i)
  
  max_y_point <- line_data[which.max(line_data$y), ]
  
  if (nrow(max_y_point) > 0) {
    plot <- plot +
      geom_text(
        data = max_y_point,
        aes(x = x - 1.75, y = y + 0.75, angle = -15),
        label = paste(i_label, "(kJ/kg)"),
        vjust = -0.5,
        color = "gray24",
        size = 3.5
      )
  }
  
  return(plot)
}

#' Apply Plot Theme
#'
#' @param plot ggplot object
#' @return ggplot object with theme applied
#' @keywords internal
apply_plot_theme <- function(plot) {
  plot +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 20),
      legend.position = "right",
      legend.key.height = unit(2, "cm"),
      legend.key.width = unit(1, "cm"),
      legend.title = element_text(size = 14, angle = 90, hjust = 0.5),
      legend.text = element_text(size = 14), 
      legend.title.position = "right",
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text.x = element_text(size = 14, margin = margin(t = 8)),
      axis.text.y = element_text(size = 14, margin = margin(r = 8)),
      panel.border = element_rect(fill = NA, color = "black", size = 1),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    coord_equal(ratio = 6/10, xlim = c(-10, 50), ylim = c(0, 50), 
                expand = FALSE, clip = "on") +
    guides(size = "none")
}

#' Get Color Palette for Quantiles
#'
#' @param palette_name Name of color palette or custom color vector
#' @param n Number of colors needed (based on quantiles)
#' @return Color vector for quantiles of appropriate length
#' @keywords internal
get_quantile_palette <- function(palette_name = NULL, n = 4) {
  if (is.null(palette_name)) {
    return(colorRampPalette(c("blue", "cyan", "green", "yellow", "red"))(n))
  }
  
  if (is.character(palette_name) && length(palette_name) > 1) {
    if (length(palette_name) == n) {
      return(palette_name)
    }
    return(colorRampPalette(palette_name)(n))
  }
  
  if (is.character(palette_name) && length(palette_name) == 1) {
    return(switch(palette_name,
                  "personalized_colorscale" = colorRampPalette(c("#4575b4", "#74add1", "#fdae61", "#d73027"))(n),
                  "default" = colorRampPalette(c("blue", "cyan", "green", "yellow", "red"))(n),
                  get_predefined_quantile_palette(palette_name, n)
    ))
  }
  
  return(colorRampPalette(c("blue", "cyan", "green", "yellow", "red"))(n))
}

#' Get Predefined Quantile Palette
#'
#' Internal function to get predefined palettes for quantiles
#' @param palette_name Palette name
#' @param n Number of colors
#' @return Color vector
#' @keywords internal
get_predefined_quantile_palette <- function(palette_name, n) {
  if (palette_name %in% c("viridis", "plasma", "inferno", "magma", "cividis")) {
    return(viridisLite::viridis(n, option = palette_name))
  }
  
  if (palette_name %in% rownames(RColorBrewer::brewer.pal.info)) {
    max_colors <- RColorBrewer::brewer.pal.info[palette_name, "maxcolors"]
    if (n <= max_colors) {
      return(RColorBrewer::brewer.pal(n, palette_name))
    }
    base_pal <- RColorBrewer::brewer.pal(min(n, max_colors), palette_name)
    return(colorRampPalette(base_pal)(n))
  }
  
  warning("Palette '", palette_name, "' not found. Using default quantile palette.")
  return(colorRampPalette(c("blue", "cyan", "green", "yellow", "red"))(n))
}