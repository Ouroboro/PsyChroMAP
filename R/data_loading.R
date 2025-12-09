#' Data Loading Function
#'
#' Loads climate data from various file formats and returns a standardized data frame.
#' Supported formats: RDS, CSV, TXT, NetCDF
#'
#' @param file_path Path to the data file (optional if year is provided)
#' @param year Specific year to filter data (optional)
#' @return Standardized data frame with columns: Temperature, Humidity, SH, DateTime (if available), Year (if available)
#' @export
load_data <- function(file_path = NULL, year = NULL) {
  if (is.null(file_path) && is.null(year)) {
    stop("You must specify either file_path or year")
  }
  
  # Search for files if year is specified
  if (!is.null(year)) {
    possible_files <- c(
      sprintf("data_%d.rds", year),
      sprintf("data_%d.csv", year),
      sprintf("data_%d.txt", year),
      sprintf("data_%d.nc", year),
      "combined_data.rds",
      "combined_data.csv", 
      "combined_data.txt",
      "combined_data.nc"
    )
    
    for (file in possible_files) {
      if (file.exists(file)) {
        file_path <- file
        message(sprintf("Found file: %s", file_path))
        break
      }
    }
    
    if (is.null(file_path) || !file.exists(file_path)) {
      stop(sprintf("No file found for year %d", year))
    }
  }
  
  file_ext <- tolower(tools::file_ext(file_path))
  
  # Load data based on format
  data_df <- switch(file_ext,
                    "rds" = readRDS(file_path),
                    "csv" = read.csv(file_path),
                    "txt" = read.table(file_path, header = TRUE, sep = "\t"),
                    "nc" = read_netcdf(file_path),
                    stop(sprintf("Unsupported file format: %s", file_ext))
  )
  
  # Convert DateTime to POSIXct if it exists and is character
  if ("DateTime" %in% colnames(data_df)) {
    if (is.character(data_df$DateTime)) {
      # Try multiple common datetime formats
      data_df$DateTime <- parse_datetime_column(data_df$DateTime)
    }
  }
  
  # Verify and calculate required columns
  data_df <- validate_and_calculate_columns(data_df)
  
  # Filter by year if specified
  if (!is.null(year)) {
    data_df <- filter_by_year(data_df, year)
  }
  
  data_df <- na.omit(data_df)
  message(sprintf("Loaded %d records from file: %s", nrow(data_df), file_path))
  
  return(data_df)
}

#' Parse DateTime Column
#'
#' Internal function to parse datetime columns with multiple possible formats
#' @param datetime_vector Vector of datetime strings
#' @return POSIXct vector
#' @keywords internal
parse_datetime_column <- function(datetime_vector) {
  # Try different datetime formats
  formats <- c(
    "%Y-%m-%d %H:%M:%S",
    "%Y/%m/%d %H:%M:%S",
    "%d-%m-%Y %H:%M:%S",
    "%d/%m/%Y %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M"
  )
  
  for (fmt in formats) {
    parsed <- tryCatch(
      as.POSIXct(datetime_vector, format = fmt, tz = "UTC"),
      error = function(e) NULL,
      warning = function(w) NULL
    )
    
    if (!is.null(parsed) && all(!is.na(parsed))) {
      message(sprintf("Successfully parsed DateTime with format: %s", fmt))
      return(parsed)
    }
  }
  
  # If no format works, try automatic parsing
  message("Using automatic datetime parsing")
  return(as.POSIXct(datetime_vector, tz = "UTC"))
}

#' Read NetCDF File
#'
#' Internal function to read NetCDF files
#' @param file_path Path to NetCDF file
#' @return Data frame with climate data
#' @keywords internal
read_netcdf <- function(file_path) {
  nc_data <- ncdf4::nc_open(file_path)
  on.exit(ncdf4::nc_close(nc_data))
  
  time_var <- try_get_nc_var(nc_data, "time")
  temp_data <- try_get_nc_var(nc_data, c("Temperature", "Temperatura"))
  hum_data <- try_get_nc_var(nc_data, c("Humidity", "Umidità"))
  sh_data <- try_get_nc_var(nc_data, c("Specific_Humidity", "SH"))
  
  # Calculate SH if missing
  if (is.null(sh_data) && !is.null(temp_data) && !is.null(hum_data)) {
    sh_data <- sh(hum_data, temp_data)
  }
  
  if (is.null(temp_data) || is.null(hum_data) || is.null(sh_data)) {
    stop("NetCDF file does not contain required variables")
  }
  
  data_df <- data.frame(
    Temperatura = temp_data,
    Umidità = hum_data,
    SH = sh_data
  )
  
  # Add DateTime if available
  if (!is.null(time_var)) {
    data_df$DateTime <- as.POSIXct(time_var * 3600, origin = "1970-01-01", tz = "UTC")
  }
  
  return(data_df)
}

#' Try to Get NetCDF Variable
#'
#' Internal helper function to safely extract variables from NetCDF files
#' @param nc_data NetCDF file object
#' @param var_names Vector of possible variable names
#' @return Variable data or NULL if not found
#' @keywords internal
try_get_nc_var <- function(nc_data, var_names) {
  for (var_name in var_names) {
    tryCatch({
      return(ncdf4::ncvar_get(nc_data, var_name))
    }, error = function(e) NULL)
  }
  return(NULL)
}

#' Validate and Calculate Required Columns
#'
#' Internal function to ensure required columns are present
#' @param data_df Data frame to validate
#' @return Validated data frame with required columns
#' @keywords internal
validate_and_calculate_columns <- function(data_df) {
  required_cols <- c("Temperatura", "Umidità", "SH")
  missing_cols <- setdiff(required_cols, colnames(data_df))
  
  if (length(missing_cols) > 0) {
    if ("SH" %in% missing_cols && all(c("Temperatura", "Umidità") %in% colnames(data_df))) {
      data_df$SH <- sh(data_df$Umidità, data_df$Temperatura)
      message("SH column calculated from Temperature and Humidity")
    } else {
      stop(sprintf("Missing columns in file: %s", paste(missing_cols, collapse = ", ")))
    }
  }
  
  return(data_df)
}

#' Filter Data by Year
#'
#' Internal function to filter data by year
#' @param data_df Data frame to filter
#' @param year Year to filter by
#' @return Filtered data frame
#' @keywords internal
filter_by_year <- function(data_df, year) {
  if ("Year" %in% colnames(data_df)) {
    data_df <- data_df[data_df$Year == year, ]
  } else if ("DateTime" %in% colnames(data_df)) {
    data_df <- data_df[lubridate::year(data_df$DateTime) == year, ]
  }
  return(data_df)
}
