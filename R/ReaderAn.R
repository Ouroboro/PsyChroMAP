# Load necessary packages
library(ggplot2)
library(lubridate)
library(gifski)
library(data.table)
library(ncdf4)  # Aggiunto per la lettura dei file NetCDF

sh <- function(H, T, Pressure = 1013.25) {
  pv <- (11.7 ^ (8.07131 - 1730.63 / (T + 233.426))) * 1.1
  mr <- 622 * pv / (Pressure - pv)
  w <- mr * H / 100
  return(w)
}

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

create_line_data <- function(Hr) {
  Tl <- seq(-10, 50, by = 0.02)
  data.frame(
    x = Tl,
    y = sh(Hr, Tl)
  )
}

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

dataframeplot <- function(){
  temperature_seq <- seq(-9.5, 49.5, by = 0.5)
  result_list <- list()
  for(temp in temperature_seq) {
    if(temp %% 1 == 0.5) {
      sh_seq <- seq(0.5, 49.5, by = 1)
    } else {
      sh_seq <- seq(1, 50, by = 1)
    }
    temp_df <- data.frame(Temperatura = temp, SH = sh_seq)
    result_list[[length(result_list) + 1]] <- temp_df
  }
  final_combinations <- do.call(rbind, result_list)
  rownames(final_combinations) <- NULL
  return(final_combinations)
}

add_occurrence_counts <- function(combinations_df, data_df) {
  combinations_df$Occurrences <- 0
  for(i in 1:nrow(combinations_df)) {
    current_temp <- combinations_df$Temperatura[i]
    current_sh <- combinations_df$SH[i]
    temp_range <- c(current_temp - 0.5, current_temp + 0.5)
    sh_range <- c(current_sh - 0.5, current_sh + 0.5)
    count <- sum(
      data_df$Temperatura >= temp_range[1] & 
        data_df$Temperatura <= temp_range[2] & 
        data_df$SH >= sh_range[1] & 
        data_df$SH <= sh_range[2],
      na.rm = TRUE
    )
    combinations_df$Occurrences[i] <- count
  }
  combinations_df <- combinations_df[combinations_df$Occurrences != 0, ]
  combinations_df$Temperatura <- combinations_df$Temperatura
  combinations_df$SH <- combinations_df$SH - 0.5
  return(combinations_df)
}

# Funzione per caricare i dati da diversi formati
load_data <- function(file_path = NULL, year = NULL) {
  if (is.null(file_path) && is.null(year)) {
    stop("Devi specificare o file_path o year")
  }
  
  # Se viene specificato l'anno, cerca i file nella directory di lavoro
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
    
    # Cerca il primo file esistente
    for (file in possible_files) {
      if (file.exists(file)) {
        file_path <- file
        message(sprintf("Trovato file: %s", file_path))
        break
      }
    }
    
    if (is.null(file_path) || !file.exists(file_path)) {
      stop(sprintf("Nessun file trovato per l'anno %d", year))
    }
  }
  
  # Determina il tipo di file dall'estensione
  file_ext <- tolower(tools::file_ext(file_path))
  
  # Carica i dati in base al formato
  if (file_ext == "rds") {
    data_df <- readRDS(file_path)
  } else if (file_ext == "csv") {
    data_df <- read.csv(file_path)
  } else if (file_ext == "txt") {
    data_df <- read.table(file_path, header = TRUE, sep = "\t")
  } else if (file_ext == "nc") {
    # Lettura file NetCDF
    nc_data <- ncdf4::nc_open(file_path)
    
    # Legge le variabili (usa nomi standard)
    time_var <- tryCatch(
      ncdf4::ncvar_get(nc_data, "time"),
      error = function(e) NULL
    )
    
    # Prova diversi nomi possibili per le variabili
    temp_data <- tryCatch(
      ncdf4::ncvar_get(nc_data, "Temperature"),
      error = function(e) tryCatch(
        ncdf4::ncvar_get(nc_data, "Temperatura"),
        error = function(e) NULL
      )
    )
    
    hum_data <- tryCatch(
      ncdf4::ncvar_get(nc_data, "Humidity"),
      error = function(e) tryCatch(
        ncdf4::ncvar_get(nc_data, "Umidità"),
        error = function(e) NULL
      )
    )
    
    sh_data <- tryCatch(
      ncdf4::ncvar_get(nc_data, "Specific_Humidity"),
      error = function(e) tryCatch(
        ncdf4::ncvar_get(nc_data, "SH"),
        error = function(e) NULL
      )
    )
    
    ncdf4::nc_close(nc_data)
    
    # Se manca SH, la calcola
    if (is.null(sh_data) && !is.null(temp_data) && !is.null(hum_data)) {
      sh_data <- sh(hum_data, temp_data)
    }
    
    # Crea il data frame
    if (!is.null(temp_data) && !is.null(hum_data) && !is.null(sh_data)) {
      data_df <- data.frame(
        Temperatura = temp_data,
        Umidità = hum_data,
        SH = sh_data
      )
      
      # Aggiunge DateTime se disponibile
      if (!is.null(time_var)) {
        # Converti il tempo (assumendo ore dal 1970-01-01)
        data_df$DateTime <- as.POSIXct(time_var * 3600, origin = "1970-01-01", tz = "UTC")
      }
    } else {
      stop("File NetCDF non contiene le variabili richieste")
    }
  } else {
    stop(sprintf("Formato file non supportato: %s", file_ext))
  }
  
  # Verifica che le colonne necessarie siano presenti
  required_cols <- c("Temperatura", "Umidità", "SH")
  missing_cols <- setdiff(required_cols, colnames(data_df))
  
  if (length(missing_cols) > 0) {
    # Se manca SH ma abbiamo Temperatura e Umidità, calcola SH
    if ("SH" %in% missing_cols && all(c("Temperatura", "Umidità") %in% colnames(data_df))) {
      data_df$SH <- sh(data_df$Umidità, data_df$Temperatura)
      message("Colonna SH calcolata da Temperatura e Umidità")
    } else {
      stop(sprintf("Colonne mancanti nel file: %s", paste(missing_cols, collapse = ", ")))
    }
  }
  
  # Filtra i dati per l'anno specificato se necessario
  if (!is.null(year)) {
    if ("Year" %in% colnames(data_df)) {
      data_df <- data_df[data_df$Year == year, ]
    } else if ("DateTime" %in% colnames(data_df)) {
      data_df <- data_df[year(data_df$DateTime) == year, ]
    }
  }
  
  # Rimuove righe con NA
  data_df <- na.omit(data_df)
  
  message(sprintf("Caricati %d record dal file: %s", nrow(data_df), file_path))
  return(data_df)
}

# Funzione per creare il plot per un anno specifico
create_year_plot <- function(year) {
  # Leggi i dati per l'anno specificato
  data_df <- load_data(year = year)
  
  # Crea la griglia di combinazioni
  combinations_df <- dataframeplot()
  
  # Aggiungi i conteggi delle occorrenze
  combinations_df <- add_occurrence_counts(combinations_df, data_df)
  
  # Calcola i parametri stagionali
  params <- seasonal_parameters(data_df$Temperatura, data_df$Umidità)
  Tx <- params[1]
  Hx <- params[2]
  hx <- params[3]
  wx <- params[4]
  
  # Crea il plot principale
  plot <- ggplot(combinations_df, aes(x = Temperatura, y = SH)) +
    geom_point(aes(color = Occurrences, size = 1, stroke = 1), shape = 19) +
    scale_color_gradientn(
      colors = c("blue4","blue", "cyan","green", "yellow", "red","red3"),
      limits = c(0, 300),
      na.value = NA) +
    theme_bw() +
    labs(
      title = paste("Temperature vs SH by Occurrences - Year", year),
      x = "Temperature", 
      y = "SH Value"
    )
  
  # Values to use for drawing the psychrometric chart lines
  Hr_values <- seq(10, 100, by = 10)
  Hp_values <- seq(0, 180, by = 10)
  
  # Draw same Relative Humidity lines
  for (Hr in Hr_values) {
    line_data <- create_line_data(Hr)
    plot <- plot + 
      geom_line(data = line_data, aes(x = x, y = y), color = "grey82", linewidth = 0.2)
  }
  
  # Draw Isohentalpic lines
  for (h in Hp_values) {
    line_data <- create_line_data2(h)
    plot <- plot + 
      geom_line(data = line_data, aes(x = x, y = y), linetype = "dotted", color = "grey82", linewidth = 0.4)
    
    i <- 1006 * h / 1000
    i <- sprintf("%.2f", i)
    
    max_y_point <- line_data[which.max(line_data$y), ]
    
    plot <- plot +
      geom_text(
        data = max_y_point,
        aes(x = x - 1.75, y = y + 0.75, angle = -15),
        label = paste(i, "(kJ/kg)"),
        vjust = -0.5,
        color = "gray24",
        size = 3.5
      )
  }
  
  # Apply theme modifications and coordinate system
  plot <- plot +
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
    coord_equal(ratio = 6/10, xlim = c(-10, 50), ylim = c(0, 50), expand = FALSE, clip = "on") +
    guides(size = "none") +
    annotate("text", x = 48.5, y = 7.7, label = "10% RH", size = 3, color = "gray64", angle = 23) +
    annotate("text", x = 48.5, y = 42.4, label = "50% RH", size = 3, color = "gray64", angle = 62) +
    annotate("text", x = 38.5, y = 46.5, label = "100% RH", size = 3, color = "gray64", angle = 63)
  
  return(plot)
}

# Funzione per creare l'animazione
# Funzione per creare l'animazione
animate <- function(file_path = NULL, file_type = "auto", output_gif = "psychrometric_animation.gif") {
  
  # Se non viene fornito un file_path, cerca automaticamente i file disponibili
  if (is.null(file_path)) {
    possible_files <- c(
      "combined_data.rds", "combined_data.csv", "combined_data.txt", "combined_data.nc",
      "data_1998.rds", "data_1998.csv", "data_1998.txt", "data_1998.nc"  # esempio per il primo anno
    )
    
    for (file in possible_files) {
      if (file.exists(file)) {
        file_path <- file
        message(sprintf("Trovato file: %s", file_path))
        break
      }
    }
    
    if (is.null(file_path)) {
      stop("Nessun file dati trovato. Specifica il percorso del file.")
    }
  }
  
  # Carica tutti i dati per identificare gli anni disponibili
  message("Caricamento dati per identificare gli anni...")
  all_data <- load_data(file_path)
  
  # Identifica gli anni dai dati
  if ("Year" %in% colnames(all_data)) {
    years <- unique(all_data$Year)
  } else if ("DateTime" %in% colnames(all_data)) {
    years <- unique(lubridate::year(all_data$DateTime))
  } else {
    stop("Impossibile identificare gli anni dai dati. Assicurati che ci sia una colonna 'Year' o 'DateTime'.")
  }
  
  years <- sort(years)
  message(sprintf("Trovati anni: %s", paste(years, collapse = ", ")))
  
  temp_files <- c()
  
  for (year in years) {
    # Filtra i dati per l'anno corrente
    if ("Year" %in% colnames(all_data)) {
      year_data <- all_data[all_data$Year == year, ]
    } else {
      year_data <- all_data[lubridate::year(all_data$DateTime) == year, ]
    }
    
    # Verifica che ci siano dati per questo anno
    if (nrow(year_data) == 0) {
      warning(sprintf("Nessun dato trovato per l'anno %d", year))
      next
    }
    
    # Crea la griglia di combinazioni
    combinations_df <- dataframeplot()
    
    # Aggiungi i conteggi delle occorrenze
    combinations_df <- add_occurrence_counts(combinations_df, year_data)
    
    # Calcola i parametri stagionali
    params <- seasonal_parameters(year_data$Temperatura, year_data$Umidità)
    Tx <- params[1]
    Hx <- params[2]
    hx <- params[3]
    wx <- params[4]
    
    # Crea il plot principale
    plot <- ggplot(combinations_df, aes(x = Temperatura, y = SH)) +
      geom_point(aes(color = Occurrences, size = 1, stroke = 1), shape = 19) +
      scale_color_gradientn(
        colors = c("blue4","blue", "cyan","green", "yellow", "red","red3"),
        limits = c(0, 300),
        na.value = NA) +
      theme_bw() +
      labs(
        title = paste("Temperature vs SH by Occurrences - Year", year),
        x = "Temperature", 
        y = "SH Value"
      )
    
    # Values to use for drawing the psychrometric chart lines
    Hr_values <- seq(10, 100, by = 10)
    Hp_values <- seq(0, 180, by = 10)
    
    # Draw same Relative Humidity lines
    for (Hr in Hr_values) {
      line_data <- create_line_data(Hr)
      plot <- plot + 
        geom_line(data = line_data, aes(x = x, y = y), color = "grey82", linewidth = 0.2)
    }
    
    # Draw Isohentalpic lines
    for (h in Hp_values) {
      line_data <- create_line_data2(h)
      plot <- plot + 
        geom_line(data = line_data, aes(x = x, y = y), linetype = "dotted", color = "grey82", linewidth = 0.4)
      
      i <- 1006 * h / 1000
      i <- sprintf("%.2f", i)
      
      max_y_point <- line_data[which.max(line_data$y), ]
      
      plot <- plot +
        geom_text(
          data = max_y_point,
          aes(x = x - 1.75, y = y + 0.75, angle = -15),
          label = paste(i, "(kJ/kg)"),
          vjust = -0.5,
          color = "gray24",
          size = 3.5
        )
    }
    
    # Apply theme modifications and coordinate system
    plot <- plot +
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
      coord_equal(ratio = 6/10, xlim = c(-10, 50), ylim = c(0, 50), expand = FALSE, clip = "on") +
      guides(size = "none") +
      annotate("text", x = 48.5, y = 7.7, label = "10% RH", size = 3, color = "gray64", angle = 23) +
      annotate("text", x = 48.5, y = 42.4, label = "50% RH", size = 3, color = "gray64", angle = 62) +
      annotate("text", x = 38.5, y = 46.5, label = "100% RH", size = 3, color = "gray64", angle = 63)
    
    # Salva il plot temporaneo
    temp_file <- sprintf("temp_%d.png", year)
    ggsave(temp_file, plot, width = 50, height = 30, units = "cm", dpi = 600, bg = "white")
    temp_files <- c(temp_files, temp_file)
    
    message(sprintf("Creato plot per l'anno %d", year))
  }
  
  if (length(temp_files) == 0) {
    stop("Nessun frame creato. Verifica i dati.")
  }
  
  # Crea il GIF animato
  message("Creazione GIF animata...")
  gifski(temp_files, output_gif, width = 5000, height = 3000, delay = 1)
  
  # Rimuovi i file temporanei
  file.remove(temp_files)
  
  message(sprintf("Animazione GIF creata: %s", output_gif))
  message(sprintf("Totale frame creati: %d", length(temp_files)))
}

# Esempi di utilizzo:
# animate()  # Cerca automaticamente i file
# animate("combined_data.csv")  # Usa un file specifico
# animate("data_1998.nc")  # Usa un file NetCDF
animate("combined_data.rds", "psychrometric_chart.gif")  # Specifica il nome dell'output