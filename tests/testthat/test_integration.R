# tests/testthat/test_integration.R

test_that("Integration test works with all file formats", {
  # Usa i dati di esempio dal pacchetto
  nc_file <- system.file("extdata", "combined_data.nc", package = "nome_package")
  rds_file <- system.file("extdata", "combined_data.rds", package = "nome_package")
  csv_file <- system.file("extdata", "combined_data.csv", package = "nome_package")
  txt_file <- system.file("extdata", "combined_data.txt", package = "nome_package")
  
  # Test caricamento dati
  expect_silent(data_nc <- load_data(nc_file))
  expect_silent(data_rds <- load_data(rds_file))
  expect_silent(data_csv <- load_data(csv_file))
  expect_silent(data_txt <- load_data(txt_file))
  
  # Test plot statico
  static_plot <- psyplot(
    data_df = data_nc,
    title = "Temperature vs SH",
    color_palette = "personalized_colorscale",
    animate = FALSE,
    quantiles = TRUE,
    name = "Test000",
    interval = "MONTH",
    qvec = c(75, 90, 95, 100)
  )
  
  expect_s3_class(static_plot, "ggplot")
  
  # Test plot animato (potrebbe essere disabilitato in ambiente CI)
  if (interactive()) {
    gif_path <- psyplot(
      data_df = data_nc,
      title = "Temperature vs SH", 
      color_palette = "personalized_colorscale",
      animate = TRUE,
      interval = "YEAR",
      color_limit = 3000,
      name = "Test2"
    )
    
    expect_true(file.exists(gif_path))
  }
})