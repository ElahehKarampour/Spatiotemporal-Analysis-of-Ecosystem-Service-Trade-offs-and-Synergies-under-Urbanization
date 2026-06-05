library(raster)
library(terra)
library(sf)

path <- "D:/Aban_1_new/data/"


ndvi_2000 <- raster(paste0(path, "NDVI_2000.tif"))
ndbi_2000 <- raster(paste0(path, "NDBI_2000.tif"))

ndvi_2006 <- raster(paste0(path, "NDVI_2006.tif"))
ndbi_2006 <- raster(paste0(path, "NDBI_2006.tif"))

ndvi_2012 <- raster(paste0(path, "NDVI_2012.tif"))
ndbi_2012 <- raster(paste0(path, "NDBI_2012.tif"))

ndvi_2018 <- raster(paste0(path, "NDVI_2018.tif"))
ndbi_2018 <- raster(paste0(path, "NDBI_2018.tif"))

ndvi_2024 <- raster(paste0(path, "NDVI_2024.tif"))
ndbi_2024 <- raster(paste0(path, "NDBI_2024.tif"))

pop_2000 <- raster(paste0(path, "population_landscan_2000.tif"))
pop_2000 <- setMinMax(pop_2000)

pop_2006 <- raster(paste0(path, "population_landscan_2006.tif"))
pop_2006 <- setMinMax(pop_2006)

pop_2012 <- raster(paste0(path, "population_landscan_2012.tif"))
pop_2012 <- setMinMax(pop_2012)

pop_2018 <- raster(paste0(path, "population_landscan_2018.tif"))
pop_2018 <- setMinMax(pop_2018)

pop_2024 <- raster(paste0(path, "population_landscan_2024.tif"))  
pop_2024 <- setMinMax(pop_2024)


template <- raster(paste0(path, "Water_Yield_2012.tif"))  


resample_year <- function(ndvi, ndbi, pop, template, year) {

  ndvi_res <- resample(ndvi, template, method = 'bilinear')
  ndbi_res <- resample(ndbi, template, method = 'bilinear')

  pop_res <- resample(pop, template, method = 'bilinear')
  

  sum_before <- cellStats(pop, sum, na.rm = TRUE)
  sum_after <- cellStats(pop_res, sum, na.rm = TRUE)
  cat("سال", year, "- نسبت حفظ جمعیت:", sum_after / sum_before, "\n")
  
  return(list(
    NDVI = ndvi_res,
    NDBI = ndbi_res,
    POP = pop_res,
    year = year
  ))
}


data_2000 <- resample_year(ndvi_2000, ndbi_2000, pop_2000, template, 2000)
data_2006 <- resample_year(ndvi_2006, ndbi_2006, pop_2006, template, 2006)
data_2012 <- resample_year(ndvi_2012, ndbi_2012, pop_2012, template, 2012)
data_2018 <- resample_year(ndvi_2018, ndbi_2018, pop_2018, template, 2018)
data_2024 <- resample_year(ndvi_2024, ndbi_2024, pop_2024, template, 2024)


calculate_urban_indicators <- function(data) {

  ndvi <- data$NDVI
  ndbi <- data$NDBI
  pop <- data$POP
  year <- data$year
  
  builtup_binary <- ndbi > 0  
  

  window_size <- 3
  pbl <- focal(builtup_binary * 100,  
               w = matrix(1, window_size, window_size),
               fun = mean,
               na.rm = TRUE,
               pad = TRUE)
  
  green_binary <- ndvi > 0.2

  template_utm <- projectRaster(template, crs = "+proj=utm +zone=48 +datum=WGS84", res = 30)

  cell_size_m <- res(template)[1] * 111320  
  cell_area_m2 <- cell_size_m^2
  

  green_area_m2 <- green_binary * cell_area_m2
  
  gspc <- green_area_m2 / pop
  
  gspc[is.infinite(gspc)] <- NA
  gspc[pop == 0 | is.na(pop)] <- 0 
  
  builtup_intensity <- ndbi  
  
  vegetation_intensity <- ndvi
  

  writeRaster(pbl,
              filename = paste0(path, "PBL_", year, ".tif"),
              format = "GTiff",
              overwrite = TRUE)
  
  writeRaster(gspc,
              filename = paste0(path, "GSPC_", year, ".tif"),
              format = "GTiff",
              overwrite = TRUE)
  
  writeRaster(builtup_binary,
              filename = paste0(path, "Builtup_Binary_", year, ".tif"),
              format = "GTiff",
              overwrite = TRUE)
  
  writeRaster(green_binary,
              filename = paste0(path, "Green_Binary_", year, ".tif"),
              format = "GTiff",
              overwrite = TRUE)
  
  return(list(
    year = year,
    PBL = pbl,
    GSPC = gspc,
    Builtup_Binary = builtup_binary,
    Green_Binary = green_binary,
    NDBI = ndbi,
    NDVI = ndvi
  ))
}

results_2000 <- calculate_urban_indicators(data_2000)
results_2006 <- calculate_urban_indicators(data_2006)
results_2012 <- calculate_urban_indicators(data_2012)
results_2018 <- calculate_urban_indicators(data_2018)
results_2024 <- calculate_urban_indicators(data_2024)


check_results <- function(results) {
  year <- results$year
  
  pbl_stats <- cellStats(results$PBL, summary)
  cat("\nPBL (درصد اراضی ساخته‌شده):\n")
  print(round(pbl_stats, 2))

  gspc_stats <- cellStats(results$GSPC, summary, na.rm = TRUE)
  cat("\nGSPC (متر مربع فضای سبز به ازای هر نفر):\n")
  print(round(gspc_stats, 2))
  

  builtup_percent <- cellStats(results$Builtup_Binary, mean, na.rm = TRUE) * 100
  cat("\nدرصد مناطق ساخته‌شده:", round(builtup_percent, 2), "%\n")

  green_percent <- cellStats(results$Green_Binary, mean, na.rm = TRUE) * 100
  cat("درصد فضای سبز:", round(green_percent, 2), "%\n")
}

check_results(results_2000)
check_results(results_2006)
check_results(results_2012)
check_results(results_2018)
check_results(results_2024)


library(ggplot2)
library(rasterVis)

plot_indicators <- function(results) {
  year <- results$year
  
  par(mfrow = c(2, 2))
  
  # PBL
  plot(results$PBL,
       main = paste("PBL - درصد اراضی ساخته‌شده (", year, ")"),
       col = terrain.colors(100),
       axes = FALSE,
       box = FALSE)
  
  # GSPC
  plot(results$GSPC,
       main = paste("GSPC - سرانه فضای سبز (", year, ") m²/نفر"),
       col = terrain.colors(100),
       axes = FALSE,
       box = FALSE)
  
  # Builtup Binary
  plot(results$Builtup_Binary,
       main = paste("اراضی ساخته‌شده (", year, ")"),
       col = c("gray", "red"),
       axes = FALSE,
       box = FALSE)
  legend("bottomright", legend = c("غیرساخته‌شده", "ساخته‌شده"), fill = c("gray", "red"))
  
  # Green Binary
  plot(results$Green_Binary,
       main = paste("فضای سبز (", year, ")"),
       col = c("gray", "green"),
       axes = FALSE,
       box = FALSE)
  legend("bottomright", legend = c("بدون پوشش", "فضای سبز"), fill = c("gray", "green"))
  
  par(mfrow = c(1, 1))
}


plot_indicators(results_2024)