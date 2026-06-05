library(raster)
library(spdep)
library(terra)  
library(sf)
library(ggplot2)

wy <- raster("D:/Aban_1_new/data/Water_Yield_2024.tif")
fs <- raster("D:/Aban_1_new/data/Food_Supply_2024.tif")
sr <- raster("D:/Aban_1_new/data/Soil_Retention_2024.tif")
cs <- raster("D:/Aban_1_new/data/Carbon_Storage_2024.tif")
hq <- raster("D:/Aban_1_new/data/HABITAT_QUALITY_2024.tif")


template <- wy 
fs_resampled <- resample(fs, template, method = 'bilinear')
sr_resampled <- resample(sr, template, method = 'bilinear')
cs_resampled <- resample(cs, template, method = 'bilinear')
hq_resampled <- resample(hq, template, method = 'bilinear')


df <- data.frame(
  WY = values(wy),
  FS = values(fs_resampled),
  SR = values(sr_resampled),
  CS = values(cs_resampled),
  HQ = values(hq_resampled),
  cell_id = 1:ncell(wy)
)


df_clean <- na.omit(df)

coords <- xyFromCell(template, which(!is.na(values(template))))
nb <- knn2nb(knearneigh(coords, k = 8))  # 8 همسایه برای Queen contiguity
w <- nb2listw(nb)

# library(spdep)
# coords <- coordinates(template)[!is.na(values(template)), ]
# nb <- dnearneigh(coords, 0, res(template)[1] * 1.5)  
# w <- nb2listw(nb)

# 7. Bivariate LISA
library(spdep)

calculate_bivariate_lisa_safe <- function(var1_name, var2_name, df, w, nsim = 999) {
  
  var1 <- df[[var1_name]]
  var2 <- df[[var2_name]]
  
  var1_z <- scale(var1)
  var2_z <- scale(var2)
  
  var1_lag <- lag.listw(w, var1_z)
  var2_lag <- lag.listw(w, var2_z)
  
  n <- length(var1_z)
  Ii <- (var1_z * var2_lag + var2_z * var1_lag) / 2
  
  Ii_sim <- numeric(nsim)
  
  for(i in 1:nsim) {

    var2_perm <- sample(var2_z)
    var2_lag_perm <- lag.listw(w, var2_perm)
    Ii_sim[i] <- mean(var1_z * var2_lag_perm + var2_perm * var1_lag) / 2
  }
  
  p_value <- sapply(Ii, function(x) {
    sum(abs(Ii_sim) >= abs(x)) / nsim
  })
  
  clusters <- rep(5, length(var1))  # 5 = Not Significant
  
  # High-High
  clusters[Ii > 0 & var1 > median(var1) & var2 > median(var2) & p_value < 0.05] <- 1
  
  # Low-Low
  clusters[Ii > 0 & var1 <= median(var1) & var2 <= median(var2) & p_value < 0.05] <- 2
  
  # Low-High
  clusters[Ii < 0 & var1 <= median(var1) & var2 > median(var2) & p_value < 0.05] <- 3
  
  # High-Low
  clusters[Ii < 0 & var1 > median(var1) & var2 <= median(var2) & p_value < 0.05] <- 4
  
  return(list(clusters = clusters, Ii = Ii, p_value = p_value))
}

result <- calculate_bivariate_lisa_safe("CS", "HQ", df_clean, w, nsim = 999)
clusters_cs_hq <- result$clusters

result_raster <- template
values(result_raster)[!is.na(values(template))] <- clusters_cs_hq

cols <- c("red", "blue", "green", "purple", "gray")
names(cols) <- c("High-High", "Low-Low", "Low-High", "High-Low", "Not Significant")

plot(result_raster, 
     col = cols,
     breaks = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5),
     main = "Bivariate LISA: CS-HQ (2024)",
     legend = FALSE)

legend("bottomright", 
       legend = names(cols),
       fill = cols,
       cex = 0.8)

**********************************************

png("D:/Aban_1_new/imgs/Bivariate_LISA_CS_HQ_2024.png", 
    width = 2000, 
    height = 1600, 
    res = 300,  # دقت 300 DPI
    bg = "white") 

par(mar = c(2, 2, 3, 2))

plot(result_raster, 
     col = cols,
     breaks = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5),
     main = "Bivariate LISA: CS-HQ (2024)",
     legend = FALSE,
     axes = FALSE
   )

legend("bottomright", 
       legend = names(cols),
       fill = cols,
       cex = 0.7,
       box.lty = 0) 

dev.off()  

******************************************
svg("D:/Aban_1_new/imgs/Bivariate_LISA_CS_HQ_2024.svg", 
    width = 10,height = 8)

par(mar = c(1, 1, 3, 1))

plot(result_raster, 
     col = cols,
     breaks = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5),
     main = "Bivariate LISA: CS-HQ (2024)",
     legend = FALSE,
     axes = FALSE)

legend("bottomright", 
       legend = names(cols),
       fill = cols,
       cex = 1,
       bty = "n",
       box.lty = 0,
       title = "Cluster Type")

dev.off()