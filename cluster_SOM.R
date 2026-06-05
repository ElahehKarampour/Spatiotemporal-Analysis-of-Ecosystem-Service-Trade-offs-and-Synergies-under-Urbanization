library(raster)


path <- "D:/Aban_1_new/data/"
years <- c(2000, 2006, 2012, 2018, 2024)
es_vars <- c("Water_Yield", "Food_Supply", "Soil_Retention", "Carbon_Storage", "HABITAT_QUALITY")

load_es_year <- function(year) {
  
  cat("   سال", year, ": ")
  
  df_list <- list()
  
  for(var in es_vars) {
    file_name <- paste0(path, var, "_", year, ".tif")
    r <- raster(file_name)
    df_list[[var]] <- values(r)
  }
  
  df <- as.data.frame(df_list)
  n_cells <- nrow(df)
  n_na <- sum(is.na(df))
  
  cat(format(n_cells, big.mark = ","), "cell,", 
      format(n_na, big.mark = ","), "NA\n")
  
  return(df)
}


all_data_list <- list()

for(year in years) {
  df <- load_es_year(year)
  all_data_list[[as.character(year)]] <- df
 }

all_data <- do.call(rbind, all_data_list)

cat(" combined data:", 
    format(nrow(all_data), big.mark = ","), 
    "cell ×", ncol(all_data), "variable\n")

na_rows <- rowSums(is.na(all_data)) > 0
n_na_rows <- sum(na_rows)

if(n_na_rows > 0) {
  all_data_clean <- all_data[!na_rows, ]
  cat(format(n_na_rows, big.mark = ","))
  cat( 
      format(nrow(all_data_clean), big.mark = ","), 
      "cell\n")
} else {
  all_data_clean <- all_data
  cat( 
      format(nrow(all_data_clean), big.mark = ","), 
      "cell\n")
}

all_data_scaled <- scale(all_data_clean)

cat("\n", paste(rep("=", 50), collapse = ""), "\n")

cat(paste(rep("=", 50), collapse = ""), "\n")
cat("تعداد سال‌ها:", length(years), "\n")
cat(" تعداد متغیرها:", ncol(all_data_clean), "\n")
cat(" تعداد سلول‌ها:", format(nrow(all_data_clean), big.mark = ","), "\n")
cat(" محدوده مقادیر:", 
    round(min(all_data_clean, na.rm = TRUE), 2),  
    round(max(all_data_clean, na.rm = TRUE), 2), "\n")

set.seed(123)
sample_size <- 30000
total_rows <- nrow(all_data_scaled)

sample_idx <- sample(total_rows, sample_size)
som_data <- all_data_scaled[sample_idx, ]

cat( nrow(som_data), "cell\n")
cat(
    round(sample_size/total_rows * 100, 2), "%\n")

saveRDS(list(
  all_data = all_data_clean,
  scaled_data = all_data_scaled,
  som_data = som_data,
  sample_idx = sample_idx,
  years = years,
  es_vars = es_vars
), file = paste0(path, "es_data_prepared.rds"))

cat("\n Data saved to file.:", 
    paste0(path, "es_data_prepared.rds\n"))

************************************************************

library(kohonen)
library(raster)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cluster)
library(fmsb)

path <- "D:/Aban_1_new/data/"
years <- c(2000, 2006, 2012, 2018, 2024)
es_vars <- c("Water_Yield", "Food_Supply", "Soil_Retention", "Carbon_Storage", "HABITAT_QUALITY")


if(file.exists(paste0(path, "es_data_prepared.rds"))) {
  prepared_data <- readRDS(paste0(path, "es_data_prepared.rds"))
  
  all_data_clean <- prepared_data$all_data
  all_data_scaled <- prepared_data$scaled_data
  years <- prepared_data$years
  es_vars <- prepared_data$es_vars  
} 

set.seed(123) 
sample_size <- 30000
total_rows <- nrow(all_data_scaled)

sample_idx <- sample(total_rows, sample_size)
som_data <- all_data_scaled[sample_idx, ]

cat("  اندازه نمونه:", nrow(som_data), "cell\n")
cat("  تعداد متغیرها:", ncol(som_data), "\n")
cat("  نسبت نمونه به کل:", round(sample_size/total_rows * 100, 2), "%\n")

# SOM (15×15 = 225 neuron)
som_grid <- somgrid(xdim = 15, ydim = 15, topo = "hexagonal")


start_time <- Sys.time()

set.seed(123)
som_model <- som(som_data, 
                 grid = som_grid,
                 rlen = 500,        
                 keep.data = TRUE,
                 dist.fcts = "sumofsquares",
                 mode = "online")

end_time <- Sys.time()
execution_time <- difftime(end_time, start_time, units = "mins")

cat("  time:", round(execution_time, 2), "minute\n")

par(mfrow = c(2, 2))

plot(som_model, type = "changes", 
     main = "Training Progress",
     sub = paste("Final Error:", round(tail(som_model$changes[,1], 1), 4)))


plot(som_model, type = "counts", 
     main = "Neuron Counts",
     shape = "round")


plot(som_model, type = "quality", 
     main = "Mapping Quality")


plot(som_model, type = "dist.neighbours", 
     main = "Neighbour Distances")


som_codes <- som_model$codes[[1]]

#  silhouette  k=2 - k=8
k_values <- 2:8
sil_scores <- numeric(length(k_values))
wss_scores <- numeric(length(k_values))

for(i in seq_along(k_values)) {
  k <- k_values[i]
  
  set.seed(123)
  kmeans_temp <- kmeans(som_codes, centers = k, nstart = 25)

  sil <- silhouette(kmeans_temp$cluster, dist(som_codes))
  sil_scores[i] <- mean(sil[, 3])
  
  wss_scores[i] <- kmeans_temp$tot.withinss
  
  cat("  k =", k, ": silhouette =", round(sil_scores[i], 4), "\n")
}


best_k <- k_values[which.max(sil_scores)]

par(mfrow = c(1, 2))
plot(k_values, sil_scores, type = "b", 
     xlab = "تعداد خوشه‌ها", 
     ylab = "Silhouette Coefficient",
     main = "تعیین تعداد بهینه خوشه",
     col = "blue", pch = 19)
abline(v = best_k, col = "red", lty = 2, lwd = 2)
grid()

plot(k_values, wss_scores, type = "b",
     xlab = "تعداد خوشه‌ها",
     ylab = "Within Sum of Squares",
     main = "Elbow Method",
     col = "green", pch = 19)
grid()

set.seed(123)
kmeans_final <- kmeans(som_codes, centers = best_k, nstart = 50)


cluster_per_neuron <- kmeans_final$cluster

train_clusters <- cluster_per_neuron[som_model$unit.classif]


bundle_profiles <- aggregate(som_data, 
                             by = list(Cluster = train_clusters), 
                             FUN = mean)

bundle_profiles_norm <- bundle_profiles
for(j in 2:ncol(bundle_profiles_norm)) {
  col_min <- min(bundle_profiles_norm[, j])
  col_max <- max(bundle_profiles_norm[, j])
  if(col_max > col_min) {
    bundle_profiles_norm[, j] <- (bundle_profiles_norm[, j] - col_min) / (col_max - col_min)
  } else {
    bundle_profiles_norm[, j] <- 0.5
  }
}

print(round(bundle_profiles_norm, 3))


radar_data <- as.data.frame(bundle_profiles_norm[, -1])
rownames(radar_data) <- paste("Bundle", 1:best_k)

radar_data <- rbind(rep(1, ncol(radar_data)),
                    rep(0, ncol(radar_data)),
                    radar_data)

colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FFE194", "#A52A2A")


par(mfrow = c(1, 1))

radarchart(radar_data,
           axistype = 1,
           pcol = colors[1:best_k],
           pfcol = adjustcolor(colors[1:best_k], alpha.f = 0.3),
           plwd = 2,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0, 1, 0.25),
           cglwd = 0.8,
           vlcex = 0.8,
           title = "ES Bundles Characteristics")

legend(x = 1.2, y = 1, 
       legend = rownames(radar_data)[3:(best_k+2)],
       bty = "n", pch = 20, col = colors[1:best_k],
       text.col = "grey", cex = 0.9, pt.cex = 2)

saveRDS(list(
  som_model = som_model,
  kmeans_final = kmeans_final,
  best_k = best_k,
  bundle_profiles = bundle_profiles,
  bundle_profiles_norm = bundle_profiles_norm,
  sample_idx = sample_idx,
  som_data = som_data,
  cluster_per_neuron = cluster_per_neuron
), file = paste0(path, "som_results_30k.rds"))


dev.copy(png, paste0(path, "Fig5a_Radar_Chart.png"), 
         width = 2000, height = 1600, res = 300)
dev.off()





do_full_prediction <- TRUE  
if(do_full_prediction) {
  
  batch_size <- 50000
  n_batches <- ceiling(total_rows / batch_size)
  all_clusters <- numeric(total_rows)
  
  for(i in 1:n_batches) {
    start_idx <- (i-1) * batch_size + 1
    end_idx <- min(i * batch_size, total_rows)
    
    batch_data <- all_data_scaled[start_idx:end_idx, , drop = FALSE]

    pred <- predict(som_model, newdata = batch_data)
    all_clusters[start_idx:end_idx] <- cluster_per_neuron[pred$unit.classif]
  }
  

  saveRDS(all_clusters, file = paste0(path, "all_clusters_30k.rds"))
}

********************************************************************

library(raster)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(viridis)

som_results <- readRDS(paste0(path, "som_results_30k.rds"))
all_clusters <- readRDS(paste0(path, "all_clusters_30k.rds"))


som_model <- som_results$som_model
kmeans_final <- som_results$kmeans_final
best_k <- som_results$best_k
bundle_profiles <- som_results$bundle_profiles
bundle_profiles_norm <- som_results$bundle_profiles_norm
cluster_per_neuron <- som_results$cluster_per_neuron

par(mfrow = c(2, 2))


plot(som_model, type = "counts", 
     main = "تعداد نمونه در هر نورون",
     shape = "round",
     palette.name = rainbow)


plot(som_model, type = "dist.neighbours", 
     main = "فاصله بین نورون‌ها (U-Matrix)",
     palette.name = terrain.colors)

plot(som_model, type = "quality", 
     main = "کیفیت نگاشت",
     palette.name = topo.colors)


plot(som_model, type = "mapping", 
     main = "خوشه‌های نهایی",
     col = cluster_per_neuron,
     pch = 19)

dev.copy(png, paste0(path, "SOM_Maps.png"), 
         width = 3000, height = 2400, res = 300)
dev.off()

bundle_long <- bundle_profiles_norm %>%
  pivot_longer(cols = -Cluster, names_to = "ES", values_to = "Value")

p1 <- ggplot(bundle_long, aes(x = ES, y = Value, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FFE194")) +
  labs(title = "پروفایل خوشه‌های ES",
       x = "خدمات اکوسیستمی",
       y = "مقدار نرمال‌شده",
       fill = "خوشه") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)

ggsave(paste0(path, "Bundle_Profiles_Bar.png"), 
       plot = p1, width = 10, height = 6, dpi = 300)


heatmap_data <- as.matrix(bundle_profiles_norm[, -1])
rownames(heatmap_data) <- paste("خوشه", 1:best_k)


library(pheatmap)
pheatmap(heatmap_data,
         main = "نقشه حرارتی پروفایل خوشه‌ها",
         color = colorRampPalette(c("blue", "white", "red"))(100),
         display_numbers = TRUE,
         number_format = "%.2f",
         filename = paste0(path, "Bundle_Heatmap.png"),
         width = 8, height = 6)


template <- raster(paste0(path, "Water_Yield_2000.tif"))


clusters_raster <- raster(template)


valid_cells <- which(!is.na(values(template)))


if(length(valid_cells) == length(all_clusters)) {
  values(clusters_raster)[valid_cells] <- all_clusters
} else {
  cat("تطابق تعداد سلول‌ها:", length(valid_cells), "vs", length(all_clusters), "\n")
  n <- min(length(valid_cells), length(all_clusters))
  values(clusters_raster)[valid_cells[1:n]] <- all_clusters[1:n]
}


writeRaster(clusters_raster, 
            filename = paste0(path, "ES_Bundles_Raster.tif"),
            format = "GTiff", overwrite = TRUE)
