library(terra)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
library(networkD3)
library(patchwork)

path <- "D:/Aban_1_new/data/"
years <- c(2000, 2006, 2012, 2018, 2024)
es_vars <- c("Water_Yield", "Food_Supply", 
             "Soil_Retention", "Carbon_Storage", 
             "HABITAT_QUALITY")


create_bundle <- function(year){

  rasters <- lapply(es_vars, function(v){
    rast(paste0(path, v, "_", year, ".tif"))
  })
  
  es_stack <- rast(rasters)
  names(es_stack) <- es_vars
  
  df <- as.data.frame(es_stack, xy=FALSE, na.rm=FALSE)
  
  valid_cells <- complete.cases(df)
  
  df_scaled <- scale(df[valid_cells, es_vars])
  
  set.seed(123)
  k <- 3
  km <- kmeans(df_scaled, centers=k)
  
  bundle_raster <- rast(es_stack[[1]])
  values(bundle_raster) <- NA
  
  values(bundle_raster)[valid_cells] <- km$cluster
  
  return(bundle_raster)
}
bundle_results <- list()

for (y in years){
  bundle_results[[as.character(y)]] <- create_bundle(y)
}
plot_list <- list()

for (y in years){
  
  r <- bundle_results[[as.character(y)]] <- create_bundle(y)
  
  df_map <- as.data.frame(r, xy=TRUE, na.rm=TRUE)
  colnames(df_map)[3] <- "bundle"
  
  print(paste("Year", y, "Rows:", nrow(df_map)))  # چک
  
  p <- ggplot(df_map) +
    geom_raster(aes(x=x, y=y, fill=factor(bundle))) +
    scale_fill_manual(values=c("grey","blue","orange")) +
    ggtitle(paste("Year", y)) +
    theme_void()
  
  plot_list[[as.character(y)]] <- p
}

print(wrap_plots(plot_list))

dev.off()


# ────────────────────────────────────────────────────────────────
#  Calculate the average of each ecosystem service for each bundle (focus on 2024)
# ────────────────────────────────────────────────────────────────

year_focus <- "2024"
r_bundle   <- bundle_results[[year_focus]]
r_stack    <- rast(lapply(es_vars, function(v) {
  rast(paste0(path, v, "_", year_focus, ".tif"))
}))
names(r_stack) <- es_vars

df_es <- as.data.frame(r_stack, xy = TRUE)

df_bundle <- as.data.frame(r_bundle, xy = TRUE)
colnames(df_bundle)[3] <- "bundle"

df_combined <- df_es %>%
  inner_join(df_bundle %>% select(x, y, bundle), by = c("x", "y")) %>%
  filter(!is.na(bundle))  

# Average per service per bundle
bundle_mean <- df_combined %>%
  group_by(bundle) %>%
  summarise(
    across(all_of(es_vars), ~mean(., na.rm = TRUE)),
    n_cells = n(),                
    .groups = "drop"
  ) %>%
  mutate(
    bundle = as.character(bundle),
  
    prop = n_cells / sum(n_cells) * 100
  )

print(bundle_mean %>% arrange(bundle))

bundle_mean_scaled <- bundle_mean %>%
  mutate(across(all_of(es_vars), ~scales::rescale(.)))

print(bundle_mean_scaled)
*************************************

library(ggplot2)
library(patchwork)  

plot_list <- list()

bundle_mapping <- c(
  "1" = "EPB",
  "2" = "CCB",
  "3" = "SSB"
)

color_mapping <- c(
  "CCB"   = "grey50",
  "SSB"    = "orange",
  "EPB" = "blue"
)

for (y in years) {
  
  r <- bundle_results[[as.character(y)]]
  
  df_map <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  colnames(df_map) <- c("x", "y", "bundle")    
  
  df_map <- df_map %>%
    mutate(
      bundle_name = recode(as.character(bundle), !!!bundle_mapping),
      bundle_name = factor(bundle_name, 
                         levels = c("CCB", "SSB", "EPB"))
    )
  
  cat("Year", y, " - num_cells:", format(nrow(df_map), big.mark = ","), "\n")
  
  p <- ggplot(df_map) +
    geom_raster(aes(x = x, y = y, fill = bundle_name)) +
    scale_fill_manual(
      values = color_mapping,
      name = NULL,
      drop = FALSE  
    ) +
    labs(title = paste(y)) +
    coord_fixed() +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom",
      #legend.title = element_blank()
      legend.key.size = unit(0.8, "lines")
    )
  
  plot_list[[as.character(y)]] <- p
}

combined_plots <- wrap_plots(plot_list, ncol = 3)   
print(combined_plots)

ggsave("D:/Aban_1_new/imgs/ES_Bundles_All_Years_Named.png",
       plot = combined_plots,
       width = 18, height = 10, dpi = 300, bg = "white")
******************************************

plot_list <- list()

for (y in years) {
  
  r <- bundle_results[[as.character(y)]]
  
  df_map <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  colnames(df_map) <- c("x", "y", "bundle")
  
  df_map <- df_map %>%
    mutate(
      bundle_name = recode(as.character(bundle), !!!bundle_mapping),
      bundle_name = factor(bundle_name, levels = c("CCB", "SSB", "EPB"))
    )
  
  cat("Year", y, " - num_cells:", format(nrow(df_map), big.mark = ","), "\n")
  
  p <- ggplot(df_map) +
    geom_raster(aes(x = x, y = y, fill = bundle_name)) +
    scale_fill_manual(
      values = color_mapping,
      name = NULL,
      drop = FALSE
    ) +
    labs(title = paste(y)) +
    coord_fixed() +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.key.size = unit(0.8, "lines")
    )
  
  if (y == max(years)) {
    p <- p + theme(legend.position = "bottomright",
       legend.direction = "horizontal",
       legend.background = element_rect(fill = "white", colour = "grey80"),
       legend.margin = margin(4, 8, 4, 8))
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  plot_list[[as.character(y)]] <- p
}

combined_plots <- wrap_plots(
  plot_list, 
  ncol = 3
) &
  theme(
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "white", colour = "grey80"),
    legend.margin = margin(4, 8, 4, 8)
  )

legend_plot <- ggplot(data.frame(bundle_name = factor(c("CCB","SSB","EPB")))) +
  geom_bar(aes(x = bundle_name, fill = bundle_name)) +
  scale_fill_manual(values = color_mapping, name = NULL) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.background = element_rect(fill = "white", colour = "white"),
    legend.margin = margin(0, 0, 0, 0)
  )

the_legend <- cowplot::get_legend(legend_plot)

plots_without_legend <- lapply(plot_list, function(p) {
  p + theme(legend.position = "none")
})

combined_no_legend <- wrap_plots(plots_without_legend, ncol = 3)

final_plot <- combined_no_legend +
  inset_element(
    the_legend,
    left   = 2.08,    
    bottom = 0.02,
    right  = 0.99,
    top    = 0.50,
    align_to = "full",
    ignore_tag = TRUE
  )

print(final_plot)
print(combined_plots)

ggsave(
  "D:/Aban_1_new/imgs/ES_Bundles_All_Years_OneLegend.png",
  final_plot,
  width = 18, 
  height = 10,
  bg = "white"
)

library(ggplot2)
library(patchwork)
library(terra)

plot_list <- list()
years <- c(2000, 2006, 2012, 2018, 2024)

for (y in years) {
  
  r <- bundle_results[[as.character(y)]]

  df_map <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  
  names(df_map) <- c("x", "y", "bundle")
  
  cat("  num_rows:", format(nrow(df_map), big.mark = ","), "\n")
  
  p <- ggplot(df_map) +
    geom_raster(aes(x = x, y = y, fill = factor(bundle))) +
    scale_fill_manual(
      values = c("grey", "blue", "orange", "green", "red", "purple"),
      name = "ES Bundle"
    ) +
    coord_fixed() +
    ggtitle(paste("ES Bundles -", y)) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom"
    )
  
  plot_list[[as.character(y)]] <- p
}

combined_plots <- wrap_plots(plot_list, ncol = 3)
print(combined_plots)

ggsave(paste0(path, "ES_Bundles_All_Years.png"), 
       plot = combined_plots, 
       width = 18, height = 12, dpi = 300)

r2024 <- bundle_results[["2024"]]         

df_2024 <- as.data.frame(r2024, xy = TRUE, na.rm = TRUE)
colnames(df_2024) <- c("x", "y", "bundle")

print(sort(unique(df_2024$bundle)))         
library(dplyr)
library(tidyr)
df_map

df_all <- df_es %>% 
  inner_join(df_2024 %>% select(x, y, bundle), by = c("x", "y"))

bundle_mean <- df_all %>%
  group_by(bundle) %>%
  summarise(across(where(is.numeric) & !c(x,y,bundle), 
                   ~mean(., na.rm = TRUE))) %>%
  ungroup()

bundle_mean_long <- bundle_mean %>%
  pivot_longer(-bundle, names_to = "ES", values_to = "Mean") %>%
  arrange(bundle, desc(Mean))

print(bundle_mean_long, n = 30)
**********************************************

plot_list <- list()
years <- c(2000, 2006, 2012, 2018, 2024)

for (y in years) {
  
  r <- bundle_results[[as.character(y)]]
  
  df_map <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  
  names(df_map) <- c("x", "y", "bundle")
  
  cat("  num_rows:", format(nrow(df_map), big.mark = ","), "\n")
  
 
  p <- ggplot(df_map) +
    geom_raster(aes(x = x, y = y, fill = factor(bundle))) +
    scale_fill_manual(
      values = c("grey", "blue", "orange", "green", "red", "purple"),
      name = "ES Bundle"
    ) +
    coord_fixed() +
    ggtitle(paste("ES Bundles -", y)) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom"
    )
  
  plot_list[[as.character(y)]] <- p
}

combined_plots <- wrap_plots(plot_list, ncol = 3)
print(combined_plots)

cat("\n", paste(rep("=", 60), collapse = ""), "\n")

cat(paste(rep("=", 60), collapse = ""), "\n")

library(dplyr)
library(tidyr)

all_bundles <- c()
all_years <- c()

for (y in years) {
  r <- bundle_results[[as.character(y)]]
  df_temp <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df_temp) <- c("x", "y", "bundle")
  
  all_bundles <- c(all_bundles, df_temp$bundle)
  all_years <- c(all_years, rep(y, nrow(df_temp)))
}

bundle_counts <- table(all_bundles)
bundle_percent <- prop.table(bundle_counts) * 100

for(i in 1:length(bundle_counts)) {
  cat(sprintf("  خوشه %d: %s سلول (%.2f%%)\n", 
              as.numeric(names(bundle_counts)[i]),
              format(bundle_counts[i], big.mark = ","),
              bundle_percent[i]))
}

es_vars <- c("Water_Yield", "Food_Supply", "Soil_Retention", "Carbon_Storage", "HABITAT_QUALITY")
es_data_list <- list()

for(var in es_vars) {
  cat("  ", var, ": ")
  var_values <- c()
  
  for(y in years) {
    file_name <- paste0(path, var, "_", y, ".tif")
    if(file.exists(file_name)) {
      r_es <- raster(file_name)
      var_values <- c(var_values, values(r_es))
    }
  }
  
  var_values <- var_values[!is.na(var_values)]
  es_data_list[[var]] <- var_values
  cat(format(length(var_values), big.mark = ","), "value\n")
}

es_df <- as.data.frame(es_data_list)
bundle_df <- data.frame(
  bundle = all_bundles[1:nrow(es_df)]
)

valid_rows <- complete.cases(es_df)
es_df_clean <- es_df[valid_rows, ]
bundle_clean <- bundle_df$bundle[valid_rows]

cat("  تعداد کل سلول‌های معتبر:", format(nrow(es_df_clean), big.mark = ","), "\n")

es_df_clean$bundle <- bundle_clean

bundle_means <- aggregate(. ~ bundle, data = es_df_clean, FUN = mean)

bundle_means <- bundle_means[order(bundle_means$bundle), ]

print(round(bundle_means, 2))

global_means <- colMeans(es_df_clean[, es_vars])

bundle_names <- character(nrow(bundle_means))
bundle_colors <- c()

for(i in 1:nrow(bundle_means)) {
  
  cluster_num <- bundle_means$bundle[i]
  vals <- bundle_means[i, es_vars]
  
  cat(sprintf("\n cluster %d:\n", cluster_num))
  cat("  value:", paste(round(vals, 2), collapse = ", "), "\n")
  
 
  scores <- vals / global_means
  
  
  if(vals$CS > global_means["CS"] * 1.2 && vals$HQ > global_means["HQ"] * 1.2) {
    # CCB: Carbon-rich bundle
    bundle_names[i] <- "CCB"
    bundle_colors <- c(bundle_colors, "#FF6B6B")
    cat(" name: CCB (Areas with high carbon storage and habitat quality)\n")
    
  } else if(vals$SR > global_means["SR"] * 1.2 && vals$HQ > global_means["HQ"] * 1.1) {
    # SSB: Soil retention bundle
    bundle_names[i] <- "SSB"
    bundle_colors <- c(bundle_colors, "#4ECDC4")
    cat("  name: SSB (Areas with high soil protection)\n")
    
  } else if(vals$WY > global_means["WY"] * 1.3 && vals$FS > global_means["FS"] * 1.2) {
    # EPB: Ecosystem provisioning bundle
    bundle_names[i] <- "EPB"
    bundle_colors <- c(bundle_colors, "#96CEB4")
    cat(" name: EPB (Areas with high water and food production)\n")
    
  } else if(min(scores) < 0.7 && max(scores) > 1.3) {
    # TMB: Trade-off bundle
    bundle_names[i] <- "TMB"
    bundle_colors <- c(bundle_colors, "#45B7D1")
    cat("  name: TMB (Areas with conflicting services)\n")
    
  } else {
    # Mixed
    bundle_names[i] <- paste0("B", cluster_num, "_MIX")
    bundle_colors <- c(bundle_colors, "#FFE194")
    cat("  name:", bundle_names[i], "(Areas with medium composition)\n")
  }
}

results_table <- data.frame(
  Cluster = bundle_means$bundle,
  Name = bundle_names,
  Count = as.vector(bundle_counts[order(as.numeric(names(bundle_counts)))]),
  Percent = round(as.vector(bundle_percent[order(as.numeric(names(bundle_percent)))]), 2),
  WY = round(bundle_means$WY, 2),
  FS = round(bundle_means$FS, 2),
  SR = round(bundle_means$SR, 2),
  CS = round(bundle_means$CS, 2),
  HQ = round(bundle_means$HQ, 2)
)

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
print(results_table)

cluster_to_name <- setNames(bundle_names, bundle_means$bundle)
cluster_to_color <- setNames(bundle_colors, bundle_means$bundle)

plot_list_named <- list()

for (y in years) {
  cat("  year", y, "...\n")
  
  r <- bundle_results[[as.character(y)]]
  df_map <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df_map) <- c("x", "y", "bundle")
  

  df_map$bundle_name <- cluster_to_name[as.character(df_map$bundle)]
  

  p <- ggplot(df_map) +
    geom_raster(aes(x = x, y = y, fill = bundle_name)) +
    scale_fill_manual(
      values = cluster_to_color,
      name = "ES Bundle"
    ) +
    coord_fixed() +
    ggtitle(paste("ES Bundles -", y)) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom"
    )
  
  plot_list_named[[as.character(y)]] <- p
}

combined_plots_named <- wrap_plots(plot_list_named, ncol = 3)
print(combined_plots_named)

profile_long <- bundle_means %>%
  pivot_longer(cols = all_of(es_vars), names_to = "ES", values_to = "Value")

profile_long$Bundle_Name <- cluster_to_name[as.character(profile_long$bundle)]

p_profile <- ggplot(profile_long, aes(x = ES, y = Value, fill = Bundle_Name)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = cluster_to_color) +
  labs(title = "profile ES Bundles",
       x = "Ecosystem services",
       y = "Average value",
       fill = "ES Bundle") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_profile)

write.csv(results_table, 
          file = paste0(path, "bundle_summary_table.csv"),
          row.names = FALSE)

ggsave(paste0(path, "ES_Bundles_Named_All_Years.png"), 
       plot = combined_plots_named, 
       width = 18, height = 12, dpi = 300)

ggsave(paste0(path, "Bundle_Profile_Plot.png"), 
       plot = p_profile, 
       width = 10, height = 6, dpi = 300)

******************************************

es_vars <- c("Water_Yield", "Food_Supply", "Soil_Retention", "Carbon_Storage", "HABITAT_QUALITY")
years <- c(2000, 2006, 2012, 2018, 2024)

valid_cells_list <- list()

for(var in es_vars) {
  cat("  ", var, ": ")
  var_cells <- c()
  
  for(y in years) {
    file_name <- paste0(path, var, "_", y, ".tif")
    if(file.exists(file_name)) {
      r <- raster(file_name)
      valid <- which(!is.na(values(r)))
      var_cells <- c(var_cells, valid)
    }
  }
  

  valid_cells_list[[var]] <- unique(var_cells)
  cat(format(length(valid_cells_list[[var]]), big.mark = ","), "Valid cell\n")
}

# Finding common cells between all variables
common_cells <- Reduce(intersect, valid_cells_list)
cat("\n Common cells between all variables:", 
    format(length(common_cells), big.mark = ","), "\n")


es_matrix <- matrix(NA, nrow = length(common_cells), ncol = length(es_vars))
colnames(es_matrix) <- es_vars

for(j in seq_along(es_vars)) {
  var <- es_vars[j]
  cat("  ", var, ": ")
  
  all_values <- c()
  for(y in years) {
    file_name <- paste0(path, var, "_", y, ".tif")
    if(file.exists(file_name)) {
      r <- raster(file_name)
      # Extracting common cell values
      values_at_cells <- values(r)[common_cells]
      all_values <- c(all_values, values_at_cells)
    }
  }
  
  es_matrix[, j] <- all_values
  cat(format(length(all_values), big.mark = ","), "value\n")
}

es_df <- as.data.frame(es_matrix)


# Calculating the number of valid cells each year
cells_per_year <- c()
for(y in years) {
  r_temp <- raster(paste0(path, "WY_", y, ".tif"))
  cells_per_year <- c(cells_per_year, sum(!is.na(values(r_temp))))
}

for(i in seq_along(years)) {
  cat("  year", years[i], ":", format(cells_per_year[i], big.mark = ","), "\n")
}


set.seed(123)
sample_size <- min(50000, length(common_cells))  
sample_idx <- sample(length(common_cells), sample_size)

sample_clusters <- all_clusters[1:sample_size]  

es_sample <- es_df[sample_idx, ]
es_sample$bundle <- sample_clusters

bundle_means <- aggregate(. ~ bundle, data = es_sample, FUN = mean)

print(round(bundle_means, 2))


if(exists("bundle_profiles") && !is.null(bundle_profiles)) {
  cat("\n SOM: profiles\n")
  print(round(bundle_profiles, 2))
  
  bundle_means_som <- bundle_profiles
} else {
  cat(" SOM profile not found\n")
}

if(exists("bundle_means")) {
  
  global_means <- colMeans(es_sample[, es_vars])
  bundle_names <- character(nrow(bundle_means))
  
  for(i in 1:nrow(bundle_means)) {
    cluster_num <- bundle_means$bundle[i]
    vals <- bundle_means[i, es_vars]
    
    cat(sprintf("\n cluster %d:\n", cluster_num))
    cat("  values:", paste(round(vals, 2), collapse = ", "), "\n")
    
    if(vals$CS > global_means["CS"] * 1.2 && vals$HQ > global_means["HQ"] * 1.2) {
      bundle_names[i] <- "CCB"
      cat(" CCB: Areas with high carbon storage and habitat quality\n")
      
    } else if(vals$SR > global_means["SR"] * 1.2 && vals$HQ > global_means["HQ"] * 1.1) {
      bundle_names[i] <- "SSB"
      cat(" SSB: مناطق با حفاظت خاک بالا\n")
      
    } else if(vals$WY > global_means["WY"] * 1.3 && vals$FS > global_means["FS"] * 1.2) {
      bundle_names[i] <- "EPB"
      cat(" EPB: مناطق با تولید آب و غذای بالا\n")
      
    } else if((min(vals) < min(global_means) * 0.8) && (max(vals) > max(global_means) * 1.2)) {
      bundle_names[i] <- "TMB"
      cat("  ✅ TMB: مناطق با تضاد بین سرویس‌ها\n")
      
    } else {
      bundle_names[i] <- paste0("MIX", cluster_num)
      cat("  ✅ MIX: مناطق با ترکیب متوسط\n")
    }
  }
  
  
  results_table <- data.frame(
    Cluster = bundle_means$bundle,
    Name = bundle_names,
    WY = round(bundle_means$WY, 2),
    FS = round(bundle_means$FS, 2),
    SR = round(bundle_means$SR, 2),
    CS = round(bundle_means$CS, 2),
    HQ = round(bundle_means$HQ, 2)
  )
  
  cat("\n", paste(rep("=", 70), collapse = ""), "\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  print(results_table)
  
 
  write.csv(results_table, 
            file = paste0(path, "bundle_final_table.csv"),
            row.names = FALSE)
}
