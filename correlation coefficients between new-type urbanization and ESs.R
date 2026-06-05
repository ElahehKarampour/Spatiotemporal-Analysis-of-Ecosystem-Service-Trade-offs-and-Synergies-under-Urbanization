library(raster)
library(Hmisc)  

path <- "D:/Aban_1_new/data/"
template <- raster(paste0(path, "Water_Yield_2012.tif")) 

year <- 2012

es_vars <- c("WY", "FS", "SR", "CS", "HQ")
es_files <- c(
  "Water_Yield", 
  "Food_Supply", 
  "Soil_Retention", 
  "Carbon_Storage", 
  "HABITAT_QUALITY"
)

es_list <- list()
for(i in seq_along(es_vars)) {
  file_name <- paste0(path, es_files[i], "_", year, ".tif")
  if(file.exists(file_name)) {
    es_list[[es_vars[i]]] <- raster(file_name)
    cat("  ✅", es_vars[i], "بارگذاری شد\n")
  } else {
    cat("  ⚠️", es_vars[i], "یافت نشد:", file_name, "\n")
  }
}

urban_vars <- c("PD", "UPR", "GSPC", "PBL")
urban_files <- c(
  "PD",           # تراکم جمعیت
  "UPR",          # نسبت جمعیت شهری
  "GSPC",         # سرانه فضای سبز
  "PBL"           # درصد اراضی ساخته‌شده
)

urban_list <- list()
for(i in seq_along(urban_vars)) {
  file_name <- paste0(path, urban_files[i], "_", year, ".tif")
  if(file.exists(file_name)) {
    urban_list[[urban_vars[i]]] <- raster(file_name)
    cat("  ✅", urban_vars[i], "بارگذاری شد\n")
  } else {
    cat("  ⚠️", urban_vars[i], "یافت نشد:", file_name, "\n")
  }
}
urban_list

es_resampled <- list()
for(var in names(es_list)) {
  es_resampled[[var]] <- resample(es_list[[var]], template, method = 'bilinear')
}

urban_resampled <- list()
for(var in names(urban_list)) {
  urban_resampled[[var]] <- resample(urban_list[[var]], template, method = 'bilinear')
}

es_df <- data.frame(lapply(es_resampled, values))

urban_df <- data.frame(lapply(urban_resampled, values))

combined_df <- cbind(es_df, urban_df)

combined_df_clean <- na.omit(combined_df)

cor_results <- rcorr(as.matrix(combined_df_clean), type = "pearson")

cor_matrix <- cor_results$r

p_matrix <- cor_results$P


cor_table <- cor_matrix[urban_vars[urban_vars %in% colnames(cor_matrix)], 
                        es_vars[es_vars %in% colnames(cor_matrix)]]

p_table <- p_matrix[urban_vars[urban_vars %in% colnames(p_matrix)], 
                    es_vars[es_vars %in% colnames(p_matrix)]]

format_correlation <- function(cor_val, p_val) {
  cor_rounded <- round(cor_val, 3)
  
  cor_str <- sprintf("%.3f", cor_rounded)
  
  cor_str <- gsub("^0\\.", "\\.", cor_str)
  cor_str <- gsub("^-0\\.", "-\\.", cor_str)
  
  if(p_val < 0.01) {
    cor_str <- paste0(cor_str, "**")
  } else if(p_val < 0.05) {
    cor_str <- paste0(cor_str, "*")
  }
  
  return(cor_str)
}

final_table <- matrix("", nrow = nrow(cor_table), ncol = ncol(cor_table))
rownames(final_table) <- rownames(cor_table)
colnames(final_table) <- colnames(cor_table)

for(i in 1:nrow(cor_table)) {
  for(j in 1:ncol(cor_table)) {
    if(!is.na(cor_table[i,j]) & !is.na(p_table[i,j])) {
      final_table[i,j] <- format_correlation(cor_table[i,j], p_table[i,j])
    }
  }
}

final_df <- as.data.frame(final_table)
print(final_df)

write.csv(final_df, 
          file = paste0(path, "correlation_table_", year, ".csv"),
          row.names = TRUE)

print(round(cor_table, 3))

print(round(p_table, 4))

library(ggplot2)
library(reshape2)

cor_melt <- melt(as.matrix(cor_table))
names(cor_melt) <- c("Urban", "ES", "Correlation")

p <- ggplot(cor_melt, aes(x = ES, y = Urban, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-1, 1)) +
  geom_text(aes(label = sprintf("%.2f", Correlation)), size = 3) +
  labs(title = paste("Correlation between Urbanization and ESs (", year, ")"),
       x = "Ecosystem Services", y = "Urbanization Indicators") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p)

ggsave(paste0(path, "correlation_heatmap_", year, ".png"), 
       plot = p, width = 10, height = 8, dpi = 300)

es_stats <- data.frame(
  Variable = names(es_resampled),
  Min = sapply(es_resampled, function(x) cellStats(x, min, na.rm = TRUE)),
  Max = sapply(es_resampled, function(x) cellStats(x, max, na.rm = TRUE)),
  Mean = sapply(es_resampled, function(x) cellStats(x, mean, na.rm = TRUE)),
  SD = sapply(es_resampled, function(x) cellStats(x, sd, na.rm = TRUE))
)

es_stats_numeric <- es_stats[, -1]
es_stats_rounded <- round(es_stats_numeric, 3)
es_stats_final <- cbind(Variable = es_stats$Variable, es_stats_rounded)
print(es_stats_final)

urban_stats <- data.frame(
  Variable = names(urban_resampled),
  Min = sapply(urban_resampled, function(x) cellStats(x, min, na.rm = TRUE)),
  Max = sapply(urban_resampled, function(x) cellStats(x, max, na.rm = TRUE)),
  Mean = sapply(urban_resampled, function(x) cellStats(x, mean, na.rm = TRUE)),
  SD = sapply(urban_resampled, function(x) cellStats(x, sd, na.rm = TRUE))
)

urban_stats_numeric <- urban_stats[, -1]
urban_stats_rounded <- round(urban_stats_numeric, 3)
urban_stats_final <- cbind(Variable = urban_stats$Variable, urban_stats_rounded)
print(urban_stats_final)

***********************************************************************

