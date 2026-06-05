# ====================================================
#  سطح و پراکنش مکانی شهرنشینی جدید
# ====================================================

library(raster)
library(ggplot2)
library(dplyr)
library(patchwork)
library(viridis)

path <- "D:/Aban_1_new/data/"
years <- c(2000, 2006, 2012, 2018, 2024)

# ====================================================
# ۱. محاسبه مقادیر میانگین برای نمودار خطی 
# ====================================================

mean_values <- c()
sd_values <- c()

for(year in years) {
 
  ntui_file <- paste0(path, "NTUI_", year, ".tif")
  
  if(file.exists(ntui_file)) {
    r <- raster(ntui_file)
    
    mean_val <- cellStats(r, mean, na.rm = TRUE)
    sd_val <- cellStats(r, sd, na.rm = TRUE)
    
    mean_values <- c(mean_values, mean_val)
    sd_values <- c(sd_values, sd_val)
    
    cat(sprintf("  سال %d: میانگین = %.4f, انحراف معیار = %.4f\n", 
                year, mean_val, sd_val))
  } else {
    mean_values <- c(mean_values, NA)
    sd_values <- c(sd_values, NA)
  }
}

trend_df <- data.frame(
  Year = years,
  Mean = mean_values,
  SD = sd_values
)

trend_df <- na.omit(trend_df)

p_trend <- ggplot(trend_df, aes(x = Year, y = Mean)) +
  geom_line(color = "blue", size = 1.5) +
  geom_point(color = "red", size = 4) +
  geom_text(aes(label = sprintf("%.4f", Mean)), 
            vjust = -1, size = 3.5) +
  geom_ribbon(aes(ymin = Mean - SD, ymax = Mean + SD), 
              alpha = 0.2, fill = "lightblue") +
  scale_x_continuous(breaks = years) +
  scale_y_continuous(limits = c(0, max(mean_values, na.rm = TRUE) * 1.2),
                     labels = scales::number_format(accuracy = 0.01)) +
  labs(title = "New-type Urbanization Level",
       x = "Year",
       y = "NTUI Level") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

print(p_trend)

ggsave(paste0(path, "Fig6a_NTUI_Trend.png"), 
       plot = p_trend, width = 10, height = 6, dpi = 300)

breaks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, Inf)
labels <- c("≤0.1", "≤0.2", "≤0.3", "≤0.4", "≤0.5", ">0.5")
colors <- c("#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15")

map_list <- list()

breaks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, Inf)
labels <- c("≤0.1", "≤0.2", "≤0.3", "≤0.4", "≤0.5", ">0.5")
colors <- c("#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15")
map_list <- list()

for(year in years) {
  cat("  year", year, "...\n")
  
 
  ntui_file <- paste0(path, "NTUI_", year, ".tif")
  
  if(file.exists(ntui_file)) {
    r <- raster(ntui_file)
    
    df_map <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
    names(df_map) <- c("x", "y", "value")
    
    df_map$class <- cut(df_map$value, breaks = breaks, labels = labels)
    
    p <- ggplot(df_map) +
      geom_raster(aes(x = x, y = y, fill = class)) +
      scale_fill_manual(
        values = setNames(colors, labels),
        name = "NTUI Level",
        drop = FALSE
      ) +
      coord_fixed() +
      ggtitle(paste(year)) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "none"  
      )
    
    map_list[[as.character(year)]] <- p
  }
}

df_legend <- data.frame(
  x = 1:6,
  y = 1,
  class = factor(labels, levels = labels)
)

legend_plot <- ggplot(df_legend, aes(x = x, y = y, fill = class)) +
  geom_tile() +
  scale_fill_manual(
    values = setNames(colors, labels),
    name = "NTUI Level",
    drop = FALSE
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

library(cowplot)
legend <- get_legend(legend_plot)

if(length(map_list) == 5) {

  top_row <- map_list[c("2000", "2006", "2012")]
  bottom_row <- map_list[c("2018", "2024")]
  
  combined_maps <- wrap_plots(
    top_row[[1]], top_row[[2]], top_row[[3]],
    bottom_row[[1]], bottom_row[[2]], plot_spacer(),
    ncol = 3
  ) +
    plot_annotation(
      title = "New-type Urbanization Level Spatial Distribution",
      theme = theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
      )
    )

  final_plot <- plot_grid(
    combined_maps,
    legend,
    ncol = 1,
    rel_heights = c(10, 1)
  )
  
  print(final_plot)
  
  ggsave(paste0(path, "Fig6b_NTUI_Spatial.png"), 
         plot = final_plot, width = 15, height = 12, dpi = 300)
}


for(year in years) {
  ntui_file <- paste0(path, "NTUI_", year, ".tif")
  
  if(file.exists(ntui_file)) {
    r <- raster(ntui_file)
    
    df_map <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
    names(df_map) <- c("x", "y", "value")
    df_map$class <- cut(df_map$value, breaks = breaks, labels = labels)
    
    p <- ggplot(df_map) +
      geom_raster(aes(x = x, y = y, fill = class)) +
      scale_fill_manual(
        values = setNames(colors, labels),
        name = "NTUI Level",
        drop = FALSE
      ) +
      coord_fixed() +
      ggtitle(paste("New-type Urbanization Level -", year)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()
      )
    
    ggsave(paste0(path, "NTUI_Map_", year, ".png"), 
           plot = p, width = 10, height = 8, dpi = 300)
  }
}

stats_df <- data.frame(
  Year = years,
  Mean = mean_values,
  SD = sd_values,
  Min = NA,
  Max = NA
)

for(i in seq_along(years)) {
  year <- years[i]
  ntui_file <- paste0(path, "NTUI_", year, ".tif")
  
  if(file.exists(ntui_file)) {
    r <- raster(ntui_file)
    stats_df$Min[i] <- cellStats(r, min, na.rm = TRUE)
    stats_df$Max[i] <- cellStats(r, max, na.rm = TRUE)
  }
}

print(round(stats_df, 4))

write.csv(stats_df, file = paste0(path, "NTUI_Statistics.csv"), row.names = FALSE)

if(length(years) >= 2) {
  
  r_first <- raster(paste0(path, "NTUI_", years[1], ".tif"))
  r_last <- raster(paste0(path, "NTUI_", years[length(years)], ".tif"))
  
  change_raster <- r_last - r_first
  
  df_change <- as.data.frame(change_raster, xy = TRUE, na.rm = TRUE)
  names(df_change) <- c("x", "y", "change")
  
  p_change <- ggplot(df_change) +
    geom_raster(aes(x = x, y = y, fill = change)) +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0,
      name = "Change"
    ) +
    coord_fixed() +
    ggtitle(paste("NTUI Change (", years[1], "-", years[length(years)], ")")) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom"
    )
  
  print(p_change)
  
  ggsave(paste0(path, "NTUI_Change_Map.png"), 
         plot = p_change, width = 10, height = 8, dpi = 300)
}

trend_df <- data.frame(
  Year = years,
  Mean = mean_values,
  SD = sd_values
)

trend_df <- na.omit(trend_df)

p_rect <- ggplot(trend_df, aes(x = factor(Year), y = Mean)) +
  
  geom_col(fill = "lightblue", width = 0.5) +

  geom_text(aes(label = sprintf("%.4f", Mean)), 
            vjust = -0.8, size = 4, fontface = "plain") +

  geom_hline(yintercept = seq(0, max(mean_values, na.rm = TRUE) + 0.05, by = 0.05), 
             color = "gray85", size = 0.3) +
  

  scale_y_continuous(
    limits = c(0, max(mean_values, na.rm = TRUE) * 1.15),
    breaks = seq(0, max(mean_values, na.rm = TRUE) + 0.05, by = 0.05),
    labels = scales::number_format(accuracy = 0.01),
    expand = c(0, 0)
  ) +
  
  scale_x_discrete() +

  labs(
    title = "(a) New-type urbanization level of San Antonio",
    x = NULL,
    y = "New-type urbanization level"
  ) +
  

  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.y = element_text(size = 11, face = "plain"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 11, color = "black", face = "plain"),
    axis.ticks = element_line(color = "blue"),
    axis.line = element_line(color = "black"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank()
  )

print(p_rect)

ggsave(paste0(path, "Fig6a_Rectangular_chart_Data.png"), 
       plot = p_rect, width = 8, height = 6, dpi = 300)

print(trend_df)

********************************************

library(raster)
library(ggplot2)
library(patchwork)
library(cowplot)
library(dplyr)

path <- "D:/Aban_1_new/data/"
years <- c(2000, 2006, 2012, 2018, 2024)

breaks <- c(0, 0.1, 0.2, 0.3, 0.4, Inf) 
labels <- c("≤0.1", "≤0.2", "≤0.3", "≤0.4", ">0.4")
colors <- c("#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26")


for(i in 1:length(labels)) {
  cat(sprintf("  %s: %s\n", labels[i], colors[i]))
}

map_list <- list()

for(year in years) {
  
  ntui_file <- paste0(path, "NTUI_", year, ".tif")
  
  if(file.exists(ntui_file)) {
    r <- raster(ntui_file)

    df_map <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
    names(df_map) <- c("x", "y", "value")
    
    df_map$class <- cut(df_map$value, 
                        breaks = breaks, 
                        labels = labels,
                        include.lowest = TRUE,
                        right = TRUE)
    
    class_counts <- table(df_map$class)
    cat("    توزیع:", paste(names(class_counts), class_counts, collapse = ", "), "\n")
    p <- ggplot(df_map) +
      geom_raster(aes(x = x, y = y, fill = class)) +
      scale_fill_manual(
        values = setNames(colors, labels),
        name = "NTUI Level",
        drop = FALSE
      ) +
      coord_fixed() +
      ggtitle(paste(year)) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "none"
      )
    
    map_list[[as.character(year)]] <- p
  } }

df_legend <- data.frame(
  x = 1:length(labels),
  y = 1,
  class = factor(labels, levels = labels)
)

legend_plot <- ggplot(df_legend, aes(x = x, y = y, fill = class)) +
  geom_tile() +
  scale_fill_manual(
    values = setNames(colors, labels),
    name = "NTUI Level",
    drop = FALSE
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.5, "cm")
  )

legend <- get_legend(legend_plot)

# ====================================================
#  patchwork 
# ====================================================

if(length(map_list) == 5) {
  # حذف legend از همه نقشه‌ها
  maps_no_legend <- lapply(map_list, function(p) {
    p + theme(legend.position = "none")
  })
  
  design <- "
  123
  45#
  "
  
  combined_maps <- wrap_plots(
    maps_no_legend[["2000"]], maps_no_legend[["2006"]], maps_no_legend[["2012"]],
    maps_no_legend[["2018"]], maps_no_legend[["2024"]],
    design = design
  ) +
    plot_annotation(
      title = "(b) New-type urbanization level spatial distribution",
      theme = theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold")
      )
    )
  
  final_plot <- combined_maps / legend +
    plot_layout(heights = c(10, 1))
  
  print(final_plot)
  
  ggsave(paste0(path, "Fig6b_NTUI_Spatial_Patchwork.svg"), 
         plot = final_plot, width = 15, height = 12, dpi = 300)
}

for(i in 1:length(labels)) {
  count <- sum(all_years_df$class == labels[i], na.rm = TRUE)
  percent <- count / nrow(all_years_df) * 100
  cat(sprintf("  %s: %s cell (%.2f%%)\n", 
              labels[i], format(count, big.mark = ","), percent))
}

*************************************
all_values <- c()
for(year in years) {
  r <- raster(paste0(path, "NTUI_", year, ".tif"))
  all_values <- c(all_values, values(r))
}
all_values <- all_values[!is.na(all_values)]

cat("  min:", min(all_values), "\n")
cat("  max:", max(all_values), "\n")
cat("  mean:", mean(all_values), "\n")
cat("  چندک‌ها:\n")
print(quantile(all_values, probs = c(0, 0.25, 0.5, 0.75, 0.95, 1)))
# ====================================================

