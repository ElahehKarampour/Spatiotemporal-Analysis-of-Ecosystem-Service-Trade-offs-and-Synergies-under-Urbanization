library(randomForest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(raster)
library(patchwork)

path <- "D:/Aban_1_new/data/"
years <- c(2000, 2006, 2012, 2018, 2024)

es_vars <- c("Water_Yield", "Food_Supply", "Soil_Retention", "Carbon_Storage", "HABITAT_QUALITY")
es_names <- c("Water Yield", "Food Supply", "Soil Retention", 
              "Carbon Storage", "Habitat Quality")

urban_vars <- c("PD", "PBL", "UPR", "GSPC")
urban_names <- c("PD", "PBL", "UPR", "GSPC")

urban_dimensions <- data.frame(
  Variable = urban_vars,
  Dimension = c("Population",
                "Environment", "Population", "Environment"),
  Color = c("blue", "orange", "green", "darkgreen")
)

all_data <- data.frame()

for(year in years) {
  cat("  سال", year, "...\n")
  
  year_data <- list()
  
  for(var in es_vars) {
    file_name <- paste0(path, var, "_", year, ".tif")
    if(file.exists(file_name)) {
      r <- raster(file_name)
      year_data[[var]] <- values(r)
    } else {
      cat("    ⚠️ فایل", var, "یافت نشد\n")
      year_data[[var]] <- NA
    }
  }
  
  for(var in urban_vars) {
    file_name <- paste0(path, var, "_", year, ".tif")
    if(file.exists(file_name)) {
      r <- raster(file_name)
      year_data[[var]] <- values(r)
    } else {
      year_data[[var]] <- NA
    }
  }
  
  df_year <- as.data.frame(year_data)
  df_year$Year <- year
  
  df_year_clean <- na.omit(df_year)
  cat("    ✅", nrow(df_year_clean), "valid_cell\n")
  
  all_data <- rbind(all_data, df_year_clean)
}
# ====================================================
# Random Forest for each ES
# ====================================================

rf_results <- list()
importance_list <- list()
var_explained <- c()

for(i in seq_along(es_vars)) {
  es <- es_vars[i]
  es_name <- es_names[i]
  
  cat("  ", es_name, "...\n")
  
  formula_rf <- as.formula(paste(es, "~", paste(urban_vars, collapse = " + ")))

  set.seed(123)
  rf_model <- randomForest(formula_rf,
                           data = all_data,
                           ntree = 500,
                           importance = TRUE,
                           na.action = na.omit)
  
  rf_results[[es]] <- rf_model
  

  imp <- importance(rf_model)
  imp_df <- data.frame(
    Variable = rownames(imp),
    Importance = imp[, "%IncMSE"],
    ES = es_name,
    Dimension = urban_dimensions$Dimension[match(rownames(imp), urban_dimensions$Variable)],
    Color = urban_dimensions$Color[match(rownames(imp), urban_dimensions$Variable)]
  )
  
  importance_list[[es]] <- imp_df
  
  var_explained <- c(var_explained, round(rf_model$rsq[500] * 100, 2))
  cat(" واریانس توضیح داده شده:", round(rf_model$rsq[500] * 100, 2), "%\n")
}

names(var_explained) <- es_vars


importance_all <- do.call(rbind, importance_list)

plot_list <- list()

for(i in seq_along(es_vars)) {
  es <- es_vars[i]
  es_name <- es_names[i]
  
  df_es <- importance_all[importance_all$ES == es_name, ]
  
  df_es <- df_es[order(df_es$Importance), ]
  df_es$Variable <- factor(df_es$Variable, levels = df_es$Variable)
  
  p <- ggplot(df_es, aes(x = Variable, y = Importance, fill = Dimension)) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_fill_manual(values = c("Economy" = "orange", 
                                  "Environment" = "green", 
                                  "Population" = "blue")) +
    coord_flip() +
    labs(
      title = paste0("(", letters[i], ") ", es_name),
      subtitle = paste("Var explained:", var_explained[es], "%"),
      x = NULL,
      y = "%IncMSE"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray30"),
      axis.text = element_text(size = 8),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  plot_list[[es]] <- p
}

dimension_importance <- importance_all %>%
  group_by(ES, Dimension) %>%
  summarise(MeanImportance = mean(Importance), .groups = "drop")


combined_plots <- wrap_plots(
  plot_list[["WY"]], plot_list[["FS"]], plot_list[["SR"]],
  plot_list[["CS"]], plot_list[["HQ"]], plot_spacer(),
  plot_spacer(), plot_spacer(), plot_spacer(),
  ncol = 3
) +
  plot_annotation(
    title = "Fig. 7. New-type urbanization indicators importance for ecosystem services",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    )
  ) &
  theme(legend.position = "bottom")

print(combined_plots)

ggsave(paste0(path, "Fig7_Importance_All.png"), 
       plot = combined_plots, width = 15, height = 12, dpi = 300)

ggsave(paste0(path, "Fig7_Importance_Full.svg"), 
       plot = combined_plots, width = 18, height = 15, dpi = 300)

write.csv(importance_all, 
          file = paste0(path, "Importance_Results.csv"),
          row.names = FALSE)

var_df <- data.frame(
  ES = es_names,
  VarianceExplained = var_explained
)
write.csv(var_df, 
          file = paste0(path, "Variance_Explained.csv"),
          row.names = FALSE)

cat("\n", paste(rep("=", 60), collapse = ""), "\n")

cat(paste(rep("=", 60), collapse = ""), "\n")

for(i in seq_along(es_vars)) {
  cat(sprintf("\n%s (واریانس: %.2f%%):\n", es_names[i], var_explained[i]))
  df_es <- importance_all[importance_all$ES == es_names[i], ]
  df_es <- df_es[order(-df_es$Importance), ]
  for(j in 1:3) {
    cat(sprintf("  %d. %s: %.2f\n", j, df_es$Variable[j], df_es$Importance[j]))
  }
}

library(randomForest)
library(dplyr)

chunk_size <- 50000  
n_chunks <- ceiling(nrow(all_data) / chunk_size)
n_iterations <- 5  

importance_matrix <- matrix(0, nrow = length(urban_vars), ncol = length(es_vars))
rownames(importance_matrix) <- urban_vars
colnames(importance_matrix) <- es_vars

var_exp_matrix <- matrix(0, nrow = n_iterations, ncol = length(es_vars))
colnames(var_exp_matrix) <- es_vars

# ====================================================
# Patchwork processing with averaging
# ====================================================

for(iter in 1:n_iterations) {
  cat("\n repeat", iter, "از", n_iterations, "...\n")
  
  set.seed(123 + iter)
  sample_idx <- sample(nrow(all_data), chunk_size)
  data_sample <- all_data[sample_idx, ]
  
  for(i in seq_along(es_vars)) {
    es <- es_vars[i]
    es_name <- es_names[i]
    
    cat("  ", es_name, "...\n")
    
    formula_rf <- as.formula(paste(es, "~", paste(urban_vars, collapse = " + ")))

    rf_model <- randomForest(formula_rf,
                             data = data_sample,
                             ntree = 300,
                             importance = TRUE,
                             na.action = na.omit)
    
    imp <- importance(rf_model)
    importance_matrix[, i] <- importance_matrix[, i] + imp[, "%IncMSE"] / n_iterations
    
    var_exp_matrix[iter, i] <- rf_model$rsq[length(rf_model$rsq)] * 100
  }
}


var_explained <- colMeans(var_exp_matrix)
names(var_explained) <- es_vars

final_importance <- matrix(0, nrow = length(urban_vars), ncol = length(es_vars))
rownames(final_importance) <- urban_vars
colnames(final_importance) <- es_vars

weight_sum <- 0

for(chunk in 1:n_chunks) {
  cat("  chunk", chunk, "/", n_chunks, "...\n")
  
  start_idx <- (chunk-1) * chunk_size + 1
  end_idx <- min(chunk * chunk_size, nrow(all_data))
  
  data_chunk <- all_data[start_idx:end_idx, ]
  chunk_weight <- nrow(data_chunk) / nrow(all_data)  
  

  for(i in seq_along(es_vars)) {
    es <- es_vars[i]
    
    formula_rf <- as.formula(paste(es, "~", paste(urban_vars, collapse = " + ")))
    
    rf_model <- randomForest(formula_rf,
                             data = data_chunk,
                             ntree = 200,  
                             importance = TRUE)
    
    imp <- importance(rf_model)
    final_importance[, i] <- final_importance[, i] + imp[, "%IncMSE"] * chunk_weight
  }
  
  weight_sum <- weight_sum + chunk_weight
}

importance_list <- list()

for(i in seq_along(es_vars)) {
  es <- es_vars[i]
  es_name <- es_names[i]
  
  imp_df <- data.frame(
    Variable = urban_vars,
    Importance = final_importance[, i],
    ES = es_name,
    Dimension = urban_dimensions$Dimension[match(urban_vars, urban_dimensions$Variable)],
    Color = urban_dimensions$Color[match(urban_vars, urban_dimensions$Variable)]
  )
  
  imp_df <- imp_df[order(imp_df$Importance), ]
  imp_df$Variable <- factor(imp_df$Variable, levels = imp_df$Variable)
  
  importance_list[[es]] <- imp_df
}


plot_list <- list()

for(i in seq_along(es_vars)) {
  es <- es_vars[i]
  es_name <- es_names[i]
  
  p <- ggplot(importance_list[[es]], aes(x = Variable, y = Importance, fill = Dimension)) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_fill_manual(values = c("Economy" = "orange", 
                                  "Environment" = "green", 
                                  "Population" = "blue")) +
    coord_flip() +
    labs(
      title = paste0("(", letters[i], ") ", es_name),
      #subtitle = paste("Var explained:", round(var_explained[es], 2), "%"),
      x = NULL,
      y = "%IncMSE"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 9),
      legend.position = "none"
    )
  
  plot_list[[es]] <- p
}

*********************************************

library(patchwork)

combined <- wrap_plots(plot_list, ncol = 3) +
  plot_annotation(
    title = "New-type urbanization indicators importance",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  )

print(combined)

ggsave(paste0(path, "Fig7_Importance_Chunked.svg"), 
       plot = combined, width = 15, height = 12, dpi = 300)

results_df <- data.frame(
  ES = rep(es_names, each = length(urban_vars)),
  Variable = rep(urban_vars, length(es_vars)),
  Importance = as.vector(final_importance),
  Dimension = rep(urban_dimensions$Dimension, length(es_vars))
)

write.csv(results_df, paste0(path, "Importance_Results_Chunked.csv"), row.names = FALSE)


*****************************************************************
library(xgboost)
library(dplyr)
library(ggplot2)
library(patchwork)

chunk_size <- 50000
n_chunks <- ceiling(nrow(all_data) / chunk_size)

train_ratio <- 0.8
set.seed(123)

xgb_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

nrounds <- 300

final_importance <- matrix(0, nrow = length(urban_vars), ncol = length(es_vars))
rownames(final_importance) <- urban_vars
colnames(final_importance) <- es_vars

r2_matrix <- matrix(0, nrow = n_chunks, ncol = length(es_vars))
colnames(r2_matrix) <- es_vars

weight_sum <- 0

# ====================================================
# Train/Test
# ====================================================

for(chunk in 1:n_chunks) {
  
  cat("\n ", chunk, "از", n_chunks, "...\n")
  
  start_idx <- (chunk - 1) * chunk_size + 1
  end_idx <- min(chunk * chunk_size, nrow(all_data))
  
  data_chunk <- na.omit(all_data[start_idx:end_idx, ])
  
  if(nrow(data_chunk) < 50) next  
  
  chunk_weight <- nrow(data_chunk) / nrow(all_data)
  
  train_idx <- sample(nrow(data_chunk),
                      size = floor(train_ratio * nrow(data_chunk)))
  
  train_data <- data_chunk[train_idx, ]
  test_data  <- data_chunk[-train_idx, ]
  
  for(i in seq_along(es_vars)) {
    
    es <- es_vars[i]
    
    X_train <- as.matrix(train_data[, urban_vars])
    y_train <- train_data[[es]]
    
    X_test  <- as.matrix(test_data[, urban_vars])
    y_test  <- test_data[[es]]
    
    dtrain <- xgb.DMatrix(data = X_train, label = y_train)
    dtest  <- xgb.DMatrix(data = X_test,  label = y_test)
    
    model <- xgb.train(
      params = xgb_params,
      data = dtrain,
      nrounds = nrounds,
      verbose = 0
    )
    
    
    pred_test <- predict(model, dtest)
    r2 <- cor(pred_test, y_test)^2
    r2_matrix[chunk, i] <- r2 * 100
    
    imp <- xgb.importance(model = model)
    
    gain_vector <- rep(0, length(urban_vars))
    names(gain_vector) <- urban_vars
    
    if(nrow(imp) > 0) {
      gain_vector[imp$Feature] <- imp$Gain
    }
    
    final_importance[, i] <-
      final_importance[, i] + gain_vector * chunk_weight
  }
  
  weight_sum <- weight_sum + chunk_weight
}

var_explained <- colMeans(r2_matrix, na.rm = TRUE)
names(var_explained) <- es_vars

cat("\n mean Test R² (%):\n")
print(round(var_explained, 2))



importance_list <- list()

for(i in seq_along(es_vars)) {
  
  es <- es_vars[i]
  es_name <- es_names[i]
  
  imp_df <- data.frame(
    Variable = urban_vars,
    Importance = final_importance[, i],
    ES = es_name,
    Dimension = urban_dimensions$Dimension[
      match(urban_vars, urban_dimensions$Variable)
    ],
    Color = urban_dimensions$Color[
      match(urban_vars, urban_dimensions$Variable)
    ]
  )
  
  imp_df <- imp_df[order(imp_df$Importance), ]
  imp_df$Variable <- factor(imp_df$Variable,
                            levels = imp_df$Variable)
  
  importance_list[[es]] <- imp_df
}

plot_list <- list()

for(i in seq_along(es_vars)) {
  
  es <- es_vars[i]
  es_name <- es_names[i]
  
  p <- ggplot(importance_list[[es]],
              aes(x = Variable,
                  y = Importance,
                  fill = Dimension)) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_fill_manual(values = c(
      "Economy" = "orange",
      "Environment" = "green",
      "Population" = "blue"
    )) +
    coord_flip() +
    labs(
      title = paste0("(", letters[i], ") ", es_name),
      subtitle = paste("Test R²:",
                       round(var_explained[es], 2), "%"),
      x = NULL,
      y = "Gain"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5,
                                size = 11,
                                face = "bold"),
      plot.subtitle = element_text(hjust = 0.5,
                                   size = 9),
      legend.position = "none"
    )
  
  plot_list[[es]] <- p
}

combined <- wrap_plots(plot_list, ncol = 3) +
  plot_annotation(
    title = "Fig. 7. New-type urbanization indicators importance (XGBoost - Chunked Train/Test)",
    theme = theme(plot.title = element_text(
      hjust = 0.5,
      size = 14,
      face = "bold"
    ))
  )

print(combined)

ggsave(paste0(path, "Fig7_Importance_XGBoost_Chunked_TrainTest.svg"),
       plot = combined,
       width = 15,
       height = 12,
       dpi = 300)

results_df <- data.frame(
  ES = rep(es_names, each = length(urban_vars)),
  Variable = rep(urban_vars, length(es_vars)),
  Importance = as.vector(final_importance),
  Dimension = rep(urban_dimensions$Dimension,
                  length(es_vars))
)

write.csv(results_df,
          paste0(path,
                 "Importance_Results_XGBoost_Chunked_TrainTest.csv"),
          row.names = FALSE)

******************************************************************

library(xgboost)
library(dplyr)
library(ggplot2)
library(patchwork)
library(forcats)   

out_dir <- "D:/Aban_1_new/figs_xgb/"

chunk_size    <- 60000         
train_ratio   <- 0.80
set.seed(2024)

xgb_params <- list(
  objective   = "reg:squarederror",
  eval_metric = "rmse",
  max_depth   = 6,
  eta         = 0.07,
  subsample   = 0.80,
  colsample_bytree = 0.75,
  tree_method = "hist",           
  grow_policy = "lossguide"
)

n_es <- length(es_vars)

final_gain   <- matrix(0, nrow = length(urban_vars), ncol = n_es,
                       dimnames = list(urban_vars, es_vars))

r2_test_all  <- matrix(NA, nrow = 999, ncol = n_es)  
weight_sum   <- 0
chunk_counter <- 0

for (i in seq(1, nrow(all_data), by = chunk_size)) {
  
  chunk_counter <- chunk_counter + 1
  cat(sprintf(" چانک %2d  →  ", chunk_counter))
  
  chunk_end <- min(i + chunk_size - 1, nrow(all_data))
  df_chunk  <- all_data[i:chunk_end, ] |> na.omit()
  
  if (nrow(df_chunk) < 200) {
    cat("خیلی کوچک → رد شد\n")
    next
  }
  
  cat(nrow(df_chunk), " ردیف\n")
  
  chunk_weight <- nrow(df_chunk) / nrow(all_data)
  weight_sum   <- weight_sum + chunk_weight
  
  set.seed(2024 + chunk_counter)
  train_idx <- sample(seq_len(nrow(df_chunk)), size = floor(train_ratio * nrow(df_chunk)))
  
  train_df <- df_chunk[train_idx, ]
  test_df  <- df_chunk[-train_idx, ]
  
  X_train <- as.matrix(train_df[, urban_vars])
  X_test  <- as.matrix(test_df[, urban_vars])
  
  #  Ecosystem Service 
  for (j in seq_along(es_vars)) {
    
    es_target <- es_vars[j]
    
    dtrain <- xgb.DMatrix(data = X_train, label = train_df[[es_target]])
    dtest  <- xgb.DMatrix(data = X_test,  label = test_df[[es_target]])
    
    watchlist <- list(train = dtrain, eval = dtest)
    
    model <- xgb.train(
      params      = xgb_params,
      data        = dtrain,
      nrounds     = 800,
      watchlist   = watchlist,
      early_stopping_rounds = 40,
      verbose     = 0
    )
    
    pred <- predict(model, dtest)
    r2   <- cor(pred, getinfo(dtest, "label"))^2
    r2_test_all[chunk_counter, j] <- r2
    
    imp <- xgb.importance(model = model)
    
    gain_vec <- numeric(length(urban_vars))
    names(gain_vec) <- urban_vars
    
    if (nrow(imp) > 0) {
      m <- match(imp$Feature, urban_vars)
      gain_vec[m[!is.na(m)]] <- imp$Gain[!is.na(m)]
    }
    
    final_gain[, j] <- final_gain[, j] + gain_vec * chunk_weight
  }
}

final_gain <- final_gain / weight_sum

mean_r2 <- colMeans(r2_test_all[1:chunk_counter, , drop = FALSE], na.rm = TRUE) * 100
names(mean_r2) <- es_vars

print(round(mean_r2, 1)); cat("\n")

plot_list <- list()

dimension_colors <- c(
  "Economy"    = "orange",
  "Environment" = "green",
  "Population"  = "blue"
)

for (j in seq_along(es_vars)) {
  
  es_code <- es_vars[j]
  es_lbl  <- es_names[j]             
  imp_df <- data.frame(
    Variable   = urban_vars,
    Gain       = final_gain[, es_code],
    Dimension  = urban_dimensions$Dimension[match(urban_vars, urban_dimensions$Variable)],
    stringsAsFactors = FALSE
  ) |>
    mutate(
      Variable = fct_reorder(Variable, Gain)
    ) |>
    arrange(desc(Gain))
  
  p <- ggplot(imp_df, aes(x = Variable, y = Gain, fill = Dimension)) +
    geom_col(width = 0.82) +
    scale_fill_manual(values = dimension_colors, drop = FALSE) +
    coord_flip() +
    labs(
      title    = es_lbl,
      subtitle = sprintf("R² = %.1f%%", mean_r2[es_code]),
      x        = NULL,
      y        = "Gain (importance)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(hjust = 0.5, face = "bold", size = 11),
      plot.subtitle = element_text(hjust = 0.5, size = 9.5, color = "gray50"),
      axis.text.y   = element_text(size = 9),
      legend.position = "none",
      panel.grid.major.y = element_blank()
    )
  
  plot_list[[j]] <- p
}


combined_fig <- wrap_plots(plot_list, ncol = 3, guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.key.height = unit(0.6, "lines")
  )

combined_fig <- combined_fig +
  plot_annotation(
    title = "Fig. 7. Importance of new-type urbanization indicators on ecosystem services (XGBoost)",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))
  )

print(combined_fig)

ggsave(file.path(out_dir, "Fig7_XGBoost_Importance_2024style.png"),
       combined_fig, width = 15.5, height = 11.2, dpi = 340, bg = "white")

ggsave(file.path(out_dir, "Fig7_XGBoost_Importance_2024style.svg"),
       combined_fig, width = 15.5, height = 11.2)
   
