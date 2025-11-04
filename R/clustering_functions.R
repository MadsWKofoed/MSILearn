# R/clustering_functions.R

# Log transformation helper
log_transform <- function(feature_matrix) {
  log_matrix <- log(feature_matrix)
  log_matrix[is.infinite(log_matrix) | is.na(log_matrix)] <- 0
  log_matrix
}

# K-means clustering
run_kmeans <- function(full_df, k = 3, log_scale = FALSE) {
  feature_matrix <- as.matrix(full_df[, grep("^mz_", colnames(full_df))])
  
  if (log_scale) {
    feature_matrix <- log_transform(feature_matrix) # Only transform the matrix used for calculation
  }
  
  km <- kmeans(feature_matrix, centers = k) # Clustering uses transformed data
  full_df$cluster <- km$cluster # Assign clusters back to ORIGINAL dataframe
  full_df
}

# Hierarchical clustering
run_hclust <- function(full_df, k = 3, log_scale = FALSE) {
  feature_matrix <- as.matrix(full_df[, grep("^mz_", colnames(full_df))])
  
  if (log_scale) {
    feature_matrix <- log_transform(feature_matrix)
  }
  
  d <- dist(feature_matrix)
  hc <- hclust(d, method = "ward.D2")
  clusters <- cutree(hc, k = k)
  full_df$cluster <- clusters
  full_df
}