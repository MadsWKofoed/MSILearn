# R/clustering_functions.R

# K-means clustering
run_kmeans <- function(full_df, k = 3) {
  feature_matrix <- as.matrix(full_df[, grep("^mz_", colnames(full_df))])
  km <- kmeans(feature_matrix, centers = k)
  full_df$cluster <- km$cluster
  full_df
}

# Hierarchical clustering
run_hclust <- function(full_df, k = 3) {
  feature_matrix <- as.matrix(full_df[, grep("^mz_", colnames(full_df))])
  d <- dist(feature_matrix)
  hc <- hclust(d, method = "ward.D2")
  clusters <- cutree(hc, k = k)
  full_df$cluster <- clusters
  full_df
}