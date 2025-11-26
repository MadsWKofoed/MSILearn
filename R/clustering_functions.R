# R/clustering_functions.R

# Normalization functions
normalize_data <- function(feature_matrix, method = "none") {
  if (method == "none") {
    return(feature_matrix)
  }
  
  normalized <- switch(method,
    "log" = {
      log_matrix <- log1p(feature_matrix)
      log_matrix[is.na(log_matrix)] <- 0
      log_matrix
    },
    "scale" = {
      scaled_matrix <- scale(feature_matrix)
      scaled_matrix[is.infinite(scaled_matrix) | is.na(scaled_matrix)] <- 0
      scaled_matrix
    },
    feature_matrix  # fallback
  )
  
  normalized
}

# K-means clustering
run_kmeans <- function(full_df, k = 3, normalize_method = "none") {
  feature_matrix <- as.matrix(full_df[, grep("^mz_", colnames(full_df))])
  
  # Apply normalization
  feature_matrix <- normalize_data(feature_matrix, normalize_method)
  
  # Run clustering
  km <- kmeans(feature_matrix, centers = k, nstart = 25)
  
  # Assign clusters to original dataframe
  full_df$cluster <- km$cluster
  full_df
}

# VSClust clustering
run_vsclust <- function(full_df, k = 3, normalize_method = "scale", 
                        Sds = 1.3, minMem = 0.5) {
  
  # Remove RunName column (keep x, y coordinates)
  msi_df_clust <- full_df[, !names(full_df) %in% "runNames"]
  
  # Apply normalization
  msi_df_clust <- normalize_data(msi_df_clust, normalize_method)
  
  # Determine fuzziness parameter
  fuzz <- determine_fuzz(
    dims = dim(msi_df_clust),
    NClust = k,
    Sds = Sds
  )
  
  # Run vsclust algorithm
  vsclust_alg <- vsclust_algorithm(
    msi_df_clust,
    centers = k,
    iterMax = 100,
    m = fuzz$m
  )
  
  # Store membership matrix in original dataframe
  membership_cols <- paste0("membership_", seq_len(k))
  full_df[, membership_cols] <- vsclust_alg$membership
  full_df$max_membership <- rowMaxs(vsclust_alg$membership)
  full_df$raw_cluster <- vsclust_alg$cluster
  
  # Apply minMem threshold to get corrected clusters
  full_df <- apply_minmem_threshold(full_df, minMem)
  
  full_df
}

# Helper function to apply minMem threshold (can be called independently)
apply_minmem_threshold <- function(df, minMem = 0.5) {
  # Determine cluster based on membership threshold
  cluster_corrected <- ifelse(
    df$max_membership > minMem,
    as.character(df$raw_cluster),
    "No_cluster"
  )
  
  # Renumber remaining clusters to be consecutive
  valid_clusters <- cluster_corrected[cluster_corrected != "No_cluster"]
  
  if (length(valid_clusters) > 0) {
    existing_nums <- sort(unique(as.numeric(valid_clusters)))
    new_nums <- seq_along(existing_nums)
    cluster_map <- setNames(as.character(new_nums), as.character(existing_nums))
    
    cluster_corrected <- ifelse(
      cluster_corrected == "No_cluster",
      "No_cluster",
      cluster_map[cluster_corrected]
    )
  }
  
  df$cluster <- cluster_corrected
  df
}


