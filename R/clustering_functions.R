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
  
  # Collect clustering results
  clust_alg_res <- data.frame(
    cluster = vsclust_alg[["cluster"]],
    isClusterMember = rowMaxs(vsclust_alg[["membership"]]) > minMem,
    maxMembership = rowMaxs(vsclust_alg[["membership"]]),
    vsclust_alg$membership
  )
  
  # Correct cluster labels: assign "No_cluster" if membership < minMem
  clust_alg_res$cluster_corrected <- ifelse(
    clust_alg_res$isClusterMember,
    as.character(clust_alg_res$cluster),
    "No_cluster"
  )
  
  # Add cluster labels back to original data
  full_df$cluster <- clust_alg_res$cluster_corrected
  
  full_df
}



