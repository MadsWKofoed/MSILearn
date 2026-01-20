library(Cardinal)
library(matter)
library(dplyr)
library(vsclust)
library(matrixStats)
library(plotly)
library(RColorBrewer)
library(bestNormalize)
library(purrr)

# =============================== Helpers ====================================
# Normalize pixels
normalize_pixels <- function(
    data,
    signal_cols,
    spatial_cols = c("x", "y"),
    method = c("tic", "median", "rms"),
    na.rm = TRUE
) {
  method <- match.arg(method)
  
  # Split data
  X_signal <- as.matrix(data[, signal_cols, drop = FALSE])
  X_spatial <- data[, intersect(spatial_cols, names(data)), drop = FALSE]
  
  # Compute per-pixel denominator
  denom <- switch(
    method,
    tic    = rowSums(X_signal, na.rm = na.rm),
    median = apply(X_signal, 1, median, na.rm = na.rm),
    rms    = sqrt(rowMeans(X_signal^2, na.rm = na.rm))
  )
  
  # Avoid division by zero
  denom[denom == 0] <- NA
  
  # Normalize per pixel (row-wise)
  X_signal_norm <- sweep(X_signal, 1, denom, "/")
  
  # Recombine (signal normalized, spatial untouched)
  out <- cbind(X_spatial, as.data.frame(X_signal_norm))
  
  return(out)
}

# Score clustering accuracies
score_clusterings <- function(df,
                              gt_col = "ground_truth",
                              cluster_cols,
                              no_label = "No_cluster") {
  
  gt_sym <- rlang::sym(gt_col)
  
  map_dfr(cluster_cols, function(cl_col) {
    cl_sym <- rlang::sym(cl_col)
    
    # Brug kun pixels der faktisk er i en cluster (ikke No_cluster)
    df_in <- df %>%
      filter(!is.na(!!cl_sym), !!cl_sym != no_label, !is.na(!!gt_sym))
    
    # Majority vote mapping: cluster -> ground truth
    cluster_map <- df_in %>%
      dplyr::count(cluster = !!cl_sym, ground_truth = !!gt_sym, name = "n") %>%
      group_by(cluster) %>%
      slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(cluster, mapped_truth = ground_truth)
    
    # Evaluer kun på df_in
    scored <- df_in %>%
      mutate(cluster = !!cl_sym,
             ground_truth = !!gt_sym) %>%
      left_join(cluster_map, by = "cluster") %>%
      mutate(correct = mapped_truth == ground_truth)
    
    tibble(
      clustering = cl_col,
      accuracy = mean(scored$correct, na.rm = TRUE),
      n_used = nrow(scored),
      n_total = nrow(df),
      n_excluded = nrow(df) - nrow(scored)
    )
  })
}


bp <- parallel::detectCores() - 2
setCardinalParallel(workers = bp)


temp_imzml <- "kidney_and_tumor/PROCESSED_kidney.imzML"

temp_imzml <- "kidney_and_tumor/PROCESSED_tumor.imzML"



msi_data <- readImzML(temp_imzml, memory = FALSE, check = FALSE,
                      mass.range = NULL, resolution = 10, units = c("ppm"),
                      guess.max = 1000L, as = "auto", parse.only=FALSE,
                      verbose = getCardinalVerbose(), chunkopts = list(),
                      BPPARAM = bpparam())

# Make data frame
msi_df <- make_msi_dataframe(msi_data)


# Remove RunName column
msi_df_clust <- msi_df[, !names(msi_df) %in% "runNames"]


# Find correlations of each pixel to neighboring pixels
cor_data <- msi_df_clust
cor_data$avg_corr_neighbors <- compute_neighbor_cor_8(cor_data, x_col = "x", y_col = "y", cores = 30)
cor_data$inv_cor <- 1 - cor_data$avg_corr_neighbors

# Remove pixels with no neighbors (if they exist)
cor_data <- cor_data[!is.na(cor_data$avg_corr_neighbors), ]

key_msi <- paste(msi_df$x, msi_df$y)
key_cor <- paste(cor_data$x, cor_data$y)

msi_df <- msi_df[key_msi %in% key_cor, ]

msi_df_gt <- msi_df


# Make inverse correlations more nomal distributed
yj <- yeojohnson(cor_data$inv_cor)

nclust <- 3

# Find individual fuzzifiers by iterating until enough fuzzifiers are below threshold
res <- tune_fuzz_grid_early(
  x            = yj$x.t,
  dims         = dim(cor_data),
  NClust       = nclust,
  new_max_range = c(1, 10),
  new_min = 0,
  step         = 0.1,
  target_frac  = 0.6,
  m_threshold  = 1.5
)


# Normalize the data to be clustered
signal_cols <- setdiff(
  names(cor_data),
  c("x", "y", "avg_corr_neighbors")
)

cor_data_norm_xy <- normalize_pixels(
  data = cor_data,
  signal_cols = signal_cols,
  spatial_cols = c("x", "y"),
  method = "rms"
)

# Find median to scale coordinates with
signal_scale <- median(
  abs(as.matrix(cor_data_norm_xy[, signal_cols])),
  na.rm = TRUE
)

# Scale the coordinates using median of the normalized intensities
cor_data_norm_xy$x <- (cor_data_norm_xy$x / max(cor_data_norm_xy$x)) * signal_scale *2
cor_data_norm_xy$y <- (cor_data_norm_xy$y / max(cor_data_norm_xy$y)) * signal_scale *2

# Data frame for clustering without x and y coordinates
cor_data_norm <- cor_data_norm_xy[, !names(cor_data_norm_xy) %in% c("x", "y")]


#================== Apply MSIClust with x and y coordinates ====================================


# Apply VSClust with individual fuzzifiers
msiclust_alg_xy <- vsclust_algorithm(cor_data_norm_xy,
                                 centers = nclust,
                                 iterMax = 100,
                                 m = res$best_fuzz$m)



minMem <- 0.5

msiclust_alg_res_xy <- data.frame(cluster = msiclust_alg_xy[["cluster"]],
                            isClusterMember =  rowMaxs(msiclust_alg_xy[["membership"]]) > minMem,
                            maxMembership = rowMaxs(msiclust_alg_xy[["membership"]]),
                            msiclust_alg_xy$membership)

# Map No_cluster to all those pixels with maximum membership below minMem
msiclust_alg_res_xy$cluster_corrected <- ifelse(
  msiclust_alg_res_xy$isClusterMember,
  as.character(msiclust_alg_res_xy$cluster),
  "No_cluster"
)


msi_df_gt$MSIClust_xy_cluster <- msiclust_alg_res_xy$cluster_corrected



#======================= MSIClust without x and y coordinates =================================


# Apply VSClust with individual fuzzifiers
msiclust_alg <- vsclust_algorithm(cor_data_norm,
                                 centers = nclust,
                                 iterMax = 100,
                                 m = res$best_fuzz$m)


msiclust_alg_res <- data.frame(cluster = msiclust_alg[["cluster"]],
                            isClusterMember =  rowMaxs(msiclust_alg[["membership"]]) > minMem,
                            maxMembership = rowMaxs(msiclust_alg[["membership"]]),
                            msiclust_alg$membership)

# Map No_cluster to all those pixels with maximum membership below minMem
msiclust_alg_res$cluster_corrected <- ifelse(
  msiclust_alg_res$isClusterMember,
  as.character(msiclust_alg_res$cluster),
  "No_cluster"
)


msi_df_gt$MSIClust_cluster <- msiclust_alg_res$cluster_corrected


#======================= VSClust with x and y coordinates =================================

# Find one collective fuzzifier based on the data
fuzz <- determine_fuzz(dims = dim(cor_data_norm_xy), NClust = nclust, Sds = 0.9)


vsclust_alg_xy <- vsclust_algorithm(cor_data_norm_xy,
                                 centers = nclust,
                                 iterMax = 100,
                                 m = fuzz$m)


vsclust_alg_res_xy <- data.frame(cluster = vsclust_alg_xy[["cluster"]],
                            isClusterMember =  rowMaxs(vsclust_alg_xy[["membership"]]) > minMem,
                            maxMembership = rowMaxs(vsclust_alg_xy[["membership"]]),
                            vsclust_alg_xy$membership)

vsclust_alg_res_xy$cluster_corrected <- ifelse(
  vsclust_alg_res_xy$isClusterMember,
  as.character(vsclust_alg_res_xy$cluster),
  "No_cluster"
)


msi_df_gt$VSClust_xy_cluster <- vsclust_alg_res_xy$cluster_corrected



#======================= VSClust without x and y coordinates =================================

# Find one collective fuzzifier based on the data
fuzz <- determine_fuzz(dims = dim(cor_data_norm), NClust = nclust, Sds = 0.9)


vsclust_alg <- vsclust_algorithm(cor_data_norm,
                                 centers = nclust,
                                 iterMax = 100,
                                 m = fuzz$m)


vsclust_alg_res <- data.frame(cluster = vsclust_alg[["cluster"]],
                              isClusterMember =  rowMaxs(vsclust_alg[["membership"]]) > minMem,
                              maxMembership = rowMaxs(vsclust_alg[["membership"]]),
                              vsclust_alg$membership)

vsclust_alg_res$cluster_corrected <- ifelse(
  vsclust_alg_res$isClusterMember,
  as.character(vsclust_alg_res$cluster),
  "No_cluster"
)


msi_df_gt$VSClust_cluster <- vsclust_alg_res$cluster_corrected


#=========================== Kmeans with x and y coordinates =================================


km_xy <- kmeans(cor_data_norm_xy, centers = nclust, nstart = 25)

msi_df_gt$Kmeans_xy_cluster <- km_xy$cluster


#======================== Kmeans without x and y coordinates =================================

km <- kmeans(cor_data_norm, centers = nclust, nstart = 25)

msi_df_gt$Kmeans_cluster <- km$cluster





#================================== Accuracy of clusterings ======================================


necrosis_coords <- read.csv("kidney_and_tumor/tumor_necrosis_roi_points_2025-12-27.csv", header = TRUE)
necrosis_coords <- necrosis_coords[, c("x", "y")]
healthy_coords <- read.csv("kidney_and_tumor/tumor_healthy_roi_points_2025-12-27.csv", header = TRUE)
healthy_coords <- healthy_coords[, c("x", "y")]
# reamining coords are tumor

# Putting in the ground truths defined from H&E image
msi_df_gt <- msi_df_gt %>%
  mutate(
    key = paste(x, y),
    ground_truth = case_when(
      key %in% paste(necrosis_coords$x, necrosis_coords$y) ~ "necrosis",
      key %in% paste(healthy_coords$x,  healthy_coords$y)  ~ "healthy",
      TRUE                                                  ~ "tumor"
    )
  ) %>%
  select(-key)



# Generating cluster map - Which cluster corresponds to which class
table(msi_df_gt$ground_truth, msi_df_gt$MSIClust_xy_cluster)




cluster_cols <- c(
  "MSIClust_xy_cluster",
  "MSIClust_cluster",
  "VSClust_xy_cluster",
  "VSClust_cluster",
  "Kmeans_xy_cluster",
  "Kmeans_cluster"
)

accuracy_table <- score_clusterings(
  df = msi_df_gt,
  gt_col = "ground_truth",
  cluster_cols = cluster_cols,
  no_label = "No_cluster"
)

accuracy_table






cluster_map <- msi_df_gt %>%
  dplyr::count(MSIClust_xy_cluster, ground_truth) %>%
  group_by(MSIClust_xy_cluster) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(MSIClust_xy_cluster, mapped_truth = ground_truth)

cluster_map


# Finding accuracy
msi_df_eval <- msi_df_gt %>%
  left_join(cluster_map, by = "MSIClust_xy_cluster")

accuracy <- mean(msi_df_eval$ground_truth == msi_df_eval$mapped_truth)
accuracy





