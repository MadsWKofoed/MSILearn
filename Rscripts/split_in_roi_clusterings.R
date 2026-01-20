library(ggplot2)
library(dplyr)
library(purrr)

setwd("/data/makof21/MSI_database/Data")

standardize_roi_class <- function(roi_vec, strict = FALSE,
                                  squamous_as = c("Healthy", "Squamous", "NA")) {
  squamous_as <- match.arg(squamous_as)
  
  x <- trimws(roi_vec)
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x) & nzchar(x)
  xl <- tolower(x[ok])
  
  # fjern prefix som "a_" / "b_"
  xl2 <- sub("^[a-z]_+", "", xl)
  
  # klasse-token før første "_"
  cls <- sub("_.*$", "", xl2)
  
  mapped <- rep(NA_character_, length(cls))
  mapped[cls %in% c("healthy", "h")] <- "Healthy"
  mapped[cls %in% c("hg", "highgrade", "high")] <- "HighGrade"
  mapped[cls %in% c("lg", "lgd", "lowgrade", "low")] <- "LowGrade"
  
  if (squamous_as == "Healthy") {
    mapped[cls == "squamous"] <- "Healthy"
  } else if (squamous_as == "Squamous") {
    mapped[cls == "squamous"] <- "Squamous"
  } else {
    mapped[cls == "squamous"] <- NA_character_
  }
  
  out[ok] <- mapped
  
  if (isTRUE(strict)) {
    unmapped <- unique(x[ok][is.na(mapped)])
    if (length(unmapped) > 0) {
      warning("Unmapped ROI names: ", paste(unmapped, collapse = ", "))
    }
  }
  out
}





# --- plot helper (pixels + rois) ---
plot_alignment_region <- function(spots_region, rois, title,
                                  color_by = NULL, roi_buffer = 0,
                                  show_rois = TRUE, flip_y = FALSE) {
  
  rois_r <- if (show_rois) filter_rois_to_spots(rois, spots_region, buffer = roi_buffer, fallback_all = TRUE) else rois[0,]
  
  p <- ggplot2::ggplot()
  
  if (show_rois && nrow(rois_r) > 0) {
    p <- p + ggplot2::geom_polygon(
      data = rois_r,
      ggplot2::aes(x = x, y = y, group = roi),
      fill = NA, color = "red", linewidth = 0.25
    )
  }
  
  if (!is.null(color_by) && color_by %in% colnames(spots_region)) {
    p <- p + ggplot2::geom_point(
      data = spots_region,
      ggplot2::aes(x = mx, y = my, color = .data[[color_by]]),
      shape = 15, size = 1.2, alpha = 1, stroke = 0
    ) + ggplot2::labs(color = color_by)
  } else {
    p <- p + ggplot2::geom_point(
      data = spots_region,
      ggplot2::aes(x = mx, y = my),
      shape = 15, size = 1.2, alpha = 1, stroke = 0
    )
  }
  
  p <- p +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = title, x = "mx", y = "my")
  
  if (flip_y) p <- p + ggplot2::scale_y_reverse()
  p
}



filter_rois_to_spots <- function(rois, spots_df, buffer = 0, fallback_all = TRUE) {
  xmin <- min(spots_df$mx, na.rm = TRUE) - buffer
  xmax <- max(spots_df$mx, na.rm = TRUE) + buffer
  ymin <- min(spots_df$my, na.rm = TRUE) - buffer
  ymax <- max(spots_df$my, na.rm = TRUE) + buffer
  
  rois_bbox <- rois[
    rois$x >= xmin & rois$x <= xmax &
      rois$y >= ymin & rois$y <= ymax,
    , drop = FALSE
  ]
  
  if (nrow(rois_bbox) == 0 && fallback_all) return(rois)
  if (nrow(rois_bbox) == 0) return(rois_bbox)
  
  keep <- unique(rois_bbox$roi)
  rois[rois$roi %in% keep, , drop = FALSE]
}


# Accuracy of clusterings
cluster_eval_by_region <- function(df,
                                   region_col = "region",
                                   gt_col = "ground_truth",
                                   cluster_cols,
                                   no_cluster = "No_cluster") {
  
  if (!requireNamespace("clue", quietly = TRUE)) {
    stop("Package 'clue' is required. Install with: install.packages('clue')")
  }
  
  regions <- sort(unique(df[[region_col]]))
  
  results <- setNames(vector("list", length(regions)), regions)
  
  for (reg in regions) {
    
    df_reg <- df[df[[region_col]] == reg, , drop = FALSE]
    
    acc_list <- list()
    map_list <- list()
    n_list   <- list()
    
    for (cl_col in cluster_cols) {
      
      cl_vec <- df_reg[[cl_col]]
      gt_vec <- df_reg[[gt_col]]
      
      keep <- !is.na(cl_vec) & cl_vec != no_cluster & !is.na(gt_vec)
      df_f <- df_reg[keep, , drop = FALSE]
      
      if (nrow(df_f) == 0) {
        acc_list[[cl_col]] <- NA_real_
        map_list[[cl_col]] <- tibble()
        n_list[[cl_col]]   <- 0
        next
      }
      
      cl_f <- df_f[[cl_col]]
      gt_f <- df_f[[gt_col]]
      
      mat <- as.matrix(table(cl_f, gt_f))
      
      if (all(mat == 0)) {
        acc_list[[cl_col]] <- NA_real_
        map_list[[cl_col]] <- tibble()
        n_list[[cl_col]]   <- length(gt_f)
        next
      }
      
      # Hungarian: maximize overlap
      assignment <- clue::solve_LSAP(mat, maximum = TRUE)
      
      clusters <- rownames(mat)
      truths   <- colnames(mat)
      
      mapping <- tibble(
        cluster = clusters,
        mapped_truth = truths[as.integer(assignment)],
        overlap = mat[cbind(seq_along(clusters), assignment)]
      )
      
      mapped <- mapping$mapped_truth[ match(cl_f, mapping$cluster) ]
      
      acc_list[[cl_col]] <- mean(mapped == gt_f)
      map_list[[cl_col]] <- mapping
      n_list[[cl_col]]   <- length(gt_f)
    }
    
    results[[as.character(reg)]] <- list(
      accuracy = tibble(
        method = names(acc_list),
        accuracy = unlist(acc_list)
      ),
      mapping = map_list,
      n_pixels = tibble(
        method = names(n_list),
        n = unlist(n_list)
      )
    )
  }
  
  results
}



# F1-helper function
f1_scores <- function(truth, pred) {
  labs <- sort(unique(truth))
  
  per_class <- lapply(labs, function(k) {
    tp <- sum(truth == k & pred == k, na.rm = TRUE)
    fp <- sum(truth != k & pred == k, na.rm = TRUE)
    fn <- sum(truth == k & pred != k, na.rm = TRUE)
    
    precision <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
    recall    <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
    f1 <- if (is.na(precision) || is.na(recall) || (precision + recall) == 0) NA_real_
    else 2 * precision * recall / (precision + recall)
    
    support <- sum(truth == k, na.rm = TRUE)
    
    data.frame(class = k, tp = tp, fp = fp, fn = fn,
               precision = precision, recall = recall, f1 = f1,
               support = support)
  })
  
  per_class <- do.call(rbind, per_class)
  macro_f1 <- mean(per_class$f1, na.rm = TRUE)
  weighted_f1 <- if (sum(per_class$support) == 0) NA_real_ else
    sum(per_class$f1 * per_class$support, na.rm = TRUE) / sum(per_class$support)
  
  list(per_class = per_class, macro_f1 = macro_f1, weighted_f1 = weighted_f1)
}



# F1-scoring of clusterings
cluster_f1 <- function(df,
                       region_col = "region",
                       gt_col = "ground_truth",
                       cluster_cols,
                       no_cluster = "No_cluster") {
  
  if (!requireNamespace("clue", quietly = TRUE)) {
    stop("Package 'clue' is required. Install with: install.packages('clue')")
  }
  
  regions <- sort(unique(df[[region_col]]))
  results <- setNames(vector("list", length(regions)), regions)
  
  for (reg in regions) {
    df_reg <- df[df[[region_col]] == reg, , drop = FALSE]
    
    scores_list <- list()
    mapping_list <- list()
    perclass_list <- list()
    
    for (cl_col in cluster_cols) {
      cl_vec <- df_reg[[cl_col]]
      gt_vec <- df_reg[[gt_col]]
      
      keep <- !is.na(cl_vec) & cl_vec != no_cluster & !is.na(gt_vec)
      df_f <- df_reg[keep, , drop = FALSE]
      
      if (nrow(df_f) == 0) {
        scores_list[[cl_col]] <- tibble(method = cl_col, accuracy = NA_real_, macro_f1 = NA_real_, weighted_f1 = NA_real_, n = 0)
        mapping_list[[cl_col]] <- tibble()
        perclass_list[[cl_col]] <- tibble()
        next
      }
      
      cl_f <- df_f[[cl_col]]
      gt_f <- df_f[[gt_col]]
      
      mat <- as.matrix(table(cl_f, gt_f))  # rows=clusters, cols=truth
      
      # kræver rows <= cols (ellers: NA)
      if (nrow(mat) > ncol(mat)) {
        scores_list[[cl_col]] <- tibble(method = cl_col, accuracy = NA_real_, macro_f1 = NA_real_, weighted_f1 = NA_real_, n = length(gt_f))
        mapping_list[[cl_col]] <- tibble()
        perclass_list[[cl_col]] <- tibble()
        next
      }
      
      assignment <- clue::solve_LSAP(mat, maximum = TRUE)
      
      clusters <- rownames(mat)
      truths <- colnames(mat)
      
      mapping <- tibble(
        cluster = clusters,
        mapped_truth = truths[as.integer(assignment)],
        overlap = mat[cbind(seq_along(clusters), as.integer(assignment))]
      )
      
      pred <- mapping$mapped_truth[match(cl_f, mapping$cluster)]
      
      acc <- mean(pred == gt_f)
      f1 <- f1_scores(gt_f, pred)
      
      scores_list[[cl_col]] <- tibble(
        method = cl_col,
        accuracy = acc,
        macro_f1 = f1$macro_f1,
        weighted_f1 = f1$weighted_f1,
        n = length(gt_f)
      )
      
      mapping_list[[cl_col]] <- mapping
      perclass_list[[cl_col]] <- as_tibble(f1$per_class)
    }
    
    results[[as.character(reg)]] <- list(
      scores = bind_rows(scores_list),
      mapping = mapping_list,
      per_class = perclass_list
    )
  }
  
  results
}

process_slide_by_region_with_plots <- function(
    imzml_path,
    spotlist_path,
    mis_path,
    mz_ref_path,
    mode = c("per_region", "combined"),   # NEW
    remove_regions = character(0),
    include_regions = NULL,
    snr = 3,
    tolerance = 0.5,
    units_align = "mz",
    resolution = 10,
    read_units = c("ppm"),
    include_boundary = TRUE,
    squamous_as = "Squamous",
    split_after = TRUE,
    verbose = TRUE,
    color_pixels_by = "roi_class_plot",    # eller NULL
    roi_buffer = 0
) {
  mode <- match.arg(mode)
  
  # ---------- read MSI ----------
  if (verbose) message("Reading imzML: ", imzml_path)
  msi_data <- Cardinal::readImzML(
    imzml_path,
    memory = FALSE, check = FALSE,
    mass.range = NULL, resolution = resolution, units = read_units,
    guess.max = 1000L, as = "auto", parse.only = FALSE,
    verbose = Cardinal::getCardinalVerbose(),
    chunkopts = list(),
    BPPARAM = BiocParallel::bpparam()
  )
  
  # ---------- ROI assignment (dftest) ----------
  if (verbose) message("Assigning ROIs from .mis")
  dftest <- assign_roi_from_mis(
    spotlist_path = spotlist_path,
    mis_path = mis_path,
    include_boundary = include_boundary
  )
  
  dftest$roi_class <- standardize_roi_class(dftest$roi, strict = FALSE, squamous_as = squamous_as)
  dftest$roi_class_plot <- ifelse(is.na(dftest$roi_class), "Unassigned", dftest$roi_class)
  
  # ROI polygoner fra .mis (samme som før)
  doc <- xml2::read_xml(mis_path)
  roi_nodes <- xml2::xml_find_all(doc, ".//ROI")
  rois <- lapply(roi_nodes, function(r) {
    nm <- xml2::xml_attr(r, "Name")
    pts <- xml2::xml_text(xml2::xml_find_all(r, ".//Point"))
    xy <- do.call(rbind, strsplit(pts, ",")) |> apply(2, as.numeric)
    data.frame(roi = nm, x = xy[,1], y = xy[,2])
  }) |> dplyr::bind_rows()
  
  # ---------- read spotlist (region + gx/gy) ----------
  spots <- read.table(
    spotlist_path,
    comment.char = "#",
    col.names = c("stage_x","stage_y","spot_name","region"),
    stringsAsFactors = FALSE
  )
  spots$region <- sprintf("%02d", as.integer(spots$region))
  
  m <- regexec("X(\\d+)Y(\\d+)", spots$spot_name)
  hit <- regmatches(spots$spot_name, m)
  ok <- lengths(hit) == 3
  spots <- spots[ok, , drop = FALSE]
  hit <- hit[ok]
  spots$gx <- as.integer(vapply(hit, `[`, "", 2))
  spots$gy <- as.integer(vapply(hit, `[`, "", 3))
  
  # map mx/my + roi_class_plot fra dftest via gx/gy
  key_dft <- paste(dftest$gx, dftest$gy)
  key_sp  <- paste(spots$gx, spots$gy)
  
  spots$mx <- unname(setNames(dftest$mx, key_dft)[key_sp])
  spots$my <- unname(setNames(dftest$my, key_dft)[key_sp])
  spots$roi_class_plot <- unname(setNames(dftest$roi_class_plot, key_dft)[key_sp])
  spots$roi_class_plot[is.na(spots$roi_class_plot)] <- "Unassigned"
  
  # ---------- regions to use ----------
  regs <- sort(unique(spots$region))
  if (!is.null(include_regions)) {
    include_regions <- sprintf("%02d", as.integer(include_regions))
    regs <- regs[regs %in% include_regions]
  }
  if (length(remove_regions) > 0) {
    remove_regions <- sprintf("%02d", as.integer(remove_regions))
    regs <- regs[!(regs %in% remove_regions)]
  }
  if (verbose) message("Regions included: ", paste(regs, collapse = ", "))
  
  # ---------- set region into pixelData for subsetPixels ----------
  xy <- as.data.frame(Cardinal::coord(msi_data))
  colnames(xy) <- c("gx","gy")
  key_img <- paste(xy$gx, xy$gy)
  
  region_map <- setNames(spots$region, key_sp)
  Cardinal::pixelData(msi_data)$region <- unname(region_map[key_img])
  
  # ground-truth lookup for df join
  roi_map <- setNames(dftest$roi_class_plot, key_dft)
  
  # mz ref
  mz_ref_tbl <- read.table(mz_ref_path, header = TRUE)
  mz_ref <- mz_ref_tbl$Centroid
  
  
  # ---------- output ----------
  dfs <- list()
  plots <- list()
  
  # ---------- run per-region or combined ----------
  if (mode == "per_region") {
    
    dfs <- setNames(vector("list", length(regs)), regs)
    plots <- setNames(vector("list", length(regs)), regs)
    
    for (r in regs) {
      if (verbose) message("Region ", r, ": plot + process")
      
      msi_r <- Cardinal::subsetPixels(msi_data, region == r)
      
      control_mean <- Cardinal::summarizeFeatures(msi_r, "mean")
      control_ref <- control_mean %>%
        Cardinal::peakPick(SNR = snr) %>%
        Cardinal::peakAlign(ref = mz_ref, tolerance = tolerance, units = units_align) %>%
        Cardinal::subsetFeatures() %>%
        Cardinal::process()
      
      msi_binned <- Cardinal::bin(
        msi_r,
        ref = Cardinal::mz(control_ref),
        tolerance = tolerance,
        units = units_align,
        BPPARAM = BiocParallel::bpparam()
      ) %>% Cardinal::process()
      
      msi_df <- make_msi_dataframe(msi_binned)
      if (!all(c("x","y") %in% colnames(msi_df))) {
        stop("make_msi_dataframe skal returnere kolonnerne x og y (grid coords).")
      }
      key_df <- paste(msi_df$x, msi_df$y)
      msi_df$ground_truth <- unname(roi_map[key_df])
      msi_df$ground_truth[is.na(msi_df$ground_truth)] <- "Unassigned"
      
      dfs[[r]] <- msi_df
    }
    
  } else {  # mode == "combined"
    
    if (verbose) message("Combined mode: plot + process (all included regions)")
    
    sp_all <- subset(spots, region %in% regs & !is.na(mx) & !is.na(my))
    plots[["ALL"]] <- plot_alignment_region(
      spots_region = sp_all,
      rois = rois,
      title = paste("Alignment – ALL regions (", paste(regs, collapse = ","), ")", sep=""),
      color_by = color_pixels_by,
      roi_buffer = roi_buffer
    )
    
    msi_all <- Cardinal::subsetPixels(msi_data, region %in% regs)
    
    control_mean <- Cardinal::summarizeFeatures(msi_all, "mean")
    control_ref <- control_mean %>%
      Cardinal::peakPick(SNR = snr) %>%
      Cardinal::peakAlign(ref = mz_ref, tolerance = tolerance, units = units_align) %>%
      Cardinal::subsetFeatures() %>%
      Cardinal::process()
    
    msi_binned <- Cardinal::bin(
      msi_all,
      ref = Cardinal::mz(control_ref),
      tolerance = tolerance,
      units = units_align,
      BPPARAM = BiocParallel::bpparam()
    ) %>% Cardinal::process()
    
    msi_df <- make_msi_dataframe(msi_binned)
    if (!all(c("x","y") %in% colnames(msi_df))) {
      stop("make_msi_dataframe skal returnere kolonnerne x og y (grid coords).")
    }
    
    
    # region pr pixel fra pixelData(msi_all)$region
    msi_df$region <- as.character(Cardinal::pixelData(msi_all)$region)
    
    key_df <- paste(msi_df$x, msi_df$y)
    msi_df$ground_truth <- unname(roi_map[key_df])
    msi_df$ground_truth[is.na(msi_df$ground_truth)] <- "Unassigned"
    
    dfs_by_region <- split(msi_df, msi_df$region)
    dfs_by_region <- dfs_by_region[names(dfs_by_region) %in% regs]
    
    dfs[["ALL"]] <- msi_df
    
    # ALL plot (med større buffer så vi helt sikkert får ROIs med)
    plots[["ALL"]] <- plot_alignment_region(
      spots_region = sp_all,
      rois = rois,
      title = paste0("Alignment – ALL regions (", paste(regs, collapse=","), ")"),
      color_by = color_pixels_by,
      roi_buffer = max(roi_buffer, 200),   # <- vigtig for ALL
      show_rois = TRUE,
      flip_y = TRUE
    )
    
    plots_by_region <- lapply(regs, function(r) {
      sp_r <- subset(spots, region == r & !is.na(mx) & !is.na(my))
      plot_alignment_region(
        spots_region = sp_r,
        rois = rois,
        title = paste("Alignment – region", r),
        color_by = color_pixels_by,
        roi_buffer = roi_buffer,
        show_rois = TRUE,
        flip_y = TRUE
      )
    })
    names(plots_by_region) <- regs
  }
  
  list(
    dfs = dfs,
    dfs_by_region = if (mode == "combined" && split_after) dfs_by_region else NULL,
    plots = plots,
    plots_by_region = if (mode == "combined") plots_by_region else NULL,
    dftest = dftest,
    rois = rois,
    spots = spots,
    included_regions = regs,
    mode = mode
  )
}



region_res <- process_slide_by_region_with_plots(
  imzml_path    = "new_data/29042018_slideC1/29042018_slideC1.imzML",
  spotlist_path = "new_data/29042018_slideC1/29042018_slideC1_SPOTLIST.txt",
  mis_path      = "new_data/29042018_slideC1/29042018_slideC1.mis",
  mz_ref_path   = "new_data/On-tissue_peaklist.txt",
  remove_regions = c("06"),
  mode = "combined",
  split_after = TRUE,
  snr = 3,
  tolerance = 0.5
)

# Plot for region 01
region_res$plots[["ALL"]]
region_res[["plots_by_region"]][["01"]]

# Dataframe for region 01
msi_df <- region_res[["dfs_by_region"]][["01"]]
msi_df <- region_res[["dfs"]][["ALL"]]


# ===================================== Clusterings =======================================

msi_df_clust <- msi_df[, !names(msi_df) %in% c("runNames", "region", "ground_truth")]

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



# Normalize the data to be clustered
signal_cols <- setdiff(
  names(cor_data),
  c("x", "y", "avg_corr_neighbors")
)

cor_data_norm_xy <- normalize_pixels(
  data = cor_data,
  signal_cols = signal_cols,
  spatial_cols = c("x", "y"),
  method = "tic"
)

# Find median to scale coordinates with
signal_scale <- median(
  abs(as.matrix(cor_data_norm_xy[, signal_cols])),
  na.rm = TRUE
)

# Scale the coordinates using median of the normalized intensities
cor_data_norm_xy$x <- (cor_data_norm_xy$x / max(cor_data_norm_xy$x)) * signal_scale * (sqrt(ncol(cor_data_norm_xy)) )
cor_data_norm_xy$y <- (cor_data_norm_xy$y / max(cor_data_norm_xy$y)) * signal_scale * (sqrt(ncol(cor_data_norm_xy)) )

# Data frame for clustering without x and y coordinates
cor_data_norm <- cor_data_norm_xy[, !names(cor_data_norm_xy) %in% c("x", "y")]


# Set number of clusters
nclust <- 4

#================== Apply MSIClust with x and y coordinates ====================================


# Find individual fuzzifiers by iterating until enough fuzzifiers are below threshold
res_xy <- tune_fuzz_grid_early(
  x            = yj$x.t,
  dims         = dim(cor_data_norm_xy),
  NClust       = nclust,
  new_max_range = c(1, 10),
  new_min = 0,
  step         = 0.1,
  target_frac  = 0.6,
  m_threshold  = 1.5
)

# Apply VSClust with individual fuzzifiers
msiclust_alg_xy <- vsclust_algorithm(cor_data_norm_xy,
                                     centers = nclust,
                                     iterMax = 100,
                                     m = res_xy$best_fuzz$m)



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



# ============================= MSIClust without x and y coordinates ====================================

# Find individual fuzzifiers by iterating until enough fuzzifiers are below threshold
res <- tune_fuzz_grid_early(
  x            = yj$x.t,
  dims         = dim(cor_data_norm),
  NClust       = nclust,
  new_max_range = c(1, 10),
  new_min = 0,
  step         = 0.1,
  target_frac  = 0.6,
  m_threshold  = 1.5
)

# Apply VSClust with individual fuzzifiers
msiclust_alg <- vsclust_algorithm(cor_data_norm,
                                  centers = 3,
                                  iterMax = 100,
                                  m = res_xy$best_fuzz$m)


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
fuzz <- determine_fuzz(dims = dim(cor_data_norm_xy), NClust = nclust, Sds = 1.8)


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
fuzz <- determine_fuzz(dims = dim(cor_data_norm), NClust = nclust, Sds = 2.075)


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
mean(msi_df_gt$VSClust_cluster == "No_cluster")

#=========================== Kmeans with x and y coordinates =================================


km_xy <- kmeans(cor_data_norm_xy, centers = nclust, nstart = 25)

msi_df_gt$Kmeans_xy_cluster <- km_xy$cluster


#======================== Kmeans without x and y coordinates =================================

km <- kmeans(cor_data_norm, centers = nclust, nstart = 25)

msi_df_gt$Kmeans_cluster <- km$cluster





#================================== Accuracy of clusterings ======================================


cluster_cols <- c(
  #"MSIClust_xy_cluster",
  "MSIClust_cluster",
  #"VSClust_xy_cluster",
  "VSClust_cluster",
  #"Kmeans_xy_cluster",
  "Kmeans_cluster"
)




# acc_res <- cluster_eval_by_region(
#   df = msi_df_gt,
#   cluster_cols = cluster_cols,
#   no_cluster = "No_cluster"
# )

f1_res <- cluster_f1(
  df = msi_df_gt,
  cluster_cols = cluster_cols,
  no_cluster = "No_cluster"
)

# ============================== Plot the cluster result ===================================

image_data <- msi_df_clust
image_data$cluster <- msi_df_gt$VSClust_cluster
image_data$cluster <- msi_df_gt$MSIClust_cluster


df <- image_data
base_df <- df
orientation <- "Flip Y"

if (orientation == "Flip X") {
  df$x <- max(base_df$x) - df$x + min(base_df$x)
} else if (orientation == "Flip Y") {
  df$y <- max(base_df$y) - df$y + min(base_df$y)
} else if (orientation == "Flip Both") {
  df$x <- max(base_df$x) - df$x + min(base_df$x)
  df$y <- max(base_df$y) - df$y + min(base_df$y)
}

# Sørg for at cluster er character
df$cluster <- as.character(df$cluster)

## --- Farver til clusters -------------------------------------------------

# Alle klasser der faktisk findes
present_clusters <- unique(df$cluster)
present_clusters <- c(
  if ("No_cluster" %in% present_clusters) "No_cluster",
  sort(setdiff(present_clusters, "No_cluster"))
)

# Rigtige klynger (uden "No_cluster")
valid_clusters <- sort(setdiff(present_clusters, "No_cluster"))
n_valid <- length(valid_clusters)

# Farver til rigtige klynger
cols_base <- RColorBrewer::brewer.pal(max(n_valid, 3), "Set3")[seq_len(n_valid)]
names(cols_base) <- valid_clusters

# Brug en HEX-farve til "No_cluster" så Plotly også forstår den
no_cluster_col <- "#D9D9D9"   # lys grå

all_colors <- c("No_cluster" = no_cluster_col)
for (cl in valid_clusters) {
  all_colors[cl] <- cols_base[[cl]]
}

## --- Raster-billede ------------------------------------------------------

img_uri <- make_raster_png(df, "cluster", all_colors)

## --- Orientering / akser -------------------------------------------------

# base_df <- original_clustered()
# req(base_df)


# Axis ranges
x_min <- min(df$x); x_max <- max(df$x)
y_min <- min(df$y); y_max <- max(df$y)

x_range <- c(x_min, x_max)
y_range <- c(y_min, y_max)

# Billede position og størrelse
img_x <- x_min
img_y <- y_max
img_sizex <- x_max - x_min
img_sizey <- y_max - y_min

# Flip X
if (orientation == "Flip X" || orientation == "Flip Both") {
  x_range <- rev(x_range)
  img_x <- x_max                # start fra højre
  img_sizex <- -(x_max - x_min) # negativ bredde flip’er billedet horisontalt
}

# Flip Y
if (orientation == "Flip Y" || orientation == "Flip Both") {
  y_range <- rev(y_range)
  img_y <- y_min                # start fra bunden
  img_sizey <- y_max - y_min    # positiv højde (y er allerede vendt i make_raster_png)
}

## --- Plotly-plot ---------------------------------------------------------

p <- plot_ly(source = "cluster") %>%
  add_trace(x = NULL, y = NULL, type = "scatter", mode = "markers") %>%
  layout(
    images = list(list(
      source = img_uri,
      xref = "x", yref = "y",
      x = img_x, y = img_y,
      sizex = img_sizex,
      sizey = img_sizey,
      sizing = "stretch", 
      layer = "top"
    )),
    dragmode = "drawclosedpath",
    newshape = list(line = list(color = "black", width = 1),
                    fillcolor = "rgba(0,0,0,0.05)"),
    title = "MSI Clustering Result",
    xaxis = list(range = x_range, title = "x"),
    yaxis = list(range = y_range, title = "y",
                 scaleanchor = "x", scaleratio = 1),
    showlegend = TRUE,
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.15,
      yanchor = "top"
    )
  ) %>%
  config(
    displaylogo = FALSE,
    modeBarButtonsToAdd = list("drawclosedpath", "eraseshape"),
    modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian",
                               "toggleSpikelines", "toImage", "select2d", "lasso2d")
  )

## --- Legend-traces (samme stil som Class-koden) --------------------------

for (cls in present_clusters) {
  col <- all_colors[[cls]]
  
  p <- p %>%
    add_trace(
      x = x_min - 1000,
      y = y_min - 1000,
      type = "scatter",
      mode = "markers",
      marker = list(size = 10, color = col),
      name = if (cls == "No_cluster") "No cluster" else paste("Cluster", cls),
      showlegend = TRUE,
      hoverinfo = "skip",
      inherit = FALSE
    )
}

p




cluster_cols <- c(
  #"MSIClust_xy_cluster",
  "MSIClust_cluster"
  #"VSClust_xy_cluster",
  #"VSClust_cluster",
  #"Kmeans_xy_cluster",
  #"Kmeans_cluster"
)



library(dplyr)
library(tibble)
library(purrr)

# (valgfrit) lille helper til 0-1 scaling
range01 <- function(x) {
  r <- range(x, na.rm = TRUE)
  if (isTRUE(all.equal(r[1], r[2]))) return(rep(0, length(x))) # undgå /0
  (x - r[1]) / (r[2] - r[1])
}

nclust <- 4
minMem <- 0.5
alphas <- seq(0.5, 20, by = 0.5)
inv_cor <- cor_data$inv_cor

# Gem alt pr. alpha i en liste
alpha_runs <- vector("list", length(alphas))
names(alpha_runs) <- as.character(alphas)

# Hvis du har andre clustering-kolonner du gerne vil score én gang:
# other_cols <- setdiff(cluster_cols, "MSIClust_cluster")
# baseline <- cluster_f1(msi_df_gt, cluster_cols = other_cols, no_cluster = "No_cluster")

for (i in seq_along(alphas)) {
  a <- alphas[i]
  
  # ---- 1) lav fuzzifiers til denne alpha ----
  # yj <- yeojohnson(inv_cor)    
  # yj_scaled <- range01(yj$x.t) * a
  
  # Anden option uden YJ transformation
  yj_scaled <- range01(inv_cor) * a
  
  fuzz <- determine_fuzz(
    dims   = dim(cor_data_norm),
    NClust = nclust,
    Sds    = yj_scaled
  )
  
  # ---- 2) kør VSClust / MSIClust ----
  msiclust_alg <- vsclust_algorithm(
    cor_data_norm,
    centers = nclust,
    iterMax = 100,
    m       = fuzz$m
  )
  
  maxMemVec <- rowMaxs(msiclust_alg[["membership"]])
  
  msiclust_cluster_corrected <- ifelse(
    maxMemVec > minMem,
    as.character(msiclust_alg[["cluster"]]),
    "No_cluster"
  )
  
  # ---- 3) score mod ground truth ----
  df_run <- msi_df_gt
  df_run$MSIClust_cluster <- msiclust_cluster_corrected
  
  no_cluster <- "No_cluster"
  
  frac_by_region <- df_run %>%
    group_by(region) %>%
    summarise(
      n_total = n(),
      n_no_cluster = sum(MSIClust_cluster == no_cluster, na.rm = TRUE),
      n_member = sum(MSIClust_cluster != no_cluster, na.rm = TRUE),
      frac_no_cluster = n_no_cluster / n_total,
      frac_cluster_member = n_member / n_total,
      .groups = "drop"
    )
  
  f1_res <- cluster_f1(
    df = df_run,
    cluster_cols = "MSIClust_cluster",   # scorer kun MSIClust pr. alpha
    no_cluster = "No_cluster"
  )
  
  # ---- 4) gem alt for denne alpha ----
  alpha_runs[[i]] <- list(
    alpha = a,
    frac_by_region = frac_by_region,
    fuzz  = fuzz,        
    res   = f1_res      
  )
}


scores_long <- imap_dfr(alpha_runs, function(run, nm) {
  a <- run$alpha
  res <- run$res
  
  imap_dfr(res, function(reg_obj, reg_nm) {
    reg_obj$scores %>%
      mutate(alpha = a, region = reg_nm, .before = 1) %>%
      left_join(run$frac_by_region, by = "region")
  })
})

# mapping_long <- imap_dfr(alpha_runs, function(run, nm) {
#   a <- run$alpha
#   res <- run$res
#   
#   imap_dfr(res, function(reg_obj, reg_nm) {
#     # reg_obj$mapping er en liste med én tibble per metode
#     imap_dfr(reg_obj$mapping, function(map_tbl, method_nm) {
#       if (nrow(map_tbl) == 0) return(tibble())
#       map_tbl %>% mutate(alpha = a, region = reg_nm, method = method_nm, .before = 1)
#     })
#   })
# })
# 
# perclass_long <- imap_dfr(alpha_runs, function(run, nm) {
#   a <- run$alpha
#   res <- run$res
#   
#   imap_dfr(res, function(reg_obj, reg_nm) {
#     # reg_obj$per_class er en liste med én tibble per metode
#     imap_dfr(reg_obj$per_class, function(pc_tbl, method_nm) {
#       if (nrow(pc_tbl) == 0) return(tibble())
#       pc_tbl %>% mutate(alpha = a, region = reg_nm, method = method_nm, .before = 1)
#     })
#   })
# })

ggplot(scores_long, aes(alpha, weighted_f1)) +
  geom_line() +
  facet_wrap(~ region) +
  theme_minimal()

ggplot(scores_long, aes(frac_no_cluster, weighted_f1)) +
  geom_line() +
  facet_wrap(~ region) +
  geom_text(aes(label = alpha), vjust = -0.6, size = 2) +
  theme_minimal()






# Make the clustering with chosen alpha for later plotting and comparisons
inv_cor <- cor_data$inv_cor
yj_scaled <- range01(inv_cor) * 10

fuzz <- determine_fuzz(
  dims   = dim(cor_data_norm),
  NClust = nclust,
  Sds    = yj_scaled
)

# Apply VSClust with individual fuzzifiers
msiclust_alg <- vsclust_algorithm(cor_data_norm,
                                  centers = nclust,
                                  iterMax = 100,
                                  m = fuzz$m)


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
mean(msi_df_gt$MSIClust_cluster == "No_cluster")



















# ============================== OLD ================================


scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}


macro_precision_recall <- function(truth, pred) {
  labs <- sort(unique(truth))
  
  per_class <- lapply(labs, function(k) {
    tp <- sum(truth == k & pred == k, na.rm = TRUE)
    fp <- sum(truth != k & pred == k, na.rm = TRUE)
    fn <- sum(truth == k & pred != k, na.rm = TRUE)
    
    precision <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
    recall    <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
    
    data.frame(class = k,
               precision = precision,
               recall = recall)
  })
  
  per_class <- do.call(rbind, per_class)
  
  list(
    macro_precision = mean(per_class$precision, na.rm = TRUE),
    macro_recall    = mean(per_class$recall,    na.rm = TRUE)
  )
}


run_alpha_once_macroF1 <- function(alpha,
                                   data_for_clust,
                                   inv_cor,
                                   gt_vec,
                                   nclust = 3,
                                   minMem = 0.5,
                                   no_cluster = "No_cluster") {
  
  # --- 1) Sds = alpha * scaled(inv_cor) ---
  #inv01 <- scale01(inv_cor)
  #Sds <- inv01 + alpha
  
  Sds <- ((inv_cor - min(inv_cor)) / (max(inv_cor) - min(inv_cor))) * alpha
  
  fuzz <- determine_fuzz(
    dims   = dim(data_for_clust),
    NClust = nclust,
    Sds    = Sds
  )
  
  alg <- vsclust_algorithm(
    data_for_clust,
    centers = nclust,
    iterMax = 100,
    m = fuzz$m
  )
  
  maxMem <- matrixStats::rowMaxs(alg[["membership"]])
  cl_raw <- as.character(alg[["cluster"]])
  pred0  <- ifelse(maxMem > minMem, cl_raw, no_cluster)
  
  n_total      <- length(pred0)
  n_clustered  <- sum(pred0 != no_cluster, na.rm = TRUE)
  n_no_cluster <- sum(pred0 == no_cluster, na.rm = TRUE)
  
  frac_cluster    <- n_clustered / n_total
  frac_no_cluster <- n_no_cluster / n_total
  
  # --- 2) fjern No_cluster ---
  keep <- !is.na(pred0) & pred0 != no_cluster & !is.na(gt_vec)
  if (!any(keep)) {
    return(tibble(
      alpha = alpha,
      macro_f1 = NA_real_,
      macro_precision = NA_real_,
      macro_recall = NA_real_,
      accuracy = NA_real_,
      n = 0
    ))
  }
  
  pred  <- pred0[keep]
  truth <- gt_vec[keep]
  
  # --- 3) Hungarian mapping (samme som cluster_f1) ---
  mat <- as.matrix(table(pred, truth))
  
  if (nrow(mat) > ncol(mat)) {
    return(tibble(
      alpha = alpha,
      macro_f1 = NA_real_,
      macro_precision = NA_real_,
      macro_recall = NA_real_,
      accuracy = NA_real_,
      n = length(truth),
      frac_cluster = frac_cluster,
      frac_no_cluster = frac_no_cluster
    ))
  }
  
  assignment <- clue::solve_LSAP(mat, maximum = TRUE)
  clusters <- rownames(mat)
  truths   <- colnames(mat)
  
  mapping <- tibble(
    cluster = clusters,
    mapped_truth = truths[as.integer(assignment)]
  )
  
  pred_mapped <- mapping$mapped_truth[match(pred, mapping$cluster)]
  
  # --- 4) Scores (samme som cluster_f1) ---
  acc <- mean(pred_mapped == truth)
  f1  <- f1_scores(truth, pred_mapped)
  pr  <- macro_precision_recall(truth, pred_mapped)
  
  tibble(
    alpha = alpha,
    macro_f1 = f1$macro_f1,
    macro_precision = pr$macro_precision,
    macro_recall = pr$macro_recall,
    accuracy = acc,
    n = length(truth),
    frac_cluster = frac_cluster,
    frac_no_cluster = frac_no_cluster
  )
}



library(tidyr)


inv_cor <- 1 - cor_data$avg_corr_neighbors

# ground truth i samme rækkefølge som data_for_clust
# (hvis dine data stadig er aligned som i din pipeline)
gt_vec <- msi_df_gt$ground_truth

alphas <- seq(0, 7, by = 0.1)   # justér frit
alpha_res <- purrr::map_dfr(alphas, ~run_alpha_once_macroF1(
  alpha = .x,
  data_for_clust = cor_data_norm,  # eller cor_data_norm_xy
  inv_cor = inv_cor,
  gt_vec = gt_vec,
  nclust = nclust,
  minMem = minMem,
  no_cluster = "No_cluster"
))

ggplot(alpha_res, aes(x = macro_recall, y = macro_precision)) +
  geom_point() +
  #geom_text(aes(label = alpha), vjust = -0.6, size = 3) +
  theme_minimal() +
  labs(title="Recall precision plot",
       x="Recall", y="Precision")


ggplot(alpha_res, aes(x = alpha, y = macro_precision)) +
  geom_point() +
  #geom_text(aes(label = alpha), vjust = -0.6, size = 3) +
  theme_minimal() +
  labs(title="Alpha precision plot",
       x="Alpha", y="Precision")


ggplot(alpha_res, aes(x = alpha, y = macro_recall)) +
  geom_point() +
  #geom_text(aes(label = alpha), vjust = -0.6, size = 3) +
  theme_minimal() +
  labs(title="Alpha recall plot",
       x="Alpha", y="Recall")


alpha_res <- alpha_res %>%
  mutate(f1_x_frac = macro_f1 * frac_assigned)

best <- alpha_res %>%
  filter(!is.na(f1_x_frac)) %>%
  slice_max(f1_x_frac, n = 1, with_ties = FALSE)

ggplot(alpha_res, aes(x = alpha, y = f1_x_frac)) +
  geom_point() +
  geom_vline(xintercept = best$alpha, linetype = "dashed") +
  theme_minimal() +
  labs(title="Alpha - F1 × frac_assigned plot",
       x = "Alpha", y = "F1 × frac_assigned")



alpha_res <- alpha_res %>%
  mutate(f1_x_fracNo = macro_f1 * frac_assigned)

best <- alpha_res %>%
  filter(!is.na(f1_x_fracNo)) %>%
  slice_max(f1_x_fracNo, n = 1, with_ties = FALSE)

ggplot(alpha_res, aes(x = alpha, y = f1_x_fracNo)) +
  geom_point() +
  geom_vline(xintercept = best$alpha, linetype = "dashed") +
  theme_minimal() +
  labs(title="Alpha - F1 × frac_No plot",
       x = "Alpha", y = "F1 × frac_No")






alpha_res$alpha_f1_frac <- alpha_res$alpha*alpha_res$macro_f1*alpha_res$frac_assigned

ggplot(alpha_res, aes(x = alpha, y = alpha_f1_frac)) +
  geom_point() +
  #geom_text(aes(label = alpha), vjust = -0.6, size = 3) +
  theme_minimal() +
  labs(title="Relation between alpha, F1-score and fraction of cluster members",
       x="Alpha", y="Alpha * Macro F1 * Frac_cluster")

hist(inv_cor, breaks = 30)
yj <- yeojohnson(inv_cor)
yj_scaled <- (((yj$x.t - min(yj$x.t)) / (max(yj$x.t) - min(yj$x.t))) * 5.5) 
hist(yj_scaled, breaks = 30)
fuzz <- determine_fuzz(dims = dim(cor_data_norm), NClust = 3, Sds = yj_scaled)
hist(fuzz$m, breaks = 30)



alphas <- seq(0, 15, by = 0.5) 

regions <- sort(unique(msi_df_gt$region))

alpha_res_by_region <- purrr::map_dfr(regions, function(r) {
  
  idx <- msi_df_gt$region == r
  
  # skip tomme regioner
  if (sum(idx, na.rm = TRUE) == 0) return(NULL)
  
  purrr::map_dfr(alphas, function(a) {
    run_alpha_once_macroF1(
      alpha = a,
      data_for_clust = cor_data_norm[idx, , drop = FALSE], 
      #inv_cor = inv_cor[idx],
      inv_cor = yj$x.t[idx],
      gt_vec = gt_vec[idx],
      nclust = nclust,
      minMem = minMem,
      no_cluster = "No_cluster"
    ) %>%
      mutate(region = r)
  })
})

filter_region <- "01"

best <- alpha_res_by_region %>%
  filter(region == filter_region) %>%
  filter(!is.na(macro_f1)) %>%
  slice_max(macro_f1, n = 1, with_ties = FALSE)

alpha_res_by_region %>%
  filter(region == filter_region) %>%
  ggplot(aes(x = alpha, y = macro_f1)) +
  geom_point() +
  geom_vline(xintercept = best$alpha, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = paste("Alpha F1 plot – region", filter_region),
    x = "Alpha",
    y = "F1"
  )



# F1 multiplied by faction of cluster members
best_clust_mem <- alpha_res_by_region %>%
  filter(region == filter_region) %>%
  filter(!is.na(macro_f1 * frac_cluster)) %>%
  slice_max(macro_f1 * frac_cluster, n = 1, with_ties = FALSE)

alpha_res_by_region %>%
  filter(region == filter_region) %>%
  ggplot(aes(x = alpha, y = macro_f1 * frac_cluster)) +
  geom_point() +
  geom_vline(xintercept = best_clust_mem$alpha, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = paste("Alpha F1 * cluster fraction plot – region", filter_region),
    x = "Alpha",
    y = "F1 * frac_cluster"
  )


alpha_res_by_region %>%
  filter(region == filter_region) %>%
  ggplot(aes(x = frac_no_cluster, y = macro_f1)) +
  geom_point() +
  geom_text(aes(label = alpha), vjust = -0.6, size = 3) +
  theme_minimal() +
  labs(
    title = paste("Alpha, F1, No cluster fraction plot – region", filter_region),
    x = "Fraction of No cluster members",
    y = "F1"
  )


