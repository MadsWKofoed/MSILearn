library(Cardinal)
library(matter)
library(dplyr)
library(vsclust)
library(matrixStats)
library(plotly)
library(RColorBrewer)

bp <- parallel::detectCores() - 2
setCardinalParallel(workers = bp)

temp_imzml_proc <- "kidney_and_tumor/PROCESSED_kidney.imzML"
temp_imzml <- "kidney_and_tumor/RAW_kidney.imzml"

temp_imzml_proc <- "kidney_and_tumor/PROCESSED_tumor.imzML"
temp_imzml <- "kidney_and_tumor/RAW_tumor.imzml"



msi_data <- readImzML(temp_imzml, memory = FALSE, check = FALSE,
                      mass.range = NULL, resolution = 10, units = c("ppm"),
                      guess.max = 1000L, as = "auto", parse.only=FALSE,
                      verbose = getCardinalVerbose(), chunkopts = list(),
                      BPPARAM = bpparam())

msi_data_proc <- readImzML(temp_imzml_proc, memory = FALSE, check = FALSE,
                      mass.range = NULL, resolution = 10, units = c("ppm"),
                      guess.max = 1000L, as = "auto", parse.only=FALSE,
                      verbose = getCardinalVerbose(), chunkopts = list(),
                      BPPARAM = bpparam())

# Use processed versions of the data to find mz-values
mzvals <- mz(msi_data_proc)



# coords_proc <- coord(msi_data_proc)
# coords_raw <- coord(msi_data)
# 
# keep <- coords_raw %in% coords_proc
# 
# msi_subset <- subsetPixels(msi_data, keep)


# tic <- colSums(intensity(msi_data))
# pixelData(msi_data)$tic <- tic
# threshold <- quantile(tic, 0.40)
# msi_masked <- subsetPixels(msi_data, tic > threshold)
# 
# keep <- tic < threshold
# 
# image(msi_data, mz = 695.3)

# bg_roi <- selectROI(msi_data,
#                     mode = "region",
#                     #mz = c(846.3, 695.3, 725.3),
#                     mz = c(369.3, 798.5, 830.6),
#                     superpose = TRUE,
#                     contrast.enhance = "suppress",
#                     normalize.image = "linear",
#                     col = c("blue", "red", "green"))
# 
# bg <- bg_roi
# 
# group <- factor(ifelse(bg, "bg", "other"))
# 
# bg_summary <- summarizeFeatures(msi_data, stat = "mean", groups = group)
# 
# colnames(featureData(bg_summary))
# 
# 
# fd <- fData(bg_summary)
# colnames(fd)
# 
# bg_mean <- as.numeric(fd$bg.mean)
# 
# bg_cor <- spectrapply(msi_data,
#                       FUN = function(x, bg_mean) {
#                         cor(x, bg_mean, use = "complete.obs")
#                       },
#                       bg_mean = bg_mean,
#                       simplify = TRUE
#   )
# 
# pixelData(msi_data)$bg_cor <- bg_cor
# 
# threshold <- 0.4
# 
# keep <- bg_cor < threshold
# 
# msi_filtered <- subsetPixels(msi_data, keep)
# vizi_style("dark")
image(
  msi_filtered,
  #mz = c(846.3, 695.3, 725.3),
  mz = c(369.3, 798.5, 830.6),
  superpose = TRUE,
  contrast.enhance = "suppress",
  normalize.image = "linear",
  col = c("blue", "red", "green")
)

coords <- read.csv("kidney_and_tumor/tumor_roi_points_2025-12-20.csv", header = TRUE)
roi_coords <- coords[, c("x", "y")]

msi_coords <- coord(msi_data)

keep <- paste(msi_coords$x, msi_coords$y) %in%
  paste(roi_coords$x, roi_coords$y)

msi_roi <- subsetPixels(msi_data, keep)



control_mean <- summarizeFeatures(msi_roi, "mean")


mz_ref <- mzvals

snr = 3
tolerance = 0.5

control_MSI_ref <- control_mean %>%
  peakPick(SNR = snr) %>%
  peakAlign(ref = mz_ref, tolerance = tolerance, units = "mz") %>%
  subsetFeatures() %>%
  process()


msi_data_binned <- bin(
  msi_roi,
  ref = mz(control_MSI_ref),
  tolerance = tolerance,
  units = "mz",
  BPPARAM = BiocParallel::bpparam()
) %>% process()


make_msi_dataframe <- function(msi_data_binned) {
  
  msi_matrix <- t(as.matrix(spectra(msi_data_binned)))
  
  mz_names <- paste0("mz_", mz(msi_data_binned))
  
  coords <- coord(msi_data_binned)
  
  run_name <- runNames(msi_data_binned)
  pixel_names <- rep(run_name, nrow(msi_matrix))
  
  full_df <- data.frame(
    runNames = pixel_names,
    x = coords$x,
    y = coords$y,
    msi_matrix
  )
  
  colnames(full_df) <- c("runNames", "x", "y", mz_names)
  
  return(full_df)
}


msi_df <- make_msi_dataframe(msi_data_binned)

# rows_with_0 <- msi_df[apply(msi_df, 1, function(row) mean(row == 0) > 0.0001), ]

# vizi_style("dark")
image(
  msi_data_proc,
  #mz = c(846.3, 695.3, 725.3),
  mz = c(369.3, 798.5, 830.6),
  superpose = TRUE,
  contrast.enhance = "suppress",
  normalize.image = "linear",
  col = c("blue", "red", "green")
)
# image(
#   msi_data_binned,
#   mz = 846.5495
#   #superpose = TRUE,
#   #contrast.enhance = "suppress",
#   #normalize.image = "linear",
#   #col = "blue"
# )



# Remove RunName column
msi_df_clust <- msi_df[, !names(msi_df) %in% "runNames"]


cor_data <- msi_df_clust


compute_neighbor_cor_8 <- function(dat,
                                   x_col = "x",
                                   y_col = "y",
                                   mz_cols = NULL,
                                   cores = 1) {
  
  if (is.null(mz_cols)) {
    mz_cols <- setdiff(colnames(dat), c(x_col, y_col))
  }
  
  xy <- as.matrix(dat[, c(x_col, y_col)])
  intens <- as.matrix(dat[, mz_cols])
  
  key_vec <- paste(xy[, 1], xy[, 2], sep = "_")
  index_lookup <- setNames(seq_len(nrow(dat)), key_vec)
  
  # --------------- helper functions ---------------
  neighbor_idx_fun <- function(i, xy, index_lookup) {
    x <- xy[i, 1];  y <- xy[i, 2]
    
    grid <- expand.grid(
      xx = (x - 1):(x + 1),
      yy = (y - 1):(y + 1)
    )
    coords <- as.matrix(grid)
    coords <- coords[!(coords[, 1] == x & coords[, 2] == y), , drop = FALSE]
    
    keys <- paste(coords[, 1], coords[, 2], sep = "_")
    idx <- index_lookup[keys]
    idx[!is.na(idx)]
  }
  
  pixel_cor_fun <- function(i, xy, intens, index_lookup) {
    nei <- neighbor_idx_fun(i, xy, index_lookup)
    if (length(nei) == 0) return(NA_real_)
    
    v <- intens[i, ]
    mats <- intens[nei, , drop = FALSE]
    
    cors <- apply(mats, 1, function(z) cor(v, z, use = "pairwise.complete.obs"))
    mean(cors, na.rm = TRUE)
  }
  # ------------------------------------------------
  
  n <- nrow(dat)
  
  if (cores > 1) {
    require(parallel)
    cl <- parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cl))
    
    # eksportér alt der skal bruges på workers
    parallel::clusterExport(cl,
                            varlist = c("xy", "intens", "index_lookup",
                                        "neighbor_idx_fun", "pixel_cor_fun"),
                            envir = environment())
    
    avg_cor <- parallel::parSapply(
      cl,
      X = 1:n,
      FUN = pixel_cor_fun,
      xy = xy,
      intens = intens,
      index_lookup = index_lookup
    )
    
  } else {
    avg_cor <- sapply(
      1:n,
      pixel_cor_fun,
      xy = xy,
      intens = intens,
      index_lookup = index_lookup
    )
  }
  
  avg_cor
}


cor_data$avg_corr_neighbors <- compute_neighbor_cor_8(cor_data, x_col = "x", y_col = "y", cores = 30)

cor_data_clean <- cor_data[!is.na(cor_data$avg_corr_neighbors), ]

hist(cor_data_clean$avg_corr_neighbors, 
     breaks = 100, 
     main = "Distribution of mean correlations", 
     xlab = "Correlation value")
range(na.omit(cor_data$avg_corr_neighbors))
sum(is.na(cor_data$avg_corr_neighbors))

test <- cor_data_clean$avg_corr_neighbors

cor_test <- 1 - test
hist(cor_test, 
     breaks = 100, 
     main = "Distribution of 1 - mean correlation", 
     xlab = "1 - Correlation value")

library(bestNormalize)

yj <- yeojohnson(cor_test)
hist(yj$x.t, 
     breaks = 100, 
     main = "Distribution yj transformed 1 - mean correlation", 
     xlab = "transformed 1 - Correlation value")

scale_to_range <- function(x, new_min = 0, new_max = 4.9) {
  ((x - min(x))/(max(x) - min(x))) * (new_max - new_min) + new_min
}

pow_test <- scale_to_range(na.omit(cor_test))
range(na.omit(pow_test))
hist(pow_test,
     breaks = 100,
     main = "Distribution of scaled 1 - mean correlation",
     xlab = "Scaled 1 - Correlation value")


tune_fuzz_grid_early <- function(x,
                                 dims,
                                 NClust,
                                 new_max_range = c(1, 4),
                                 new_min       = 0,
                                 step          = 0.1,
                                 target_frac   = 0.8,
                                 m_threshold   = 1.2) {
  

  
  # vi starter fra den høje ende og går nedad med step
  new_max_values <- seq(from = max(new_max_range),
                        to   = new_min,
                        by   = -abs(step))
  
  best_new_max  <- tail(new_max_values, 1)
  best_fuzz     <- NULL
  best_frac     <- NA_real_
  
  for (new_max in new_max_values) {
    
    # 1) brug DIN skaleringslogik
    scaled <- ((x - min(x)) / (max(x) - min(x))) *
      (new_max - new_min) + new_min
    
    # 2) beregn fuzzifiers
    fuzz <- determine_fuzz(
      dims   = dims,
      NClust = NClust,
      Sds    = scaled
    )
    
    # 3) hvor mange m-værdier er under threshold?
    frac_below <- mean(fuzz$m <= m_threshold)
    
    # gem bedste bud (hvis vi ender med at køre hele vejen)
    best_new_max <- new_max
    best_fuzz    <- fuzz
    best_frac    <- frac_below
    
    # 4) stop tidligt når vi har NOK der er under threshold
    if (frac_below >= target_frac) {
      message(sprintf(
        "Early stop: new_max = %.2f, frac(m <= %.2f) = %.3f (target = %.3f)",
        new_max, m_threshold, frac_below, target_frac
      ))
      
      return(list(
        best_new_max  = new_max,
        best_fuzz     = fuzz,
        frac_below    = frac_below,
        target_frac   = target_frac,
        stopped_early = TRUE
      ))
    }
    
    # ellers fortsætter loopen automatisk med næste (lavere) new_max
  }
  
  # hvis vi kom hele vejen ned uden at nå target_frac:
  message("Nåede new_min uden at opnå target_frac – returnerer sidste step.")
  
  list(
    best_new_max  = best_new_max,
    best_fuzz     = best_fuzz,
    frac_below    = best_frac,
    target_frac   = target_frac,
    stopped_early = FALSE
  )
}


res <- tune_fuzz_grid_early(
  x            = yj$x.t,
  dims         = dim(cor_data_clean),
  NClust       = 3,
  new_max_range = c(1, 10),
  new_min = 0,
  step         = 0.1,
  target_frac  = 0.6,
  m_threshold  = 1.3
)

res$best_new_max
res$frac_below
range(res$best_fuzz$m)
hist(res$best_fuzz$m, breaks = 200)



# fuzz <- determine_fuzz(dims = dim(cor_data_clean), NClust = 3, Sds = res$best_fuzz$m)
# frac_below <- mean(fuzz$m <= 1.3)
# frac_below
# range(fuzz$m)
# hist(fuzz$m, 
#      breaks = 200, 
#      main = "Distribution of Fuzzifiers from\n scaled 1 - mean correlation", 
#      xlab = "Fuzzifier values",
#      xlim = c(1, max(fuzz$m)))


# # Without x and y do this step. If not then skip to scaling
# msi_df_clust <- as.matrix(msi_df[, grep("^mz_", colnames(msi_df))])


cor_data_clean_scaled <- scale(cor_data_clean[, !names(cor_data_clean) %in% "avg_corr_neighbors"])

vsclust_alg <- vsclust_algorithm(cor_data_clean_scaled,
                                 centers = 3,
                                 iterMax = 100,
                                 m = res$best_fuzz$m)



minMem <- 0.35

clust_alg_res <- data.frame(cluster = vsclust_alg[["cluster"]],
                            isClusterMember =  rowMaxs(vsclust_alg[["membership"]]) > minMem,
                            maxMembership = rowMaxs(vsclust_alg[["membership"]]),
                            vsclust_alg$membership)

range(clust_alg_res$maxMembership)
sum(clust_alg_res$maxMembership == 1)
hist(clust_alg_res$maxMembership, 
     breaks = 100, 
     main = "Maximum Membership values", 
     xlab = "Maximum membership values")

clust_alg_res$cluster_corrected <- ifelse(
  clust_alg_res$isClusterMember,
  as.character(clust_alg_res$cluster),
  "No_cluster"
)

image_data <- cor_data[, !names(cor_data) %in% "avg_corr_neighbors"]
image_data$cluster <- msi_df_gt$MSIClust_cluster

#msi_df$cluster <- clust_alg_res$cluster_corrected


make_raster_png <- function(df, fill_var, colors) {
  df$x <- df$x - min(df$x) + 1
  df$y <- df$y - min(df$y) + 1
  width <- max(df$x)
  height <- max(df$y)
  
  # Create EMPTY matrix (NA = transparent)
  mat <- matrix(NA_character_, nrow = height, ncol = width)
  
  # Flip y-coordinates: (height - y + 1) converts bottom-up to top-down
  mat[cbind(height - df$y + 1, df$x)] <- as.character(df[[fill_var]])
  
  col_img <- matrix(colors[mat], nrow = height, ncol = width)
  
  rgb_vals <- col2rgb(col_img, alpha = TRUE) / 255
  rgb_array <- array(NA_real_, dim = c(height, width, 4))
  rgb_array[,,1] <- matrix(rgb_vals["red", ], nrow = height, ncol = width)
  rgb_array[,,2] <- matrix(rgb_vals["green", ], nrow = height, ncol = width)
  rgb_array[,,3] <- matrix(rgb_vals["blue", ], nrow = height, ncol = width)
  rgb_array[,,4] <- matrix(rgb_vals["alpha", ], nrow = height, ncol = width)
  
  # Make NA pixels transparent
  na_pixels <- is.na(mat)
  rgb_array[,,4][na_pixels] <- 0
  
  tmp <- tempfile(fileext = ".png")
  png::writePNG(rgb_array, target = tmp)
  base64enc::dataURI(file = tmp, mime = "image/png")
}


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
      sizing = "stretch", layer = "below"
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

table(clust_alg_res$cluster_corrected)

msi_df$cluster <- clust_alg_res$cluster_corrected



# Accuracy of clusterings
necrosis_coords <- read.csv("kidney_and_tumor/tumor_necrosis_roi_points_2025-12-27.csv", header = TRUE)
necrosis_coords <- necrosis_coords[, c("x", "y")]
healthy_coords <- read.csv("kidney_and_tumor/tumor_healthy_roi_points_2025-12-27.csv", header = TRUE)
healthy_coords <- healthy_coords[, c("x", "y")]
# reamining coords are tumor



# Putting in the ground truths defined from H&E image
msi_df_gt <- msi_df %>%
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
table(msi_df_gt$ground_truth, msi_df_gt$cluster)

cluster_map <- msi_df_gt %>%
  dplyr::count(cluster, ground_truth) %>%
  group_by(cluster) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(cluster, mapped_truth = ground_truth)

cluster_map


# Finding accuracy
msi_df_eval <- msi_df_gt %>%
  left_join(cluster_map, by = "cluster")

accuracy <- mean(msi_df_eval$ground_truth == msi_df_eval$mapped_truth)
accuracy







msi_df_clust <- as.matrix(msi_df[, grep("^mz_", colnames(msi_df))])
msi_df_clust <- scale(msi_df_clust)
km <- kmeans(msi_df_clust, centers = 5, nstart = 25)
msi_df$cluster <- km$cluster
table(msi_df$cluster)


















library(mongolite)
library(jsonlite)

ref_data <- read.csv("/data/makof21/MSI_database/Data/572ref_mz.csv", stringsAsFactors = FALSE)
colnames(ref_data) <- c("mz")

mongo_ref <- mongo(collection = "mz_references",
                   db = "msi_project",
                   url = "mongodb://localhost:27018")

ref_doc <- list(
  reference_name = "572_features",
  mz_values = as.list(ref_data$mz),
  created_at = Sys.time(),
  description = "Top 572 features from previous data file",
  num_features = length(ref_data$mz)
)


mongo_ref$insert(ref_doc)




