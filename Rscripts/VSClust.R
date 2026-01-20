library(Cardinal)
library(matter)
library(dplyr)
library(vsclust)
library(matrixStats)
library(plotly)
library(RColorBrewer)


bp <- parallel::detectCores() - 1
setCardinalParallel(workers = bp)


temp_imzml <- "tumorinfiltrat.imzML"
temp_imzml <- "pt43fuldtregions.imzML"

msi_data <- readImzML(temp_imzml, memory = FALSE, check = FALSE,
                      mass.range = NULL, resolution = 10, units = c("ppm"),
                      guess.max = 1000L, as = "auto", parse.only=FALSE,
                      verbose = getCardinalVerbose(), chunkopts = list(),
                      BPPARAM = bpparam())


control_mean <- summarizeFeatures(msi_data, "mean")

mz_ref <- read.csv("ref_mz.csv")

snr = 3
tolerance = 0.5

control_MSI_ref <- control_mean %>%
  peakPick(SNR = snr) %>%
  peakAlign(ref = mz_ref$x, tolerance = tolerance, units = "mz") %>%
  subsetFeatures() %>%
  process()


msi_data_binned <- bin(
  msi_data,
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

# Remove RunName column
msi_df_clust <- msi_df[, !names(msi_df) %in% "runNames"]

# Add Sds column
msi_df_clust$Sds <- 1


ClustInd <- estimClustNum(msi_df_clust,
                          maxClust = 10,
                          scaling = "standardize",
                          cores = 30
                          )



estimClust.plot(ClustInd)
k <- optimalClustNum(ClustInd)
k <- 6
ClustOut <- runClustWrapper(msi_df_clust,
                            k,
                            VSClust = TRUE,
                            scaling = "standardize",
                            cores = 30)


Bestcl <- ClustOut$Bestcl
clust_res <- data.frame(cluster = Bestcl$cluster,
                        ClustOut$outFileClust,
                        isClusterMember = rowMaxs(Bestcl$membership) > 0.5,
                        maxMembership = rowMaxs(Bestcl$membership),
                        Bestcl$membership)


clust_res <- clust_res[order(as.numeric(row.names(clust_res))), ]

# Make new cluster vector/column with those below threshold set to "No_cluster"

outFileClust <- ClustOut[["outFileClust"]]






msi_df <- make_msi_dataframe(msi_data_binned)

# Remove RunName column
msi_df_clust <- msi_df[, !names(msi_df) %in% "runNames"]

# Scale the data
msi_df_clust <- scale(msi_df_clust)


fuzz <- determine_fuzz(dims = dim(msi_df_clust), NClust = 5, Sds = 0.72)
fuzz


vsclust_alg <- vsclust_algorithm(msi_df_clust,
                                 centers = 5,
                                 iterMax = 100,
                                 m = fuzz$m)

mem <- vsclust_alg[["membership"]]

minMem <- 0.5

clust_alg_res <- data.frame(cluster = vsclust_alg[["cluster"]],
                            isClusterMember =  rowMaxs(vsclust_alg[["membership"]]) > minMem,
                            maxMembership = rowMaxs(vsclust_alg[["membership"]]),
                            vsclust_alg$membership)

hist(clust_alg_res$maxMembership, 
     breaks = 100, 
     main = "Maximum Membership values", 
     xlab = "Maximum membership values")

clust_alg_res$cluster_corrected <- ifelse(
  clust_alg_res$isClusterMember,
  as.character(clust_alg_res$cluster),
  "No_cluster"
)


msi_df$cluster <- clust_alg_res$cluster_corrected



sum(clust_alg_res$isClusterMember)
sum(clust_alg_res$mazMembership == 1)








library(plotly)
library(RColorBrewer)
# --- Raster helper for both cluster and class plots (transparent background) ---
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



df <- msi_df
base_df <- df
orientation <- "Default"

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




# ======================== Using correlation for fuzzifiers =========================
library(Cardinal)
library(matter)
library(dplyr)
library(vsclust)
library(matrixStats)
library(plotly)
library(RColorBrewer)


bp <- parallel::detectCores() - 1
setCardinalParallel(workers = bp)

# --- Raster helper for both cluster and class plots (transparent background) ---
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


# Find mean correlation with neighborig pixels
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


temp_imzml <- "tumorinfiltrat.imzML"
temp_imzml <- "pt43fuldtregions.imzML"

msi_data <- readImzML(temp_imzml, memory = FALSE, check = FALSE,
                      mass.range = NULL, resolution = 10, units = c("ppm"),
                      guess.max = 1000L, as = "auto", parse.only=FALSE,
                      verbose = getCardinalVerbose(), chunkopts = list(),
                      BPPARAM = bpparam())


control_mean <- summarizeFeatures(msi_data, "mean")

mz_ref <- read.csv("ref_mz.csv")

snr = 3
tolerance = 0.5

control_MSI_ref <- control_mean %>%
  peakPick(SNR = snr) %>%
  peakAlign(ref = mz_ref$x, tolerance = tolerance, units = "mz") %>%
  subsetFeatures() %>%
  process()


msi_data_binned <- bin(
  msi_data,
  ref = mz(control_MSI_ref),
  tolerance = tolerance,
  units = "mz",
  BPPARAM = BiocParallel::bpparam()
) %>% process()



msi_df <- make_msi_dataframe(msi_data_binned)

# Remove RunName column
msi_df_clust <- msi_df[, !names(msi_df) %in% "runNames"]


cor_data <- msi_df_clust

cor_data$avg_corr_neighbors <- compute_neighbor_cor_8(cor_data, x_col = "x", y_col = "y", cores = 30)
hist(cor_data$avg_corr_neighbors, 
     breaks = 100, 
     main = "Distribution of mean correlations", 
     xlab = "Correlation value")
range(na.omit(cor_data$avg_corr_neighbors))
sum(is.na(cor_data$avg_corr_neighbors))

test <- cor_data$avg_corr_neighbors

cor_test <- 1 - test
hist(cor_test, 
     breaks = 100, 
     main = "Distribution of 1 - mean correlation", 
     xlab = "1 - Correlation value")


pow_test <- ((1 - test) ^ 0.3) * 2
range(na.omit(pow_test))
hist(pow_test, 
     breaks = 100, 
     main = "Distribution of ((1 - mean correlation) ^ 0.3) * 2", 
     xlab = "Power transformed 1 - Correlation value")




fuzz <- determine_fuzz(dims = dim(msi_df_clust), NClust = 3, Sds = pow_test)
range(fuzz$m)
hist(fuzz$m, 
     breaks = 200, 
     main = "Distribution of Fuzzifiers from\n Sds = ((1 - mean correlation) ^ 0.3) * 2", 
     xlab = "Fuzzifier values",
     xlim = c(1, 4))


# # Without x and y do this step. If not then skip to scaling
# msi_df_clust <- as.matrix(msi_df[, grep("^mz_", colnames(msi_df))])


msi_df_clust <- scale(msi_df_clust)

vsclust_alg <- vsclust_algorithm(msi_df_clust,
                                 centers = 3,
                                 iterMax = 100,
                                 m = fuzz$m)



minMem <- 0.5

clust_alg_res <- data.frame(cluster = vsclust_alg[["cluster"]],
                            isClusterMember =  rowMaxs(vsclust_alg[["membership"]]) > minMem,
                            maxMembership = rowMaxs(vsclust_alg[["membership"]]),
                            vsclust_alg$membership)

clust_alg_res$cluster_corrected <- ifelse(
  clust_alg_res$isClusterMember,
  as.character(clust_alg_res$cluster),
  "No_cluster"
)


msi_df$cluster <- clust_alg_res$cluster_corrected





df <- msi_df
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























