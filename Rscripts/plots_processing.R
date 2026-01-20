library(Cardinal)
library(matter)
library(dplyr)


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



# Image of the top 3 mz-intensities (based on variance)
norm_msi_binned <- normalize(msi_data_binned, method = "tic") %>% process()


var_intensity <- apply(as.matrix(spectra(msi_data_binned)), 1, var)
norm_var_intensity <- apply(as.matrix(spectra(norm_msi_binned)), 1, var)


top3_idx <- order(var_intensity, decreasing = TRUE)[1:3]
top3_mz <- mz(msi_data_binned)[top3_idx]

norm_top3_idx <- order(norm_var_intensity, decreasing = TRUE)[1:3]
norm_top3_mz <- mz(norm_msi_binned)[norm_top3_idx]

vizi_style("dark")

par(mfrow = c(1,2))
image(msi_data_binned, 
      mz = top3_mz,
      superpose = TRUE,
      contrast.enhance = "suppress",
      normalize.image = "linear", 
      col = c("blue", "red", "green")
)

image(norm_msi_binned, 
      mz = norm_top3_mz,
      superpose = TRUE,
      contrast.enhance = "suppress",
      normalize.image = "linear", 
      col = c("blue", "red", "green")
      )




# Sample plot (x-axis: pixel distance, y-axis: distance measure/covariance)
n_pairs <- 10000
n <- nrow(norm_msi_matrix)

# sample i,j uafhængigt
pairs <- data.frame(
  i = sample(n, n_pairs, replace = TRUE),
  j = sample(n, n_pairs, replace = TRUE)
)

# fjern i==j
pairs <- subset(pairs, i != j)

# gør par ordensuafhængige (så (i,j) og (j,i) ikke tælles to gange)
pairs$ii <- pmin(pairs$i, pairs$j)
pairs$jj <- pmax(pairs$i, pairs$j)
pairs <- unique(pairs[, c("ii", "jj")])
names(pairs) <- c("i", "j")

# trim hvis flere end ønsket
if (nrow(pairs) > n_pairs) {
  pairs <- pairs[1:n_pairs, ]
}

# --- 2) BEREGN AFSTANDE ---

# 2A Koordinater
coords <- as.matrix(norm_msi_matrix[, c("x", "y")])

space_distance <- sqrt(
  rowSums(
    (coords[pairs$i, , drop = FALSE] -
       coords[pairs$j, , drop = FALSE])^2
  )
)

# 2B Intensiteter (alle m/z-kolonner der starter med "mz_")
mz_cols <- grep("^mz_", names(norm_msi_matrix), value = TRUE)
intens <- as.matrix(norm_msi_matrix[, mz_cols])


cosine_distance <- function(a, b) {
  sim <- sum(a*b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
  return(1 - sim)
}

intensity_distance <- mapply(
  function(i, j) cosine_distance(intens[i, ], intens[j, ]),
  pairs$i,
  pairs$j
)

# Euclidean distance on intensities
# diff_intens <- intens[pairs$i, , drop = FALSE] -
#   intens[pairs$j, , drop = FALSE]
# 
# intensity_distance <- sqrt(rowSums(diff_intens^2))

# Lav samlet dataframe
df_dist <- data.frame(
  space_distance = space_distance,
  intensity_distance = intensity_distance
)

# --- 3) BINNING OG SAMMENFATNING ---

nbins <- 50
df_binned <- df_dist %>%
  mutate(bin = cut(space_distance, breaks = nbins)) %>%
  group_by(bin) %>%
  summarise(
    space_mid   = mean(space_distance),
    int_median  = median(intensity_distance),
    int_mean    = mean(intensity_distance),
    int_q25     = quantile(intensity_distance, 0.25),
    int_q75     = quantile(intensity_distance, 0.75),
    .groups = "drop"
  )

# --- 4) PLOT ---

ggplot(df_binned, aes(x = space_mid, y = int_median)) +
  geom_line() +
  geom_ribbon(aes(ymin = int_q25, ymax = int_q75), alpha = 0.2) +
  theme_bw() +
  labs(
    x = "Euclidean distance between pixel coordinates",
    y = "Cosine Intensity-distance (median, 25–75% interval)",
    title = "Relation between spacial and intensity distance\n(10,000 randomly selected pixel-pairs)"
  )



df_plot <- data.frame(
  space_distance     = space_distance,
  intensity_distance = intensity_distance
)

ggplot(df_plot, aes(x = space_distance, y = intensity_distance)) +
  geom_point(alpha = 0.2, size = 1) +
  theme_bw() +
  labs(
    x = "Euclidean distance between pixel coordinates",
    y = "Cosine distance in m/z-intensities",
    title = "10,000 randomly selected pairs of pixels\nSpatial distance vs intensity distance"
  )





















msi_matrix <- make_msi_dataframe(msi_data_binned)
norm_msi_matrix <- make_msi_dataframe(norm_msi_binned)

n_sample_pixels <- 5000

pixel_sample <- norm_msi_matrix[sample(nrow(norm_msi_matrix), n_sample_pixels), ]

coords <- as.matrix(pixel_sample[, c("x", "y")])

mz_cols <- grep("^mz_", names(pixel_sample), value = TRUE)
intensities <- as.matrix(pixel_sample[, mz_cols])



x_axis <- dist(coords)
y_axis <- dist(intensities, method = "euclidean")


d_space_vec <- as.numeric(x_axis)
d_int_vec   <- as.numeric(y_axis)

df_dist <- data.frame(
  space_distance = d_space_vec,
  intensity_distance = d_int_vec
)


idx <- sample(seq_len(nrow(df_dist)), 50000)  
df_dist_plot <- df_dist[idx, ]

library(ggplot2)

ggplot(df_dist, aes(x = space_distance, y = intensity_distance)) +
  geom_point(alpha = 0.2) +
  theme_bw() +
  labs(
    x = "Distance mellem pixels (koord. rum)",
    y = "Distance i m/z-intensiteter",
    title = "Sammenhæng mellem rumlig afstand og intensitetsforskel (MSI)"
  )



nbins <- 100

df_binned <- df_dist %>%
  mutate(bin = cut(space_distance, breaks = nbins)) %>%
  group_by(bin) %>%
  summarise(
    space_mid   = mean(space_distance),
    int_median  = median(intensity_distance),
    int_mean    = mean(intensity_distance),
    int_q25     = quantile(intensity_distance, 0.25),
    int_q75     = quantile(intensity_distance, 0.75),
    .groups = "drop"
  )

ggplot(df_binned, aes(x = space_mid, y = int_median)) +
  geom_line() +
  geom_ribbon(aes(ymin = int_q25, ymax = int_q75), alpha = 0.2) +
  theme_bw() +
  labs(
    x = "Rumlig afstand mellem pixels",
    y = "Intensitets-distance (median, med 25–75% bånd)",
    title = "Sammenhæng mellem rumlig afstand og intensitetsforskel"
  )





n <- nrow(norm_msi_matrix)
n_pairs <- 10000

# sample i og j uafhængigt
pairs <- data.frame(
  i = sample(n, n_pairs, replace = TRUE),
  j = sample(n, n_pairs, replace = TRUE)
)

# fjern par hvor i == j
pairs <- subset(pairs, i != j)

# sørg for at (i,j) og (j,i) ikke tæller som to forskellige
pairs$ii <- pmin(pairs$i, pairs$j)
pairs$jj <- pmax(pairs$i, pairs$j)
pairs <- unique(pairs[, c("ii", "jj")])
names(pairs) <- c("i", "j")

# hvis vi ender med flere end 1000 unikke par, trim ned
if (nrow(pairs) > n_pairs) {
  pairs <- pairs[1:n_pairs, ]
}


coords <- as.matrix(norm_msi_matrix[, c("x", "y")])

space_dist <- sqrt(
  rowSums(
    (coords[pairs$i, , drop = FALSE] -
       coords[pairs$j, , drop = FALSE])^2
  )
)


mz_cols <- grep("^mz_", names(norm_msi_matrix), value = TRUE)

intens <- as.matrix(norm_msi_matrix[, mz_cols])

diff_intens <- intens[pairs$i, , drop = FALSE] -
  intens[pairs$j, , drop = FALSE]

intensity_dist <- sqrt(rowSums(diff_intens^2))


df_plot <- data.frame(
  space_distance     = space_dist,
  intensity_distance = intensity_dist
)

ggplot(df_plot, aes(x = space_distance, y = intensity_distance)) +
  geom_point(alpha = 0.4, size = 1) +
  theme_bw() +
  labs(
    x = "Distance mellem pixels (koord. afstand)",
    y = "Distance i m/z-intensitetsprofil",
    title = "1000 tilfældigt valgte pixel-par\nRumlig distance vs intensitetsdistance"
  )






