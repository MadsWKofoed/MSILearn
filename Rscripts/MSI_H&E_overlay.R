library(Cardinal)
library(tiff) 
library(base64enc)
library(plotly)
library(png)


bp <- parallel::detectCores() - 2
setCardinalParallel(workers = bp)


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


coords <- read.csv("kidney_and_tumor/tumor_roi_points_2025-12-20.csv", header = TRUE)
roi_coords <- coords[, c("x", "y")]

msi_coords <- coord(msi_data)

keep <- paste(msi_coords$x, msi_coords$y) %in%
  paste(roi_coords$x, roi_coords$y)

msi_roi <- subsetPixels(msi_data, keep)


image(
  msi_data_binned,
  superpose = TRUE,
  mz = c(830.56115504286, 798.499218444455, 369.264678207859),
  tol = 0.5,
  unit = "mz",
  normalize.image = "linear",
  col = c("red", "green", "blue")
)
image(msi_roi)


RNGkind("L'Ecuyer-CMRG") 
mse_mean <- summarizeFeatures(msi_roi, "mean")

mz_ref <- mzvals

snr = 3
tolerance = 0.5

control_MSI_ref <- mse_mean %>%
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


norm_binned <- normalize(msi_data_binned,
                         method = "tic") %>% process()



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



make_raster_png_multi_mz <- function(
    df,
    mz_cols,                # fx c("mz_830.56", "mz_885.54")
    colors,                 # fx c("red", "green")
    alpha_max = 0.4,
    clip_quantiles = c(0.01, 0.99),
    na_transparent = TRUE
) {
  stopifnot(length(mz_cols) == length(colors))
  
  # Reindex coords
  x <- df$x - min(df$x, na.rm = TRUE) + 1L
  y <- df$y - min(df$y, na.rm = TRUE) + 1L
  
  width  <- max(x, na.rm = TRUE)
  height <- max(y, na.rm = TRUE)
  
  # Prepare empty RGBA accumulators
  R <- matrix(0, nrow = height, ncol = width)
  G <- matrix(0, nrow = height, ncol = width)
  B <- matrix(0, nrow = height, ncol = width)
  A <- matrix(0, nrow = height, ncol = width)
  
  # Loop over m/z channels
  for (i in seq_along(mz_cols)) {
    
    vals <- as.numeric(df[[mz_cols[i]]])
    
    mat <- matrix(NA_real_, nrow = height, ncol = width)
    mat[cbind(height - y + 1L, x)] <- vals
    
    # Robust clipping
    v <- mat[is.finite(mat)]
    if (length(v) == 0) next
    
    if (!is.null(clip_quantiles)) {
      qs <- stats::quantile(v, probs = clip_quantiles, na.rm = TRUE, names = FALSE)
      mat <- pmin(pmax(mat, qs[1]), qs[2])
    }
    
    # Normalize 0..1
    rng <- range(mat, na.rm = TRUE)
    if (!all(is.finite(rng)) || rng[2] <= rng[1]) next
    mat01 <- (mat - rng[1]) / (rng[2] - rng[1])
    
    # Get base RGB color
    col_rgb <- grDevices::col2rgb(colors[i]) / 255
    
    # Alpha contribution
    a <- alpha_max * mat01
    
    # Alpha compositing (over operator)
    R <- R * (1 - a) + col_rgb[1] * a
    G <- G * (1 - a) + col_rgb[2] * a
    B <- B * (1 - a) + col_rgb[3] * a
    A <- pmin(1, A + a)
  }
  
  if (!na_transparent) {
    A[A == 0] <- alpha_max
  }
  
  # Build RGBA array
  rgb_array <- array(0, dim = c(height, width, 4))
  rgb_array[,,1] <- R
  rgb_array[,,2] <- G
  rgb_array[,,3] <- B
  rgb_array[,,4] <- A
  
  tmp <- tempfile(fileext = ".png")
  png::writePNG(rgb_array, target = tmp)
  base64enc::dataURI(file = tmp, mime = "image/png")
}


make_raster_png_multi_mz_mix <- function(
    df,
    mz_cols,
    colors,
    alpha_max = 0.4,
    clip_quantiles = c(0.01, 0.99),
    threshold = 0.05          # <- NY: skjul svage signaler (0..1)
) {
  stopifnot(length(mz_cols) == length(colors))
  
  x <- df$x - min(df$x, na.rm = TRUE) + 1L
  y <- df$y - min(df$y, na.rm = TRUE) + 1L
  W <- max(x, na.rm = TRUE)
  H <- max(y, na.rm = TRUE)
  
  col_rgb <- t(grDevices::col2rgb(colors) / 255)  # K x 3
  K <- length(mz_cols)
  
  weights <- array(0, dim = c(H, W, K))
  
  for (k in seq_len(K)) {
    mat <- matrix(NA_real_, nrow = H, ncol = W)
    mat[cbind(H - y + 1L, x)] <- as.numeric(df[[mz_cols[k]]])
    
    v <- mat[is.finite(mat)]
    if (length(v) == 0) next
    
    if (!is.null(clip_quantiles)) {
      qs <- stats::quantile(v, probs = clip_quantiles, na.rm = TRUE, names = FALSE)
      mat <- pmin(pmax(mat, qs[1]), qs[2])
    }
    
    rng <- range(mat, na.rm = TRUE)
    if (!all(is.finite(rng)) || rng[2] <= rng[1]) next
    
    w <- (mat - rng[1]) / (rng[2] - rng[1])
    w[!is.finite(w)] <- 0
    
    # 🔇 Threshold: fjern svag støj
    w[w < threshold] <- 0
    
    weights[,,k] <- w
  }
  
  # Sum af weights
  wsum <- apply(weights, c(1,2), sum)
  wsum_safe <- ifelse(wsum == 0, 1, wsum)
  
  # Farveblanding (order-uafhængig)
  R <- apply(sweep(weights, 3, col_rgb[,1], `*`), c(1,2), sum) / wsum_safe
  G <- apply(sweep(weights, 3, col_rgb[,2], `*`), c(1,2), sum) / wsum_safe
  B <- apply(sweep(weights, 3, col_rgb[,3], `*`), c(1,2), sum) / wsum_safe
  
  # Alpha (overlap-forstærkning)
  A <- 1 - apply(1 - alpha_max * weights, c(1,2), prod)
  A[wsum == 0] <- 0
  
  rgba <- array(0, dim = c(H, W, 4))
  rgba[,,1] <- R
  rgba[,,2] <- G
  rgba[,,3] <- B
  rgba[,,4] <- A
  
  tmp <- tempfile(fileext = ".png")
  png::writePNG(rgba, target = tmp)
  base64enc::dataURI(file = tmp, mime = "image/png")
}





uri <- make_raster_png_multi_mz_mix(
  msi_df,
  mz_cols = c("mz_830.56115504286", "mz_798.499218444455", "mz_369.264678207859"),
  colors  = c("red", "green", "blue"),
  alpha_max = 0.6,
  threshold = 0.1
)


# uri <- make_raster_png_intensity(msi_df, "mz_830.56115504286", alpha_max = 0.3)

img_x <- 1
img_y <- max(msi_df$y - min(msi_df$y) + 1)  # top-kant i y
img_sizex <- max(msi_df$x - min(msi_df$x) + 1)
img_sizey <- max(msi_df$y - min(msi_df$y) + 1)

x_range <- c(1, img_sizex)
y_range <- c(1, img_sizey)






he_tif_path  <- "kidney_and_tumor/OPTICAL_tumor.tif"  

he <- readTIFF(he_tif_path)  
dim(he)



# Bilinear sampler for 2D matrix
.bilinear2 <- function(M, x, y) {
  nr <- nrow(M); nc <- ncol(M)
  
  x0 <- floor(x); x1 <- x0 + 1
  y0 <- floor(y); y1 <- y0 + 1
  
  x0 <- pmin(pmax(x0, 1), nc); x1 <- pmin(pmax(x1, 1), nc)
  y0 <- pmin(pmax(y0, 1), nr); y1 <- pmin(pmax(y1, 1), nr)
  
  Ia <- M[cbind(y0, x0)]
  Ib <- M[cbind(y0, x1)]
  Ic <- M[cbind(y1, x0)]
  Id <- M[cbind(y1, x1)]
  
  wa <- (x1 - x) * (y1 - y)
  wb <- (x - x0) * (y1 - y)
  wc <- (x1 - x) * (y - y0)
  wd <- (x - x0) * (y - y0)
  
  Ia*wa + Ib*wb + Ic*wc + Id*wd
}

# Resize RGB array [h,w,3]
.resize_rgb <- function(img, new_h, new_w) {
  h <- dim(img)[1]; w <- dim(img)[2]
  xs <- seq(1, w, length.out = new_w)
  ys <- seq(1, h, length.out = new_h)
  grid <- expand.grid(x = xs, y = ys)
  
  out <- array(0, dim = c(new_h, new_w, 3))
  for (ch in 1:3) {
    vals <- .bilinear2(img[,,ch], grid$x, grid$y)
    out[,,ch] <- matrix(vals, nrow = new_h, ncol = new_w, byrow = TRUE)
  }
  out
}

# Rotate RGB array (beholder canvas) med hvid baggrund
.rotate_rgb <- function(img, angle_deg, bg = c(1,1,1)) {
  if (angle_deg %% 360 == 0) return(img)
  
  h <- dim(img)[1]; w <- dim(img)[2]
  a <- angle_deg * pi/180
  cx <- (w + 1) / 2
  cy <- (h + 1) / 2
  
  xs <- rep(1:w, times = h)
  ys <- rep(1:h, each  = w)
  
  x <- xs - cx
  y <- ys - cy
  
  # inverse rotation (output -> input)
  x_in <-  cos(a) * x + sin(a) * y + cx
  y_in <- -sin(a) * x + cos(a) * y + cy
  
  inside <- x_in >= 1 & x_in <= w & y_in >= 1 & y_in <= h
  
  out <- array(0, dim = c(h, w, 3))
  for (ch in 1:3) {
    channel <- rep(bg[ch], length(xs))
    channel[inside] <- .bilinear2(img[,,ch], x_in[inside], y_in[inside])
    out[,,ch] <- matrix(channel, nrow = h, ncol = w, byrow = TRUE)
  }
  out
}

he_transform <- function(he, scale = 1, rotate = 0, flip_x = FALSE, flip_y = FALSE) {
  # he: fra readTIFF(); kan være [h,w] eller [h,w,c]
  if (length(dim(he)) == 2) {
    he_rgb <- array(0, dim = c(nrow(he), ncol(he), 3))
    he_rgb[,,1] <- he; he_rgb[,,2] <- he; he_rgb[,,3] <- he
  } else {
    he_rgb <- he
    if (dim(he_rgb)[3] >= 4) he_rgb <- he_rgb[,,1:3, drop = FALSE]
  }
  
  # Sikr 0..1
  mx <- max(he_rgb, na.rm = TRUE)
  if (is.finite(mx) && mx > 1) he_rgb <- he_rgb / 255
  
  # Flip
  if (flip_x) he_rgb <- he_rgb[, ncol(he_rgb):1, , drop = FALSE]  # left/right
  if (flip_y) he_rgb <- he_rgb[nrow(he_rgb):1, , , drop = FALSE]  # up/down
  
  # Scale
  if (!isTRUE(all.equal(scale, 1))) {
    new_h <- max(1, round(dim(he_rgb)[1] * scale))
    new_w <- max(1, round(dim(he_rgb)[2] * scale))
    he_rgb <- .resize_rgb(he_rgb, new_h, new_w)
  }
  
  # Rotate
  if (!isTRUE(all.equal(rotate, 0))) {
    he_rgb <- .rotate_rgb(he_rgb, rotate, bg = c(1,1,1))
  }
  
  he_rgb
}


he_to_uri <- function(he_rgb, alpha = 1) {
  h <- dim(he_rgb)[1]; w <- dim(he_rgb)[2]
  rgba <- array(0, dim = c(h, w, 4))
  rgba[,,1:3] <- he_rgb
  rgba[,,4] <- alpha
  
  tmp <- tempfile(fileext = ".png")
  png::writePNG(rgba, target = tmp)
  base64enc::dataURI(file = tmp, mime = "image/png")
}


he_rgb_aligned <- he_transform(
  he,
  scale  = 0.2,
  rotate = 0,
  flip_x = FALSE,
  flip_y = TRUE
)

he_uri <- he_to_uri(he_rgb_aligned, alpha = 0.6)




# MSI canvas size (samme som i din raster-funktion)
W <- max(msi_df$x - min(msi_df$x) + 1)
H <- max(msi_df$y - min(msi_df$y) + 1)

x_range <- c(1, W)
y_range <- c(1, H)


he_params <- list(
  x = 0,        # flyt vandret
  y = H + 5,        # flyt lodret (typisk top-kant hvis y går op)
  sizex = W * 1.13,    # skalér i x
  sizey = H * 1.13    # skalér i y
  # rotation: se note længere nede
)




 # he_uri baggrund
msi_uri <- uri                            

p <- plot_ly(source = "overlay") %>%
  add_trace(type = "scatter", mode = "markers",
            x = numeric(0), y = numeric(0),
            hoverinfo = "skip", showlegend = FALSE) %>%
  layout(
    images = list(
      # 1) H&E nederst
      list(
        source = he_uri,
        xref = "x", yref = "y",
        x = he_params$x, y = he_params$y,
        sizex = he_params$sizex, sizey = he_params$sizey,
        sizing = "stretch",
        layer = "below"
      ),
      # 2) MSI ovenpå
      list(
        source = msi_uri,
        xref = "x", yref = "y",
        x = 1, y = H,
        sizex = W, sizey = H,
        sizing = "stretch",
        layer = "above"
      )
    ),
    xaxis = list(range = x_range, title = "x"),
    yaxis = list(range = y_range, title = "y", scaleanchor = "x", scaleratio = 1),
    showlegend = FALSE
  ) %>%
  config(displaylogo = FALSE)

p

focus_plot <- function(p, x, y) {
  p %>% layout(
    xaxis = list(range = x),
    yaxis = list(range = y, scaleanchor="x", scaleratio=1)
  )
}

p %>% focus_plot(c(-10, 85), c(-10, 80))

par(mfrow = c(1, 2))

# MSI image by itself
p_msi <- plot_ly(source = "msi") %>%
  add_trace(type = "scatter", mode = "markers",
            x = numeric(0), y = numeric(0),
            hoverinfo = "skip", showlegend = FALSE) %>%
  layout(
    images = list(list(
      source = uri,          # din MSI data URI
      xref = "x", yref = "y",
      x = img_x, y = img_y,  # typisk x_min, y_max (se note)
      sizex = img_sizex,     # typisk (x_max - x_min + 1)
      sizey = img_sizey,     # typisk (y_max - y_min + 1)
      sizing = "stretch",
      layer = "below"
    )),
    dragmode = "drawclosedpath",
    newshape = list(
      line = list(color = "black", width = 1),
      fillcolor = "rgba(0,0,0,0.05)"
    ),
    xaxis = list(range = x_range, title = "x"),
    yaxis = list(range = y_range, title = "y", scaleanchor = "x", scaleratio = 1),
    showlegend = FALSE
  ) %>%
  config(
    displaylogo = FALSE,
    modeBarButtonsToAdd = list("drawclosedpath", "eraseshape"),
    modeBarButtonsToRemove = c("hoverClosestCartesian","hoverCompareCartesian",
                               "toggleSpikelines","toImage","select2d","lasso2d")
  )

p_msi %>% focus_plot(c(-10, 85), c(-10, 80))

# H&E image by itself
p_he <- plot_ly(source = "he") %>%
  add_trace(type = "scatter", mode = "markers",
            x = numeric(0), y = numeric(0),
            hoverinfo = "skip", showlegend = FALSE) %>%
  layout(
    images = list(list(
      source = he_uri,          
      xref = "x", yref = "y",
      x = he_params$x, y = he_params$y,
      sizex = he_params$sizex, 
      sizey = he_params$sizey,    
      sizing = "stretch",
      layer = "below"
    )),
    dragmode = "drawclosedpath",
    newshape = list(
      line = list(color = "black", width = 1),
      fillcolor = "rgba(0,0,0,0.05)"
    ),
    xaxis = list(range = x_range, title = "x"),
    yaxis = list(range = y_range, title = "y", scaleanchor = "x", scaleratio = 1),
    showlegend = FALSE
  ) %>%
  config(
    displaylogo = FALSE,
    modeBarButtonsToAdd = list("drawclosedpath", "eraseshape"),
    modeBarButtonsToRemove = c("hoverClosestCartesian","hoverCompareCartesian",
                               "toggleSpikelines","toImage","select2d","lasso2d")
  )

p_he %>% focus_plot(c(-10, 85), c(-10, 80))



he_uri <- he_to_uri(he_rgb_aligned, alpha = 0.99)
msi_uri <- uri

p <- plot_ly(source = "overlay") %>%
  add_trace(
    type = "scatter", mode = "markers",
    x = numeric(0), y = numeric(0),
    hoverinfo = "skip", showlegend = FALSE
  ) %>%
  layout(
    images = list(
      # 1) MSI nederst (baggrund)
      list(
        source = msi_uri,
        xref = "x", yref = "y",
        x = 1, y = H,
        sizex = W, sizey = H,
        sizing = "stretch",
        layer = "below"
      ),
      # 2) H&E ovenpå (forgrund)
      list(
        source = he_uri,
        xref = "x", yref = "y",
        x = he_params$x, y = he_params$y,
        sizex = he_params$sizex, sizey = he_params$sizey,
        sizing = "stretch",
        layer = "above"
      )
    ),
    xaxis = list(range = x_range, title = "x"),
    yaxis = list(
      range = y_range,
      title = "y",
      scaleanchor = "x",
      scaleratio = 1
    ),
    showlegend = FALSE
  ) %>%
  config(displaylogo = FALSE)

p


