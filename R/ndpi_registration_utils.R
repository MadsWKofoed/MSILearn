# R/ndpi_registration_utils.R

fit_affine_ndpi_to_msi <- function(ndpi_xy, msi_xy) {
  stopifnot(ncol(ndpi_xy) == 2, ncol(msi_xy) == 2, nrow(ndpi_xy) == nrow(msi_xy))
  n <- nrow(ndpi_xy)
  if (n < 3) {
    return(list(valid = FALSE, reason = "Need at least 3 point pairs."))
  }

  x <- ndpi_xy[, 1]
  y <- ndpi_xy[, 2]
  X <- cbind(x, y, 1)

  rank_ok <- qr(X)$rank >= 3
  if (!rank_ok) {
    return(list(valid = FALSE, reason = "Landmarks are collinear or degenerate."))
  }

  bx <- lm.fit(x = X, y = msi_xy[, 1])$coefficients
  by <- lm.fit(x = X, y = msi_xy[, 2])$coefficients

  A <- matrix(c(bx[1], bx[2], by[1], by[2]), nrow = 2, byrow = TRUE)
  b <- c(bx[3], by[3])

  pred <- cbind(
    X[, 1] * bx[1] + X[, 2] * bx[2] + bx[3],
    X[, 1] * by[1] + X[, 2] * by[2] + by[3]
  )
  err <- pred - msi_xy
  rms <- sqrt(mean(rowSums(err^2)))

  list(
    valid = TRUE,
    A = A,
    b = b,
    rms = rms,
    n_pairs = n
  )
}

apply_affine_xy <- function(xy, A, b) {
  t((A %*% t(xy)) + b)
}

oriented_to_original_xy <- function(xy, orientation, x_min, x_max, y_min, y_max) {
  out <- xy
  if (orientation %in% c("Flip X", "Flip Both")) {
    out[, 1] <- x_max + x_min - out[, 1]
  }
  if (orientation %in% c("Flip Y", "Flip Both")) {
    out[, 2] <- y_max + y_min - out[, 2]
  }
  out
}

assign_polygon_to_pixel_classes <- function(base_df, poly_xy_original, class_label, class_vec) {
  inside <- sp::point.in.polygon(
    point.x = base_df$x,
    point.y = base_df$y,
    pol.x = poly_xy_original[, 1],
    pol.y = poly_xy_original[, 2]
  ) > 0

  if (sum(inside) == 0) {
    return(list(class_vec = class_vec, n_updated = 0L))
  }

  class_vec[inside] <- class_label
  list(class_vec = class_vec, n_updated = sum(inside))
}