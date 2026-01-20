library(xml2)
library(dplyr)
library(purrr)
library(sp)
library(stringr)


bp <- parallel::detectCores() - 2
setCardinalParallel(workers = bp)


temp_imzml <- "new_data/08052018_slideN1/08052018_slideN1.imzML"



msi_data <- readImzML(temp_imzml, memory = FALSE, check = FALSE,
                      mass.range = NULL, resolution = 10, units = c("ppm"),
                      guess.max = 1000L, as = "auto", parse.only=FALSE,
                      verbose = getCardinalVerbose(), chunkopts = list(),
                      BPPARAM = bpparam())

remove_region_from_msi <- function(mse,
                                   spotlist_path,
                                   region = "05",
                                   keep = FALSE) {
  stopifnot(file.exists(spotlist_path))
  
  # --- 1) læs spotlisten (samme logik som før) ---
  spots <- read.table(
    spotlist_path,
    comment.char = "#",
    stringsAsFactors = FALSE
  )
  
  colnames(spots)[1:4] <- c("stage_x","stage_y","spot_name","region")
  
  # VIGTIG FIX: behold leading zero
  spots$region <- sprintf("%02d", as.integer(spots$region))
  
  # hvis du ikke allerede sætter colnames selv:
  # spotlisten plejer at være: stage_x stage_y spot_name region
  if (ncol(spots) < 4) stop("Spotlisten ser ud til at have < 4 kolonner.")
  colnames(spots)[1:4] <- c("stage_x", "stage_y", "spot_name", "region")
  
  # --- 2) parse gx/gy ud af spot_name: fx R04X605Y107 ---
  # tager de tal, der står efter X og Y
  m <- regexec("X(\\d+)Y(\\d+)", spots$spot_name)
  hit <- regmatches(spots$spot_name, m)
  
  ok <- lengths(hit) == 3
  spots <- spots[ok, , drop = FALSE]
  hit <- hit[ok]
  
  spots$gx <- as.integer(vapply(hit, `[`, "", 2))
  spots$gy <- as.integer(vapply(hit, `[`, "", 3))
  
  # --- 3) vælg coords for den region du vil fjerne/holde ---
  coords <- unique(spots[spots$region == region, c("gx", "gy")])
  if (nrow(coords) == 0) {
    warning(sprintf("Ingen pixels fundet for region '%s' i spotlisten.", region))
    return(mse)
  }
  
  # Cardinal forventer coord-kolonnenavne der matcher pixelData (typisk x,y)
  colnames(coords) <- c("x", "y")
  
  # --- 4) find pixel-indeks i Cardinal ud fra coordinates ---
  # pixels() finder kolonneindeks der matcher coord (se Cardinal docs) :contentReference[oaicite:1]{index=1}
  idx <- Cardinal::pixels(mse, coord = coords)
  
  if (length(idx) == 0) {
    warning("Fandt 0 matchende pixels i mse (tjek at coord-systemerne matcher).")
    return(mse)
  }
  
  # --- 5) drop eller keep ---
  if (isTRUE(keep)) {
    out <- mse[, idx, drop = FALSE]
  } else {
    keep_idx <- setdiff(seq_len(ncol(mse)), idx)
    out <- mse[, keep_idx, drop = FALSE]
  }
  
  out
}


msi_data <- remove_region_from_msi(msi_data, 
                                   "new_data/08052018_slideN1/08052018_slideN1_SPOTLIST.txt", 
                                   region="05", 
                                   keep=FALSE)


control_mean <- summarizeFeatures(msi_data, "mean")



mz_ref <- read.table("new_data/On-tissue_peaklist.txt", header = TRUE)
mz_ref <- mz_ref$Centroid

snr = 3
tolerance = 0.5

control_MSI_ref <- control_mean %>%
  peakPick(SNR = snr) %>%
  peakAlign(ref = mz_ref, tolerance = tolerance, units = "mz") %>%
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


image(msi_data_binned, mz = 978.518)






mis_path  <- "new_data/08052018_slideN1/08052018_slideN1.mis"
spot_path <- "new_data/08052018_slideN1/08052018_slideN1_SPOTLIST.txt"
# ---- spots with grid_x/grid_y ----
spots <- read.table(
  spot_path,
  comment.char="#",
  col.names=c("stage_x","stage_y","spot_name","region"),
  stringsAsFactors=FALSE
) %>%
  mutate(
    region = sprintf("%02d", as.integer(region)),
    gx = as.integer(str_match(spot_name, "X(\\d+)")[,2]),
    gy = as.integer(str_match(spot_name, "Y(\\d+)")[,2])
  )

# ---- areas from .mis (MIS coords) ----
doc <- read_xml(mis_path)

areas <- xml_find_all(doc, ".//Area")
area_df <- lapply(areas, function(a){
  nm <- xml_attr(a, "Name")
  pts <- xml_text(xml_find_all(a, "./Point"))
  xy  <- do.call(rbind, strsplit(pts, ",")) |> apply(2, as.numeric)
  data.frame(region=nm, x=xy[,1], y=xy[,2])
}) |> bind_rows()

# bbox per region in MIS
area_bbox <- area_df %>%
  group_by(region) %>%
  summarise(xmin=min(x), xmax=max(x), ymin=min(y), ymax=max(y), .groups="drop")

# bbox per region in GRID
grid_bbox <- spots %>%
  group_by(region) %>%
  summarise(gxmin=min(gx), gxmax=max(gx), gymin=min(gy), gymax=max(gy), .groups="drop")

bb <- inner_join(grid_bbox, area_bbox, by="region")
print(bb$region)  # sanity: should show 01..05

# ---- build corner correspondences ----
corr <- bb %>%
  rowwise() %>%
  do(data.frame(
    gx = c(.$gxmin, .$gxmax, .$gxmin, .$gxmax),
    gy = c(.$gymin, .$gymin, .$gymax, .$gymax),
    mx = c(.$xmin,  .$xmax,  .$xmin,  .$xmax),
    my = c(.$ymin,  .$ymin,  .$ymax,  .$ymax)
  )) %>%
  ungroup()

# ---- fit affine: [mx,my] = A*[gx,gy] + b ----
X <- cbind(corr$gx, corr$gy, 1)
coef_mx <- solve(t(X) %*% X, t(X) %*% corr$mx)
coef_my <- solve(t(X) %*% X, t(X) %*% corr$my)

A <- rbind(coef_mx[1:2], coef_my[1:2])
b <- c(coef_mx[3], coef_my[3])

# transform all spots
M <- t(A %*% t(as.matrix(spots[,c("gx","gy")])) + b)
spots$mx <- M[,1]
spots$my <- M[,2]


# ROIs
roi_nodes <- xml_find_all(doc, ".//ROI")
rois <- lapply(roi_nodes, function(r){
  nm <- xml_attr(r, "Name")
  pts <- xml_text(xml_find_all(r, "./Point"))
  xy  <- do.call(rbind, strsplit(pts, ",")) |> apply(2, as.numeric)
  data.frame(roi=nm, x=xy[,1], y=xy[,2])
}) |> bind_rows()

ggplot() +
  geom_point(data=spots, aes(mx, my), size=0.2, alpha=0.35) +
  geom_polygon(data=rois, aes(x, y, group=roi), fill=NA, color="red", linewidth=0.25) +
  coord_fixed() +
  theme_minimal()



assign_roi_from_mis <- function(spotlist_path,
                                mis_path,
                                include_boundary = TRUE,
                                drop_regions = NULL) {

  
  # -----------------------
  # 1) Read spotlist + parse grid coords from spot_name
  # -----------------------
  spots <- read.table(
    spotlist_path,
    comment.char = "#",
    col.names = c("stage_x", "stage_y", "spot_name", "region"),
    stringsAsFactors = FALSE
  )
  
  spots <- dplyr::as_tibble(spots) |>
    dplyr::mutate(
      # IMPORTANT: keep leading zeros
      region = sprintf("%02d", as.integer(region)),
      gx = as.integer(stringr::str_match(spot_name, "X(\\d+)")[, 2]),
      gy = as.integer(stringr::str_match(spot_name, "Y(\\d+)")[, 2])
    )
  
  if (any(is.na(spots$gx)) || any(is.na(spots$gy))) {
    stop("Kunne ikke parse gx/gy fra spot_name. Forventer mønster som '...X605Y107'.")
  }
  
  # -----------------------
  # 1b) Optionally drop whole regions (e.g. '05')
  # -----------------------
  if (!is.null(drop_regions)) {
    drop_regions <- sprintf("%02d", as.integer(drop_regions))
    n_before <- nrow(spots)
    spots <- dplyr::filter(spots, !(region %in% drop_regions))
    n_after <- nrow(spots)
    
    message(sprintf(
      "Dropped %d pixels from regions: %s",
      n_before - n_after,
      paste(drop_regions, collapse = ", ")
    ))
    
    if (n_after == 0) stop("Efter drop_regions er der 0 pixels tilbage.")
  }
  
  # -----------------------
  # 2) Read .mis and extract Areas (for affine fit)
  # -----------------------
  doc <- xml2::read_xml(mis_path)
  
  area_nodes <- xml2::xml_find_all(doc, ".//Area")
  if (length(area_nodes) == 0) stop("Ingen <Area> fundet i .mis. Kan ikke fitte affine transform.")
  
  areas <- lapply(area_nodes, function(a) {
    nm <- xml2::xml_attr(a, "Name")
    pts <- xml2::xml_text(xml2::xml_find_all(a, "./Point"))
    xy  <- do.call(rbind, strsplit(pts, ",")) |> apply(2, as.numeric)
    data.frame(region = nm, x = xy[, 1], y = xy[, 2])
  }) |> dplyr::bind_rows()
  
  area_bbox <- areas |>
    dplyr::group_by(region) |>
    dplyr::summarise(
      xmin = min(x), xmax = max(x),
      ymin = min(y), ymax = max(y),
      .groups = "drop"
    )
  
  # -----------------------
  # 3) Build correspondences (GRID bbox corners -> MIS bbox corners)
  # -----------------------
  grid_bbox <- spots |>
    dplyr::group_by(region) |>
    dplyr::summarise(
      gxmin = min(gx), gxmax = max(gx),
      gymin = min(gy), gymax = max(gy),
      .groups = "drop"
    )
  
  bb <- dplyr::inner_join(grid_bbox, area_bbox, by = "region")
  
  # If you dropped regions, make sure we still have enough for a stable fit
  if (nrow(bb) < 2) {
    stop("For få regions overlap mellem SPOTLIST og .mis Areas til et stabilt fit efter drop_regions.")
  }
  
  corr <- bb |>
    dplyr::rowwise() |>
    dplyr::do(data.frame(
      gx = c(.$gxmin, .$gxmax, .$gxmin, .$gxmax),
      gy = c(.$gymin, .$gymin, .$gymax, .$gymax),
      mx = c(.$xmin,  .$xmax,  .$xmin,  .$xmax),
      my = c(.$ymin,  .$ymin,  .$ymax,  .$ymax)
    )) |>
    dplyr::ungroup()
  
  # -----------------------
  # 4) Fit affine: [mx,my] = A*[gx,gy] + b  (least squares)
  # -----------------------
  X <- cbind(corr$gx, corr$gy, 1)
  
  coef_mx <- qr.solve(X, corr$mx)
  coef_my <- qr.solve(X, corr$my)
  
  A <- rbind(coef_mx[1:2], coef_my[1:2])  # 2x2
  b <- c(coef_mx[3],    coef_my[3])       # 2
  
  M <- t(A %*% t(as.matrix(spots[, c("gx", "gy")])) + b)
  spots$mx <- M[, 1]
  spots$my <- M[, 2]
  
  # -----------------------
  # 5) Extract ROIs (annotation polygons) from .mis
  # -----------------------
  roi_nodes <- xml2::xml_find_all(doc, ".//ROI")
  if (length(roi_nodes) == 0) stop("Ingen <ROI> fundet i .mis. Ingen annotationer at tildele.")
  
  rois <- lapply(roi_nodes, function(r) {
    nm  <- xml2::xml_attr(r, "Name")
    pts <- xml2::xml_text(xml2::xml_find_all(r, "./Point"))
    xy  <- do.call(rbind, strsplit(pts, ",")) |> apply(2, as.numeric)
    data.frame(roi = nm, x = xy[, 1], y = xy[, 2])
  }) |> dplyr::bind_rows()
  
  roi_list <- split(rois, rois$roi)
  
  # -----------------------
  # 6) Point-in-polygon (include boundary if requested)
  # sp::point.in.polygon returns:
  # 0 = outside, 1 = inside, 2 = on edge, 3 = on vertex
  # -----------------------
  inside_fun <- function(poly) {
    pip <- sp::point.in.polygon(spots$mx, spots$my, poly$x, poly$y)
    if (include_boundary) pip >= 1 else pip == 1
  }
  
  roi_mat <- vapply(roi_list, inside_fun, logical(nrow(spots)))
  
  # assign first ROI hit (if multiple)
  spots$roi <- apply(roi_mat, 1, function(v) {
    if (!any(v)) NA_character_ else names(which(v))[1]
  })
  
  as.data.frame(spots)
}


dftest <- assign_roi_from_mis(
  spotlist_path = "new_data/08052018_slideN1/08052018_slideN1_SPOTLIST.txt",
  mis_path      = "new_data/08052018_slideN1/08052018_slideN1.mis",
  drop_regions = "05",
  include_boundary = TRUE
)

ggplot(dftest, aes(gx, gy, color = !is.na(roi))) +
  geom_point(size=0.4) +
  coord_fixed() +
  scale_y_reverse() 

sum(is.na(dftest$roi))

dftest <- dftest[!is.na(dftest$roi), ]

standardize_roi_class <- function(roi_vec, strict = TRUE,
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
  
  # Squamous
  if (squamous_as == "Healthy") {
    mapped[cls %in% c("squamous")] <- "Healthy"
  } else if (squamous_as == "Squamous") {
    mapped[cls %in% c("squamous")] <- "Squamous"
  } else {
    mapped[cls %in% c("squamous")] <- NA_character_
  }
  
  out[ok] <- mapped
  
  if (isTRUE(strict)) {
    unmapped <- unique(x[ok][is.na(mapped)])
    if (length(unmapped) > 0) {
      warning("Fandt ROI-navne som ikke blev mappet til en klasse: ",
              paste(unmapped, collapse = ", "))
    }
  }
  
  out
}



dftest$roi_class <- standardize_roi_class(dftest$roi, strict = TRUE, squamous_as = "Squamous")



dftest$roi_class_plot <- ifelse(is.na(dftest$roi_class), "Unannotated", dftest$roi_class)

ggplot(dftest, aes(x = gx, y = gy, color = roi_class_plot)) +
  geom_point(size = 0.5, alpha = 0.9) +
  coord_fixed() +
  scale_y_reverse() +  # matcher "image"-konvention (øverst = lav y)
  labs(x = "Grid X", y = "Grid Y", color = "Annotation") +
  theme_minimal()




add_ground_truth_from_dftest <- function(msi_df_gt,
                                         dftest,
                                         gt_col = "roi_class_plot") {
  
  # --- sanity checks ---
  req1 <- c("x", "y")
  req2 <- c("gx", "gy", gt_col)
  
  if (!all(req1 %in% colnames(msi_df_gt))) {
    stop("msi_df_gt skal indeholde kolonnerne: ", paste(req1, collapse=", "))
  }
  if (!all(req2 %in% colnames(dftest))) {
    stop("dftest skal indeholde kolonnerne: ", paste(req2, collapse=", "))
  }
  
  # --- lav nøglekolonner ---
  msi_df_gt$.key <- paste(msi_df_gt$x, msi_df_gt$y)
  dftest$.key    <- paste(dftest$gx, dftest$gy)
  
  # --- reducer dftest til kun det nødvendige ---
  gt_map <- dftest[, c(".key", gt_col)]
  colnames(gt_map)[2] <- "ground_truth"
  
  # --- join ---
  out <- merge(
    msi_df_gt,
    gt_map,
    by = ".key",
    all.x = TRUE,
    sort = FALSE
  )
  
  # --- ryd op ---
  out$.key <- NULL
  
  # --- QC ---
  n_matched <- sum(!is.na(out$ground_truth))
  message("Matched ground truth for ", n_matched, " / ", nrow(out), " pixels")
  
  out
}

key_dftest <- paste(dftest$gx, dftest$gy)
key_cor <- paste(cor_data$x, cor_data$y)

dftest <- dftest[key_dftest %in% key_cor, ]

msi_df_gt <- add_ground_truth_from_dftest(
  msi_df_gt = msi_df_gt,
  dftest    = dftest,
  gt_col    = "roi_class_plot"
)


table(msi_df_gt$ground_truth, msi_df_gt$MSIClust_xy_cluster)








remove_unassigned_pixels <- function(msi_obj, dftest, unassigned_label = "Unassigned") {
  # check kolonner
  if (!all(c("gx","gy","roi_class_plot") %in% colnames(dftest))) {
    stop("dftest skal have kolonnerne: gx, gy, roi_class_plot")
  }
  
  # coords der skal fjernes
  rm_xy <- unique(dftest[dftest$roi_class_plot == unassigned_label, c("gx","gy")])
  
  if (nrow(rm_xy) == 0) {
    message("Ingen '", unassigned_label, "' pixels fundet. Returnerer uændret objekt.")
    return(msi_obj)
  }
  
  # Cardinal coords (samme grid-space)
  xy <- as.data.frame(Cardinal::coord(msi_obj))
  colnames(xy) <- c("gx","gy")
  
  # match via keys (hurtigt og robust)
  key_img <- paste(xy$gx, xy$gy)
  key_rm  <- paste(rm_xy$gx, rm_xy$gy)
  
  keep <- !(key_img %in% key_rm)
  
  message("Fjerner ", sum(!keep), " pixels mærket '", unassigned_label, "'")
  
  # subsetPixels med indeks (pixel-rækkefølge bevares)
  Cardinal::subsetPixels(msi_obj, which(keep))
}



msi_data <- remove_unassigned_pixels(msi_data, dftest, unassigned_label = "Unannotated")
