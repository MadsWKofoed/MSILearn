library(Cardinal)
library(matter)
library(dplyr)
# library(vsclust)
library(matrixStats)
library(plotly)
library(RColorBrewer)
library(EBImage)
library(sp)
library(SimpleITK)

bp <- parallel::detectCores() - 2
setCardinalParallel(workers = bp)

source("MSIreg_functions/ROI.R")
source("MSIreg_functions/conversion.R")
source("MSIreg_functions/coregister.R")
source("MSIreg_functions/entropy.R")
source("MSIreg_functions/grid.R")
source("MSIreg_functions/metrics.R")
source("MSIreg_functions/resize.R")
source("MSIreg_functions/utils.R")
source("MSIreg_functions/simpleitk.R")

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


image(
  msi_data_proc,
  superpose = TRUE,
  mz = c(414.2408, 519.2942),
  tol = 0.5,
  unit = "mz",
  normalize.image = "linear",
  col = c("blue", "red")
)


RNGkind("L'Ecuyer-CMRG") 
mse_mean <- summarizeFeatures(msi_data, "mean")

mz_ref <- mzvals

snr = 3
tolerance = 0.5

control_MSI_ref <- mse_mean %>%
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



# Find the most important features
ssc <- spatialShrunkenCentroids(
  x        = msi_data_binned,
  r        = 1,                    # nabo-radius
  k        = 15,                   # antal clusters
  s        = c(0, 24),             # shrinkage-niveauer (0 + 24)
  weights  = "gaussian",           # 'method' er nu deprec.; brug 'weights'
  BPPARAM  = getCardinalBPPARAM()
)
topf <- topFeatures(ssc, n=10, model=list(s=24))$mz 

ssc_s24 <- ssc[[2L]]

## 3) hent top-features for denne model
ssc_top <- topFeatures(ssc_s24, n = 10)  # data.frame med bl.a. 'mz' og 'class'

## m/z-kandidater til tissue/background-separation
topf <- ssc_top$mz
topf

image(
  msi_data_binned,
  mz = 830.5612
)
830.5612
# Select the outline of the tissue section
mse_roi <- selectROI(msi_data_binned, mz = 830.5612) 

# Read the .tif image
opt <- readImage("kidney_and_tumor/OPTICAL_tumor.tif") 

## MSI-attributter
mse_attrs <- list(
  ## antal pixels i x- og y-retning i MSI-data
  nX = diff(range(coord(msi_data_binned)[, "x"])) + 1L,
  nY = diff(range(coord(msi_data_binned)[, "y"])) + 1L,
  
  ## antal features (m/z) og antal pixels (spektre)
  nF = nrow(msi_data_binned),
  nP = ncol(msi_data_binned),
  
  ## opløsning af det optiske billede (H&E)
  nXo = ncol(opt),   # bredde
  nYo = nrow(opt)    # højde
)

# Sekect outline of tissue section on image (RUN FROM COMMAND LINE)
opt_roi <- drawROIOnImage(opt)


# save.image("kidney_and_tumor/tumor_opt_roi.Rdata")
# save(opt_roi, file = "kidney_and_tumor/tumor_opt_roi_object.Rdata")

mse_mask <- as_mask_matrix(mse_roi, msi=msi_data_binned)
opt_mask <- as_mask_matrix(opt_roi, msi=opt)

OUT <- cropToEdgesAndPadZeros(ref_mask=mse_mask, target_mask=opt_mask, target_img=opt)

opt <- OUT$img 
opt_roi <- OUT$mask 



OUT <- coregister(mse=msi_data_binned, opt=opt, mse_roi=mse_roi, 
                  opt_roi=opt_roi,
                  mz_list = c(369.3, 830.5612, 798.5),
                  spatial_scale=4, 
                  BPPARAM=MulticoreParam(), verbose=TRUE) 
save(OUT, file = "kidney_and_tumor/tumor_OUT_object.Rdata")

out <- rlang::duplicate(OUT)


out$fixed <- SimpleITK::as.image(out$opt, isVector=FALSE)
out$moving <- SimpleITK::as.image(out$msimg, isVector=FALSE)

# Generate the transformed and aligned moving image 
out$movingx <- Resample(out$moving, out$reg$TF)



alpha <- 0.45 

# Convert SimpleITK's image() objects into EBimage's Image() objects for easier and flexible plotting 
.fixed <- out$fixed |> as.array() |> EBImage::Image()
.moving <- out$moving |> as.array() |> EBImage::Image()
.movingx <- out$movingx |> as.array() |> EBImage::Image()

# Overlay fixed and moving images 
ol1 <- (.fixed * alpha + .moving * (1-alpha)) 

# Overlay fixed and transformed moving images 
ol2 <- (.fixed * alpha + .movingx * (1-alpha))

# Plot fixed, moving, and overlays before and after coregistration 
def.par <- par(no.readonly=TRUE)
height <- 11
width <- height * (dim(.fixed)[1] / dim(.fixed)[2])

titles <- c("Optical image", "Mass spec. image",
            "Overlay before coregistration", "Overlay after coregistration")

par(mar=c(0.25, 0.25, 0.75, 0.25))
pl <- graphics::layout(mat=matrix(c(1:4), ncol=2, byrow=TRUE),
                       heights=rep(height/2, 2), widths=rep(width/2, 2), 
                       respect=TRUE)

. <- sapply(c(1:4), function(x) {
  plot.new()
  rasterImage(list(.fixed, .moving, ol1, ol2)[[x]],
              xleft=0, xright=1, ytop=0, ybottom=1,
              interpolate=FALSE) 
  title(titles[x])
}) 
par(def.par) 




overlay <- overlayGridOnImage(out$msimg, scale_factor=1)$overlay |> as.image()
grid <- overlayGridOnImage(out$msimg, scale_factor=1)$grid |> as.image()

out$overlayx <- Resample(overlay, out$reg$TF) |> as.array() |> EBImage::Image()
out$gridx <- Resample(grid, out$reg$TF) |> as.array() |> EBImage::Image()




height <- 7 
width <- (height * 2) * (dim(out$gridx)[1] / dim(out$gridx)[2])

pl <- graphics::layout(mat=matrix(c(1:2), ncol=2, byrow=TRUE),
             heights=height, widths=rep(width/2, 2),
             respect=TRUE) 
titles <- c("Grid warping", "Overlay with grid warping") 
par(mar=c(0.25, 0.25, 0.75, 0.25))
. <- sapply(c(1:2), function(x) {
  plot.new()
  rasterImage(list(out$gridx, out$overlayx)[[x]],
              xleft=0, xright=1, ytop=0, ybottom=1,
              interpolate=TRUE) 
  title(titles[x])
})
par(def.par) 



height <- 8 
width <- (height) * (dim(out$gridx)[1] / dim(out$gridx)[2])

pl <- graphics::layout(mat=matrix(c(1), ncol=1, byrow=TRUE),
             heights=height, widths=width,
             respect=TRUE) 
titles <- c("Grid warping") 
par(mar=c(0.25, 0.25, 0.75, 0.25))
plot.new() 
rasterImage(out$gridx, xleft=0, xright=1, ytop=0, ybottom=1,
            interpolate=TRUE) 
title(titles[1])
par(def.par) 

