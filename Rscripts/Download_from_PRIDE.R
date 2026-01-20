#!/usr/bin/env Rscript

# ------------------------------------------------------------
# Download alle filer fra PXD026459 via FTP
# (ingen PRIDE API, kun FTP-directory listing)
# ------------------------------------------------------------

suppressPackageStartupMessages({
  if (!requireNamespace("RCurl", quietly = TRUE)) {
    install.packages("RCurl", repos = "https://cloud.r-project.org")
  }
})

library(RCurl)

project_id <- "PXD026459"

# FTP-base for dette projekt (fra ProteomeXchange)
ftp_base <- "ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2021/06/PXD026459/"

# Hvor skal filerne gemmes?
base_download_dir <- "data/makof21/MSI_database/Data/TMA_data"
out_dir <- file.path(base_download_dir, project_id)

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  message("Opretter mappe: ", out_dir)
} else {
  message("Bruger eksisterende mappe: ", out_dir)
}

message("Henter directory listing fra FTP...")

# hent kun filnavne
dir_listing <- getURL(ftp_base, dirlistonly = TRUE)

# split til vektor af filnavne
files <- unlist(strsplit(dir_listing, "\\r?\\n"))
files <- files[nzchar(files)]  # fjern tomme linjer

if (length(files) == 0) {
  stop("Fandt ingen filer i FTP-mappen. Tjek ftp_base.")
}

message("Fandt ", length(files), " filer på FTP:")
print(files)

# logfiler
log_ok  <- file.path(out_dir, "download_success.log")
log_err <- file.path(out_dir, "download_failed.log")
cat("", file = log_ok)
cat("", file = log_err)

message("Starter download...")

for (i in seq_along(files)) {
  fname <- files[i]
  url   <- paste0(ftp_base, fname)
  dest  <- file.path(out_dir, fname)
  
  if (file.exists(dest)) {
    message("[", i, "/", length(files), "] Springer over (findes allerede): ", fname)
    cat(fname, "SKIPPED (exists)\n", file = log_ok, append = TRUE)
    next
  }
  
  message("[", i, "/", length(files), "] Downloader: ", fname)
  
  ok <- tryCatch(
    {
      download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
      TRUE
    },
    error = function(e) {
      message("  FEJL for ", fname, ": ", conditionMessage(e))
      FALSE
    },
    warning = function(w) {
      message("  ADVARSEL for ", fname, ": ", conditionMessage(w))
      FALSE
    }
  )
  
  if (ok) {
    cat(fname, "OK\n", file = log_ok, append = TRUE)
  } else {
    cat(fname, "FAILED\n", file = log_err, append = TRUE)
  }
}

message("--------------------------------------------------")
message("Download færdig.")
message("Filerne ligger i: ", out_dir)
message("Log OK:   ", log_ok)
message("Log fejl: ", log_err)
message("--------------------------------------------------")
