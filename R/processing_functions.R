# R/processing_functions.R

load_raw_object_from_mongo <- function(sample_name, workdir,
                                       db_name   = DB_NAME,
                                       mongo_url = MONGO_URL,
                                       resolution = 10) {
  paths <- fetch_raw_pair_from_mongo(
    sample_name = sample_name,
    dest_dir    = workdir,
    db_name     = db_name,
    mongo_url   = mongo_url
  )
  Cardinal::readMSIData(
    paths$imzml,
    memory     = FALSE,
    check      = FALSE,
    resolution = resolution,
    units      = "ppm",
    BPPARAM    = BiocParallel::bpparam()
  )
}

