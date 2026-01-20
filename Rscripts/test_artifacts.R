


artifacts <- query_artifacts()
artifacts <- query_artifacts(stage_type = "binned_dataframe")
artifacts$stage_type
raw <- load_artifact(sample_name = "tumorinfiltrat.imzML",
                     stage_type = "raw")
mean <- load_artifact(sample_name = "tumorinfiltrat.imzML",
                      stage_type = "control_mean")
mean_snr <- load_artifact(sample_name = "tumorinfiltrat.imzML",
                      stage_type = "mean_snr_reference",
                      snr = 2.1)

artifacts <- query_artifacts(stage_type = "mean_snr_reference")
data <- load_artifact_by_id(artifacts$gridfs_id[1])

data <- load_artifact(sample_name = "tumorinfiltrat.imzML",
                      stage_type = "binned_dataframe")

artifacts$gridfs_id[1]

str(artifacts)

gridid <- as.character(artifacts$gridfs_id[1])
data <- load_artifact_by_id(gridid)

length(unique(artifacts$gridfs_id))


msi_data <- readImzML("tumorinfiltrat.imzML", memory = FALSE, check = FALSE,
                      mass.range = NULL, resolution = 10, units = c("ppm"),
                      guess.max = 1000L, as = "auto", parse.only = FALSE,
                      verbose = FALSE, chunkopts = list(),
                      BPPARAM = bpparam())
range(coord(msi_data)$x)
range(coord(msi_data)$y)
image(msi_data)


query_clustering_artifacts <- function(sample_name = NULL,
                                       assignment_id = NULL,
                                       clustering_method = NULL,
                                       resolution = NULL,
                                       snr = NULL,
                                       tolerance = NULL,
                                       reference_name = NULL,
                                       db_name = "MSI_test_database",
                                       mongo_url = "mongodb://localhost") {
  
  mongo_cluster_meta <- mongo(collection = "clustering_metadata",
                              db = db_name, url = mongo_url)
  
  # Build query
  query_parts <- list()
  if (!is.null(sample_name)) query_parts$sample_name <- sample_name
  if (!is.null(assignment_id)) query_parts$assignment_id <- assignment_id
  if (!is.null(clustering_method)) query_parts$clustering_method <- clustering_method
  if (!is.null(resolution)) query_parts$resolution <- resolution
  if (!is.null(snr)) query_parts$snr <- snr
  if (!is.null(tolerance)) query_parts$tolerance <- tolerance
  if (!is.null(reference_name)) query_parts$reference_name <- reference_name
  
  query_json <- if (length(query_parts) == 0) {
    '{}'
  } else {
    jsonlite::toJSON(query_parts, auto_unbox = TRUE)
  }
  
  results <- mongo_cluster_meta$find(query_json)
  results
}


clust_artifacts <- query_clustering_artifacts()
df <- load_clustering(sample_name = "tumorinfiltrat.imzML", 
                      snr = 2.9, 
                      tolerance = 0.4, 
                      most_recent = TRUE)


df1 <- df[df$Class=="300X160Y",]






query_artifacts <- function(sample_name = NULL, 
                            stage_type = NULL,
                            snr = NULL, 
                            tolerance = NULL,
                            reference_name = NULL,
                            run_id = NULL,
                            db_name = "MSI_test_database",
                            mongo_url = "mongodb://localhost") {
  
  mongo_meta <- mongo(collection = "processing_artifacts_metadata", 
                      db = db_name, url = mongo_url)
  
  # Build query
  query_parts <- list()
  if (!is.null(sample_name)) query_parts$sample_name <- sample_name
  if (!is.null(stage_type)) query_parts$stage_type <- stage_type
  if (!is.null(snr)) query_parts$snr <- snr
  if (!is.null(tolerance)) query_parts$tolerance <- tolerance
  if (!is.null(reference_name)) query_parts$reference_name <- reference_name
  if (!is.null(run_id)) query_parts$run_id <- run_id
  
  query_json <- if (length(query_parts) == 0) {
    '{}'
  } else {
    jsonlite::toJSON(query_parts, auto_unbox = TRUE)
  }
  
  results <- mongo_meta$find(query_json)
  results
}

artifacts <- query_artifacts()

unique(artifacts$stage_type)
artifacts$filename









