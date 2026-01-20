library(mongolite)

file_path <- "572features.csv"

# Read the file as lines
lines <- readLines(file_path)

# Skip metadata lines (lines starting with '#')
data_lines <- lines[!grepl("^#", lines)]

# Function to split a line while handling varying column counts
process_line <- function(line) {
  parts <- strsplit(line, ";")[[1]]
  
  # Ensure a minimum number of columns for consistency
  if (length(parts) < 6) {
    parts <- c(parts, rep(NA, 6 - length(parts)))  # Pad missing values with NA
  }
  
  # Return as a named list
  return(data.frame(mz = as.numeric(parts[1]),
                    Interval_Width = as.numeric(parts[2]),
                    Color = parts[3],
                    Name = parts[4],
                    Extra = ifelse(length(parts) > 6, parts[5], NA), # Handle cases with extra column
                    Intensity = as.numeric(parts[length(parts) - 1]),
                    Formula = parts[length(parts)],
                    stringsAsFactors = FALSE))
}

# Process each line and store in a list
data_list <- lapply(data_lines, process_line)

# Combine into a single data frame if desired
final_df <- do.call(rbind, data_list)
final_df <- final_df[-1, ]
ref_mz <- sort(final_df$mz)

write.csv(ref_mz, "572ref_mz.csv", row.names = FALSE)



mongo_connection <- mongo(collection = "mz_references",
                          db = "msi_project",
                          url = "mongodb://localhost")

mz_data <- read.csv("572ref_mz.csv")$x
doc <- list(
  reference_name = "572_lipids_gangliosides",
  mz_values = mz_data,
  date_added = Sys.time()
)
mongo_connection$insert(doc)





