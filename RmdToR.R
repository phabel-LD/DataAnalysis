# Load required package
library(knitr)

# Define input and output directories
input_dir  <- "Rmds"
output_dir <- "R"
setwd("C:/Users/phabe/OneDrive/Escritorio/PhD_MUDS/DataAnalysis/DataAnalysis")

# Find all .Rmd files recursively (relative paths only)
rmd_files <- list.files(path = input_dir, pattern = "\\.Rmd$", recursive = TRUE, full.names = FALSE)

# Create output directory if it doesn't exist
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Convert each file
for (f in rmd_files) {
  # Build full input path
  full_input <- file.path(input_dir, f)
  
  # Construct output path: replace input_dir with output_dir, change extension
  out_file <- file.path(output_dir, f)
  out_file <- gsub("\\.Rmd$", ".R", out_file)
  
  # Ensure the subdirectory exists
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  
  # Extract R code from Rmd
  knitr::purl(input = full_input, output = out_file, documentation = 1)
  cat("Converted:", full_input, "->", out_file, "\n")
}

cat("\nAll conversions complete.\n")
