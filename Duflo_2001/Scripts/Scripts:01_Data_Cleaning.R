# ------------------------------------------------------------------------------
# Script: 01_Data_Cleaning.R
# Purpose: Clean data (Auto-fixes the "Download ZIP" LFS bug)
# ------------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, readstata13, utils)

# 1. Smart Path Finding --------------------------------------------------------
target_file <- "cleaned_supas.dta"
path_a <- file.path("Original_Data", target_file)      # Run from Root
path_b <- file.path("..", "Original_Data", target_file) # Run from Scripts

if (file.exists(path_a)) {
  final_path <- path_a
} else if (file.exists(path_b)) {
  final_path <- path_b
} else {
  stop("CRITICAL: 'Original_Data' folder missing. Please ensure directory structure is correct.")
}

# 2. THE FIX: Check if file is the "Fake" LFS Pointer --------------------------
# GitHub ZIP downloads replace large files with 1KB text pointers.
# If we detect a tiny file (< 2KB), we force-download the real 573MB data.

file_size_kb <- file.info(final_path)$size / 1024

if (file_size_kb < 5) {
  message("----------------------------------------------------------------")
  message("DETECTED LFS POINTER (File is too small to be real data).")
  message("The user likely downloaded via 'Download ZIP'.")
  message(">> Auto-downloading the real 573MB dataset from GitHub...")
  message("----------------------------------------------------------------")
  
  # Your specific Raw Git LFS URL
  raw_url <- "https://github.com/ag10-0/projects_repo/raw/main/Duflo_2001/Original_Data/cleaned_supas.dta"
  
  # Increase timeout to 30 mins for 573MB file
  options(timeout = 1800) 
  
  tryCatch({
    download.file(raw_url, final_path, mode = "wb")
    message("Download Complete. Proceeding with analysis.")
  }, error = function(e) {
    stop("Download failed. Please check your internet connection.")
  })
} else {
  message("Data file verified (Size: ", round(file_size_kb/1024, 2), " MB).")
}

# 3. Load & Process ------------------------------------------------------------
cols_to_keep <- c("sex", "birthyr", "yrschl", "incwage", 
                  "nin", "en71", "wtper", "kab", "birthpl") 

raw_data <- read.dta13(final_path, select.cols = cols_to_keep, convert.factors = FALSE)
dt <- as.data.table(raw_data)

# 4. Clean Variables -----------------------------------------------------------
dt_clean <- dt[sex == 1 & birthyr >= 1950 & birthyr <= 1972]

dt_clean[, cohort := fcase(
  birthyr >= 1968 & birthyr <= 1972, "Young",
  birthyr >= 1950 & birthyr <= 1962, "Old",
  default = NA 
)]

dt_clean[, ln_wage := ifelse(incwage > 0, log(incwage), NA)]
dt_clean[, is_young := ifelse(cohort == "Young", 1, 0)]

# 5. Save Output ---------------------------------------------------------------
output_dir <- ifelse(final_path == path_b, "../Analysis_Data", "Analysis_Data")
if (!dir.exists(output_dir)) dir.create(output_dir)

save_path <- file.path(output_dir, "duflo_analytic_sample.rds")
final_sample <- dt_clean[!is.na(cohort)]
saveRDS(final_sample, save_path)

cat("SUCCESS: Data processed and saved to", save_path, "\n")