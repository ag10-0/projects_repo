# ------------------------------------------------------------------------------
# Script: 01_Data_Cleaning.R
# Purpose: Clean local data 
# ------------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, readstata13, magrittr)

# 1. robust File Finding -------------------------------------------------------
# This block checks where R is currently "looking" and adjusts the path.

target_file <- "cleaned_supas.dta"

# Check Option A: We are in the Project Root (Ideal)
path_a <- file.path("Original_Data", target_file)

# Check Option B: We are inside the Scripts folder (Common issue)
path_b <- file.path("..", "Original_Data", target_file)

if (file.exists(path_a)) {
  final_path <- path_a
  cat("Found data at:", path_a, "\n")
} else if (file.exists(path_b)) {
  final_path <- path_b
  cat("Found data at:", path_b, "\n")
} else {
  # If both fail, print the current location so you can see the mismatch
  cat("--------------------------------------------------\n")
  cat("CRITICAL ERROR: Data not found.\n")
  cat("R is currently looking in:", getwd(), "\n")
  cat("It was looking for 'Original_Data' folder here or one level up.\n")
  cat("--------------------------------------------------\n")
  stop("Please make sure 'Original_Data' exists next to the 'Scripts' folder.")
}

# 2. Load Data -----------------------------------------------------------------
cols_to_keep <- c("sex", "birthyr", "yrschl", "incwage", 
                  "nin", "en71", "wtper", "kab", "birthpl") 

# Load using the path we found above
raw_data <- read.dta13(final_path, 
                       select.cols = cols_to_keep, 
                       convert.factors = FALSE)

dt <- as.data.table(raw_data)

# 3. Filter & Process ----------------------------------------------------------
dt_clean <- dt[sex == 1 & birthyr >= 1950 & birthyr <= 1972]

dt_clean[, cohort := fcase(
  birthyr >= 1968 & birthyr <= 1972, "Young",
  birthyr >= 1950 & birthyr <= 1962, "Old",
  default = NA 
)]

dt_clean[, ln_wage := ifelse(incwage > 0, log(incwage), NA)]
dt_clean[, is_young := ifelse(cohort == "Young", 1, 0)]

# 4. Save (Also with Smart Path) -----------------------------------------------
# If we are in Scripts (Option B), we need to save one level up
output_dir <- ifelse(final_path == path_b, "../Analysis_Data", "Analysis_Data")

if (!dir.exists(output_dir)) dir.create(output_dir)

save_path <- file.path(output_dir, "duflo_analytic_sample.rds")
final_sample <- dt_clean[!is.na(cohort)]
saveRDS(final_sample, save_path)

cat("SUCCESS: Data processed and saved to", save_path, "\n")