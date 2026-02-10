# ------------------------------------------------------------------------------
# Script: 01_Data_Cleaning.R
# Purpose: Prepare analytic sample for Duflo (2001).
# Note:    Falls back to local 'cleaned_supas.dta' if cloud download fails.
# ------------------------------------------------------------------------------

# 1. Setup ---------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, readstata13, magrittr, tools)

# Set Working Directory (Adjust if needed, but TIER prefers relative paths)
# setwd("~/Desktop/Replications/Duflo_2001/Replication") 

# Create TIER folders
if (!dir.exists("Original_Data")) dir.create("Original_Data")
if (!dir.exists("Analysis_Data")) dir.create("Analysis_Data")

# 2. Data Acquisition (Robust Strategy) ----------------------------------------
dest_file <- "Original_Data/cleaned_supas.dta"
local_source <- "cleaned_data/cleaned_supas.dta" # Where you currently have it

# STRATEGY: 
# 1. Check if we already have the file in 'Original_Data'.
# 2. If not, try to copy it from your local 'cleaned_data' folder.
# 3. If that fails (user doesn't have it), try downloading (currently down).

if (!file.exists(dest_file)) {
  if (file.exists(local_source)) {
    message("Copying local data to TIER folder...")
    file.copy(local_source, dest_file)
  } else {
    # Fallback to download (Only runs if you don't have the file locally)
    # Note: Harvard Dataverse is unstable (503 Error), so we prioritize local.
    message("Attempting download (Server might be unstable)...")
    url <- "https://dataverse.harvard.edu/api/access/datafile/4605930"
    tryCatch(
      download.file(url, dest_file, mode = "wb"),
      error = function(e) stop("Download failed and no local file found. Please place 'cleaned_supas.dta' in 'Original_Data/' manually.")
    )
  }
}

# 3. Load & Process ------------------------------------------------------------
# We use the specific columns that worked in your successful replication
cols_to_keep <- c("sex", "birthyr", "yrschl", "incwage", 
                  "nin", "en71", "wtper", "kab", "birthpl") 

# Load specific columns
raw_data <- read.dta13(dest_file, select.cols = cols_to_keep, convert.factors = FALSE)
dt <- as.data.table(raw_data)

# 4. Sample Selection ----------------------------------------------------------
# Males born 1950-1972
dt_clean <- dt[sex == 1 & birthyr >= 1950 & birthyr <= 1972]

# 5. Variable Construction -----------------------------------------------------
# A. Define Cohorts
dt_clean[, cohort := fcase(
  birthyr >= 1968 & birthyr <= 1972, "Young",
  birthyr >= 1950 & birthyr <= 1962, "Old",
  default = NA 
)]

# B. Clean Wages
dt_clean[, ln_wage := ifelse(incwage > 0, log(incwage), NA)]

# C. Analysis Dummy
dt_clean[, is_young := ifelse(cohort == "Young", 1, 0)]

# 6. Save Analytic Sample ------------------------------------------------------
final_sample <- dt_clean[!is.na(cohort)]
saveRDS(final_sample, "Analysis_Data/duflo_analytic_sample.rds")

cat("--------------------------------------------------\n")
cat("Processing Complete.\n")
cat("Data Source: ", dest_file, "\n")
cat("Analytic N:  ", nrow(final_sample), "\n")
cat("--------------------------------------------------\n")