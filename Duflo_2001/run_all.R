# ------------------------------------------------------------------------------
# Script: run_all.R
# Purpose: Master script that finds files by NUMBER (01, 02, 03)
# ------------------------------------------------------------------------------

cat("========================================================================\n")
cat("STARTING DUFLO (2001) REPLICATION PIPELINE\n")
cat("========================================================================\n")

# 1. Set Working Directory -----------------------------------------------------
# We assume you are in the Project Root (Duflo_2001). 
# If not, we try to find the "Scripts" folder.

if (dir.exists("Scripts")) {
  # We are in the right place
  scripts_dir <- "Scripts"
} else if (file.exists("01_Data_Cleaning.R") || length(list.files(pattern="01.*\\.R")) > 0) {
  # We are inside the Scripts folder -> Move up one level
  setwd("..")
  scripts_dir <- "Scripts"
} else {
  # Last resort: Ask user
  cat("\n>>> I cannot find the 'Scripts' folder. Please select a file inside it:\n")
  file_path <- file.choose()
  setwd(dirname(dirname(file_path)))
  scripts_dir <- "Scripts"
}

cat("Working Directory:", getwd(), "\n")

# 2. Find Files by Pattern (Ignores weird names) -------------------------------
# This looks for any file containing "01" and ending in ".R" inside Scripts/

find_script <- function(number_pattern) {
  # List all files in Scripts folder
  files <- list.files(scripts_dir, full.names = TRUE)
  
  # Find the one that matches the number (e.g., "01")
  match <- files[grep(number_pattern, files)]
  
  if (length(match) == 0) {
    stop(paste("CRITICAL: Could not find a script with number", number_pattern))
  }
  
  return(match[1]) # Return the first match found
}

s1 <- find_script("01")
s2 <- find_script("02")
s3 <- find_script("03")

cat("Found Script 01:", s1, "\n")
cat("Found Script 02:", s2, "\n")
cat("Found Script 03:", s3, "\n")

# 3. Execute Pipeline ----------------------------------------------------------

cat("\n>>> Step 1: Running Data Cleaning...\n")
source(s1, echo = FALSE)
cat("    [Step 1 Complete]\n")

cat("\n>>> Step 2: Estimating Education Impact (Table 3)...\n")
source(s2, echo = FALSE)
cat("    [Step 2 Complete]\n")

cat("\n>>> Step 3: Estimating Labor Market Returns (Tables 4 & 5)...\n")
source(s3, echo = FALSE)
cat("    [Step 3 Complete]\n")

cat("\n========================================================================\n")
cat("REPLICATION COMPLETE.\n")
cat("Output saved to 'Output/' folder.\n")
cat("========================================================================\n")