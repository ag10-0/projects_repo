# ------------------------------------------------------------------------------
# Script: run_all.R
# Purpose: Master script to run the entire replication in one click.
# ------------------------------------------------------------------------------

# 1. Measure Total Time
start_time <- Sys.time()
cat("========================================================================\n")
cat("STARTING DUFLO (2001) REPLICATION PIPELINE\n")
cat("========================================================================\n\n")

# 2. Run Scripts in Order
# We use source() to run the scripts from the Scripts/ folder.

# Step 1: Data Cleaning & Download
cat(">>> Step 1: Running Data Cleaning...\n")
source("Scripts/01_Data_Cleaning.R", echo = FALSE)
cat("    [Done]\n\n")

# Step 2: Education Analysis (Table 3)
cat(">>> Step 2: Estimating Education Impact (Table 3)...\n")
source("Scripts/02_Analysis_Education.R", echo = FALSE)
cat("    [Done]\n\n")

# Step 3: Wage Analysis (Tables 4 & 5)
cat(">>> Step 3: Estimating Labor Market Returns (Tables 4 & 5)...\n")
source("Scripts/03_Analysis_Wages.R", echo = FALSE)
cat("    [Done]\n\n")

# 3. Final Summary
end_time <- Sys.time()
duration <- round(difftime(end_time, start_time, units = "secs"), 2)

cat("========================================================================\n")
cat("REPLICATION COMPLETE.\n")
cat("Total Runtime:", duration, "seconds.\n")
cat("Output saved to 'Output/' folder.\n")
cat("========================================================================\n")