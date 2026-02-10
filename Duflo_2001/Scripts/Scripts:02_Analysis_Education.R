# ------------------------------------------------------------------------------
# Script: 02_Analysis_Education.R
# Purpose: Replicate Table 3 
# ------------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, fixest, modelsummary)

# 1. Smart Load Data -----------------------------------------------------------
# This block automatically detects if you are running from the Root or Scripts folder.

target_file <- "duflo_analytic_sample.rds"

# Path A: Project Root (Ideal)
path_a <- file.path("Analysis_Data", target_file)

# Path B: Inside Scripts Folder (Your current situation)
path_b <- file.path("..", "Analysis_Data", target_file)

if (file.exists(path_a)) {
  data_path <- path_a
  cat("Found data at:", path_a, "\n")
} else if (file.exists(path_b)) {
  data_path <- path_b
  cat("Found data at:", path_b, "\n")
} else {
  stop("CRITICAL ERROR: Could not find 'duflo_analytic_sample.rds'. \n   Please ensure you ran Script 01 successfully.")
}

df <- readRDS(data_path)

# 2. Main DiD Regression (Table 3) ---------------------------------------------
# Y = District_FE + Cohort_FE + (Intensity * Young)
model_edu <- feols(yrschl ~ nin:is_young | birthpl + birthyr, 
                   data = df, 
                   cluster = ~birthpl)

# 3. Smart Export Results ------------------------------------------------------
# If we are in the Scripts folder (Path B), we must save output one level up.

output_dir <- ifelse(data_path == path_b, "../Output", "Output")

if (!dir.exists(output_dir)) dir.create(output_dir)

print("--- REPLICATION: TABLE 3 (Education) ---")
print(model_edu)

# Generate Table
modelsummary(list("Years of Education" = model_edu),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = c("nin:is_young" = "Program Effect (INPRES)"),
             gof_map = c("nobs", "r.squared"),
             title = "Impact of School Construction on Education",
             output = file.path(output_dir, "Table3_Replication.md"))

cat("Success! Table 3 saved to:", file.path(output_dir, "Table3_Replication.md"), "\n")