# ------------------------------------------------------------------------------
# Script: 02_Analysis_Education.R
# Purpose: Replicate Table 3 
# ------------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, fixest, modelsummary)

# 1. Smart Load Data -----------------------------------------------------------
target_file <- "duflo_analytic_sample.rds"

# Check Root Path vs Scripts Path
path_a <- file.path("Analysis_Data", target_file)
path_b <- file.path("..", "Analysis_Data", target_file)

if (file.exists(path_a)) {
  data_path <- path_a
} else if (file.exists(path_b)) {
  data_path <- path_b
} else {
  stop("CRITICAL: Analytic sample not found. Did you run Script 01?")
}

df <- readRDS(data_path)

# 2. Main DiD Regression (Table 3) ---------------------------------------------
# Y = District_FE + Cohort_FE + (Intensity * Young)
model_edu <- feols(yrschl ~ nin:is_young | birthpl + birthyr, 
                   data = df, 
                   cluster = ~birthpl)

# 3. Export Results (Smart Output Path) ----------------------------------------
output_dir <- ifelse(data_path == path_b, "../Output", "Output")
if (!dir.exists(output_dir)) dir.create(output_dir)

print("--- REPLICATION: TABLE 3 (Education) ---")
print(model_edu)

modelsummary(list("Years of Education" = model_edu),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = c("nin:is_young" = "Program Effect (INPRES)"),
             gof_map = c("nobs", "r.squared"),
             title = "Impact of School Construction on Education",
             output = file.path(output_dir, "Table3_Replication.md"))

cat("Success! Table 3 saved to", output_dir, "\n")
