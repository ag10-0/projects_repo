# ------------------------------------------------------------------------------
# Script: 03_Analysis_Wages.R
# Purpose: Replicate Tables 4 & 5 
# ------------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, fixest, modelsummary)

# 1. Smart Load Data -----------------------------------------------------------
target_file <- "duflo_analytic_sample.rds"
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

# 2. IV Regression (Table 5) ---------------------------------------------------
# Use Program Intensity (nin * is_young) to instrument for Years of Schooling
model_iv <- feols(ln_wage ~ 1 | birthpl + birthyr | yrschl ~ nin:is_young,
                  data = df,
                  cluster = ~birthpl)

# 3. Export Results ------------------------------------------------------------
output_dir <- ifelse(data_path == path_b, "../Output", "Output")
if (!dir.exists(output_dir)) dir.create(output_dir)

print("--- REPLICATION: TABLE 5 (Returns to Education) ---")
print(model_iv)

modelsummary(list("IV (Return to Edu)" = model_iv),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = c("fit_yrschl" = "Return to Education"),
             title = "Returns to Education (IV Estimates)",
             output = file.path(output_dir, "Table5_Wages.md"))

cat("Success! Table 5 saved to", output_dir, "\n")