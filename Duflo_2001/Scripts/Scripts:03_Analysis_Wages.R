# ------------------------------------------------------------------------------
# Script: 03_Analysis_Wages.R
# Purpose: Replicate Tables 4 & 5 (Labor Market Consequences)
# ------------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, fixest, modelsummary)

# 1. Load Data
df <- readRDS("Analysis_Data/duflo_analytic_sample.rds")

# 2. IV Regression (Table 5)
# Use Program Intensity (nin * is_young) to instrument for Years of Schooling (yrschl)
model_iv <- feols(ln_wage ~ 1 | birthpl + birthyr | yrschl ~ nin:is_young,
                  data = df,
                  cluster = ~birthpl)

# 3. Export Results
print("--- REPLICATION: TABLE 5 (Returns to Education) ---")
print(model_iv)

modelsummary(list("IV (Return to Edu)" = model_iv),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = c("fit_yrschl" = "Return to Education"),
             title = "Returns to Education (IV Estimates)",
             output = "Output/Table5_Wages.md")