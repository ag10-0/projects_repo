# ------------------------------------------------------------------------------
# Script: 02_Analysis_Education.R
# Purpose: Replicate Table 3 (Impact of School Construction on Education)
# ------------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, fixest, modelsummary)

# 1. Load Processed Data
if (!file.exists("Analysis_Data/duflo_analytic_sample.rds")) stop("Run Script 01 first!")
df <- readRDS("Analysis_Data/duflo_analytic_sample.rds")

# 2. Main DiD Regression (Table 3)
# Y = District_FE + Cohort_FE + (Intensity * Young)
model_edu <- feols(yrschl ~ nin:is_young | birthpl + birthyr, 
                   data = df, 
                   cluster = ~birthpl)

# 3. Export Results
if (!dir.exists("Output")) dir.create("Output")

print("--- REPLICATION: TABLE 3 (Education) ---")
print(model_edu)

modelsummary(list("Years of Education" = model_edu),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = c("nin:is_young" = "Program Effect (INPRES)"),
             gof_map = c("nobs", "r.squared"),
             title = "Impact of School Construction on Education",
             output = "Output/Table3_Replication.md")