# ==============================================================================
# ECONOMETRICS HOMEWORK 2 - SETUP
# ==============================================================================
rm(list = ls())
#Load Necessary Packages ---------------------------------------------------
library(tidyverse)  
library(stargazer) 
library(plm)       
library(AER)
library(car)
library(lmtest)    
library(sandwich) 
library(readxl) 

#Import Data ---------------------------------------------------------------

df <- read_excel("~/Desktop/M1 APE/Econometrics /HomeWork /HW2/env_kuznets.xlsx")

#Initial Data Inspection ---------------------------------------------------

glimpse(df)        
head(df)            
summary(df)        

#==============================================================================
# QUESTION 1: EKC SCATTER PLOTS (France + 2 Others)
# ==============================================================================

#Filter Data for Selected Countries ----------------------------------------
# We select France (Europe), United States (North America), and China (Asia).
unique(df$country)
selected_countries <- c("France", "USA", "China")

df_subset <- df %>%
  filter(country %in% selected_countries)

#Create Scatter Plots with Polynomial Fits ---------------------------------
p1 <- ggplot(df_subset, aes(x = GDP, y = CO2)) +
  geom_point(alpha = 0.5, color = "darkblue", size = 1.5) +
  # Overlay fitted polynomial curve (Quadratic)
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE, size = 1) +
  facet_wrap(~ country, scales = "free") + 
  labs(
    title = "Environmental Kuznets Curve: CO2 vs GDP",
    subtitle = "Polynomial fit (degree 2) overlaid on raw data",
    x = "GDP per Capita",
    y = "CO2 Emissions per Capita"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 12))

ggsave("plot_q1.png", plot = p1, width = 10, height = 4)

# ==============================================================================
# QUESTION 2
# ==============================================================================
# Filter for France only
df_france <- df %>%
  filter(country == "France") %>%
  # Create Log variables for the regression
  mutate(
    ln_co2 = log(CO2),
    ln_gdp = log(GDP),
    ln_gdp2 = ln_gdp^2, # Squared log GDP
    ln_pop = log(POP)
  )

vars_continuous <- c("CO2", "GDP", "POP", "IND", "URB", "ENG", "MOT", "INT", "CHI", "DIS")

# Subset the data for the table
stats_data <- df_france %>%
  select(all_of(vars_continuous)) %>%
  as.data.frame() # Stargazer handles dataframes better than tibbles

stargazer(stats_data, 
          type = "latex",
          title = "Descriptive Statistics (France)",
          label = "tab:descriptive_stats",
          digits = 2,
          out = "table_descriptive.tex", 
          header = FALSE) 

# ==============================================================================
# QUESTION 3
# ==============================================================================

# --- Clean Data ---
# We must remove rows where CO2, GDP, or POP are <= 0 or NA
# because log(0) = -Inf and log(negative) = NaN
df_clean <- subset(df, CO2 > 0 & GDP > 0 & POP > 0)

# Check how many observations were dropped
n_original <- nrow(df)
n_clean <- nrow(df_clean)
cat("Observations original:", n_original, "\n")
cat("Observations used:", n_clean, "\n")

if (n_clean == 0) {
  stop("Error: All data points were removed. Check if your data is numeric or if it contains zeros.")
}

# --- Transformations ---
df_clean$ln_CO2 <- log(df_clean$CO2)
df_clean$ln_GDP <- log(df_clean$GDP)
df_clean$ln_POP <- log(df_clean$POP)
df_clean$ln_GDP2 <- df_clean$ln_GDP^2

# --- Estimate Model M1 ---
model_m1 <- lm(ln_CO2 ~ ln_GDP + ln_GDP2 + ln_POP, data = df_clean)

# --- Report Results ---
summary(model_m1)

# Generate LaTeX table
stargazer(model_m1, type = "latex", header = FALSE,
          title = "Regression Results (Model M1)",
          label = "tab:m1_results")

# --- Turning Point Calculation ---
b1 <- coef(model_m1)["ln_GDP"]
b2 <- coef(model_m1)["ln_GDP2"]

cat("\n--- Turning Point Results ---\n")
cat("Beta 1:", b1, "\n")
cat("Beta 2:", b2, "\n")

# Turning point formula: -b1 / (2*b2)
tp_ln_gdp <- -b1 / (2 * b2)
tp_gdp <- exp(tp_ln_gdp)

cat("Turning Point (log GDP):", tp_ln_gdp, "\n")
cat("Turning Point (GDP level):", tp_gdp, "\n")

# Check for inverted-U
if(b1 > 0 & b2 < 0){
  cat("Evidence of Inverted-U: YES (b1 > 0, b2 < 0)\n")
} else {
  cat("Evidence of Inverted-U: NO\n")
}

# -----------------------------------------------------------------------------
# Question 4b: Breusch-Godfrey Test
# -----------------------------------------------------------------------------

# Perform the Breusch-Godfrey test for serial correlation of order 6
# H0: No serial correlation up to order 6
# H1: Serial correlation present up to order 6
bg_result <- bgtest(model_m1, order = 6)

# Print results
print(bg_result)

# Extract specific values for reporting
cat("\n--- Breusch-Godfrey Test Results (AR(6)) ---\n")
cat("LM Statistic:", bg_result$statistic, "\n")
cat("Degrees of Freedom:", bg_result$parameter, "\n")
cat("P-value:", bg_result$p.value, "\n")

if(bg_result$p.value < 0.05){
  cat("Conclusion: REJECT Null Hypothesis (Evidence of Serial Correlation)\n")
} else {
  cat("Conclusion: FAIL TO REJECT Null Hypothesis (No Evidence of Serial Correlation)\n")
}


# -----------------------------------------------------------------------------
# Question 4c: Newey-West Standard Errors
# -----------------------------------------------------------------------------

# Calculate Newey-West variance-covariance matrix
nw_vcov <- NeweyWest(model_m1)

nw_results <- coeftest(model_m1, vcov = nw_vcov)

print(nw_results)

# -----------------------------------------------------------------------------
# Comparison: OLS vs Newey-West
# -----------------------------------------------------------------------------
cat("\n--- Comparison of Standard Errors ---\n")
ols_se <- summary(model_m1)$coefficients[, "Std. Error"]
nw_se <- nw_results[, "Std. Error"]

comparison_df <- data.frame(
  Coeff = names(ols_se),
  OLS_SE = ols_se,
  NW_SE = nw_se,
  Difference = nw_se - ols_se,
  Ratio_NW_OLS = nw_se / ols_se
)

print(comparison_df)

cat("\n--- Note for Analysis ---\n")
cat("1. Check if coefficients changed: They should remain EXACTLY the same.\n")
cat("2. Check if Standard Errors increased: If Ratio > 1, NW corrected SEs are larger.\n")

# -----------------------------------------------------------------------------
# Question 5: Data Preparation and Estimation of Model M2
# -----------------------------------------------------------------------------

#Filter for Year 2007
if (!"year" %in% names(df)) {
  stop("Error: 'Year' column not found in dataframe.")
}
df_2007 <- subset(df, year == 2007)

#Select Variables and Remove Missing Values
# We need: CO2, GDP, POP, IND, URB, ENG, MOT
# UPDATE THESE NAMES if they differ in your dataset
cols_needed <- c("CO2", "GDP", "POP", "IND", "URB", "ENG", "MOT")

# Check for missing columns
missing_cols <- setdiff(cols_needed, names(df_2007))
if(length(missing_cols) > 0){
  stop(paste("Error: The following columns are missing from the dataframe:", 
             paste(missing_cols, collapse=", ")))
}

# Create a subset with only necessary columns and drop rows with ANY NA
df_m2 <- df_2007[, cols_needed]
df_m2 <- na.omit(df_m2)

# Ensure positive values for logs (remove rows with <= 0 for log variables)
df_m2 <- subset(df_m2, CO2 > 0 & GDP > 0 & POP > 0 & ENG > 0 & MOT > 0)

cat("Observations used for Model M2 (2007 Cross-section):", nrow(df_m2), "\n")

#Transformations
df_m2$ln_CO2 <- log(df_m2$CO2)
df_m2$ln_GDP <- log(df_m2$GDP)
df_m2$ln_GDP2 <- df_m2$ln_GDP^2
df_m2$ln_POP <- log(df_m2$POP)
df_m2$ln_ENG <- log(df_m2$ENG)
df_m2$ln_MOT <- log(df_m2$MOT)
# IND and URB are used as is (linear), assuming they are rates/percentages.

#Estimate Model M2
# ln(CO2) = b0 + b1*ln(GDP) + b2*ln(GDP)^2 + b3*ln(POP) + g1*IND + g2*URB + g3*ln(ENG) + g4*ln(MOT)
model_m2 <- lm(ln_CO2 ~ ln_GDP + ln_GDP2 + ln_POP + IND + URB + ln_ENG + ln_MOT, data = df_m2)

#Report Results
summary(model_m2)

# Generate LaTeX table code
stargazer(model_m2, type = "latex", header = FALSE,
          title = "Cross-Sectional Regression Results (2007)",
          label = "tab:m2_results")

# -----------------------------------------------------------------------------
# Question 8a: Instrumental Variable Relevance (Correlation Analysis)
# -----------------------------------------------------------------------------

#Reload/Prepare Data for 2007 including potential IVs
# We need the previous variables plus INT, CHI, DIS
cols_iv <- c("CO2", "GDP", "POP", "IND", "URB", "ENG", "MOT", "INT", "CHI", "DIS")

# Assuming 'df_2007' was created in the previous step (subset of year 2007)

# Check for missing columns again
missing_ivs <- setdiff(cols_iv, names(df_2007))
if(length(missing_ivs) > 0){
  stop(paste("Error: Missing columns:", paste(missing_ivs, collapse=", ")))
}

# Create subset and handle NAs
df_iv <- df_2007[, cols_iv]
df_iv <- na.omit(df_iv)
# Ensure positive values for logs where necessary
df_iv <- subset(df_iv, CO2 > 0 & GDP > 0 & POP > 0 & ENG > 0 & MOT > 0)

# Create log GDP
df_iv$ln_GDP <- log(df_iv$GDP)

#Compute Correlation Coefficients
# We check the correlation between ln(GDP) and the potential instruments
cor_int <- cor(df_iv$ln_GDP, df_iv$INT)
cor_chi <- cor(df_iv$ln_GDP, df_iv$CHI)
cor_dis <- cor(df_iv$ln_GDP, df_iv$DIS)

cat("\n--- Correlation Coefficients with ln(GDP) ---\n")
cat("Correlation with INT:", cor_int, "\n")
cat("Correlation with CHI:", cor_chi, "\n")
cat("Correlation with DIS:", cor_dis, "\n")

#Assess which is strongest
cor_vals <- c(INT=abs(cor_int), CHI=abs(cor_chi), DIS=abs(cor_dis))
best_iv <- names(which.max(cor_vals))
cat("\nBased on magnitude, the strongest candidate is:", best_iv, "\n")


# -----------------------------------------------------------------------------
# Question 8c: First-Stage Regressions
# -----------------------------------------------------------------------------

#Prepare Data 
# Create log transformations for controls and dependent variables
df_iv$ln_POP <- log(df_iv$POP)
df_iv$ln_ENG <- log(df_iv$ENG)
df_iv$ln_MOT <- log(df_iv$MOT)
df_iv$ln_GDP <- log(df_iv$GDP)
df_iv$ln_GDP2 <- df_iv$ln_GDP^2

# Create squared instruments
df_iv$INT2 <- df_iv$INT^2
df_iv$CHI2 <- df_iv$CHI^2
df_iv$DIS2 <- df_iv$DIS^2

#Define the base formula for controls
# Controls: ln(POP) + IND + URB + ln(ENG) + ln(MOT)
controls <- "ln_POP + IND + URB + ln_ENG + ln_MOT"

#Define function to run first stage and get diagnostics
run_first_stage <- function(iv_name, iv_sq_name, data) {
  
  # Formulas
  f1 <- as.formula(paste("ln_GDP ~", iv_name, "+", iv_sq_name, "+", controls))
  f2 <- as.formula(paste("ln_GDP2 ~", iv_name, "+", iv_sq_name, "+", controls))
  
  # Estimate models
  m1 <- lm(f1, data = data)
  m2 <- lm(f2, data = data)
  
  # F-test for excluded instruments (joint significance of IV and IV^2)
  # H0: coeff_IV = 0 AND coeff_IV^2 = 0
  f_test_1 <- linearHypothesis(m1, c(paste0(iv_name, "=0"), paste0(iv_sq_name, "=0")))
  f_test_2 <- linearHypothesis(m2, c(paste0(iv_name, "=0"), paste0(iv_sq_name, "=0")))
  
  return(list(model1 = m1, model2 = m2, f1 = f_test_1, f2 = f_test_2))
}

#Run for INT
cat("\n--- First Stage: INT ---\n")
res_int <- run_first_stage("INT", "INT2", df_iv)
cat("Dep. Var: ln(GDP)   | F-stat for Instruments:", res_int$f1[2, "F"], "\n")
cat("Dep. Var: ln(GDP)^2 | F-stat for Instruments:", res_int$f2[2, "F"], "\n")

#Run for CHI
cat("\n--- First Stage: CHI ---\n")
res_chi <- run_first_stage("CHI", "CHI2", df_iv)
cat("Dep. Var: ln(GDP)   | F-stat for Instruments:", res_chi$f1[2, "F"], "\n")
cat("Dep. Var: ln(GDP)^2 | F-stat for Instruments:", res_chi$f2[2, "F"], "\n")

#Run for DIS
cat("\n--- First Stage: DIS ---\n")
res_dis <- run_first_stage("DIS", "DIS2", df_iv)
cat("Dep. Var: ln(GDP)   | F-stat for Instruments:", res_dis$f1[2, "F"], "\n")
cat("Dep. Var: ln(GDP)^2 | F-stat for Instruments:", res_dis$f2[2, "F"], "\n")


# -----------------------------------------------------------------------------
# Question 8e: Manual 2SLS Estimation
# -----------------------------------------------------------------------------

# --- 0. Data Preparation (Crucial Step) ---
# Ensure all log variables exist in df_iv before running regressions
df_iv$ln_CO2 <- log(df_iv$CO2)
df_iv$ln_GDP <- log(df_iv$GDP)
df_iv$ln_GDP2 <- df_iv$ln_GDP^2
df_iv$ln_POP <- log(df_iv$POP)
df_iv$ln_ENG <- log(df_iv$ENG)
df_iv$ln_MOT <- log(df_iv$MOT)

# Create squared instruments if not already present
df_iv$INT2 <- df_iv$INT^2
df_iv$CHI2 <- df_iv$CHI^2
df_iv$DIS2 <- df_iv$DIS^2

# Define controls string again for convenience
controls_str <- "ln_POP + IND + URB + ln_ENG + ln_MOT"

# Function to run Manual 2SLS
run_manual_2sls <- function(iv_name, iv_sq_name, dataset) {
  
  # --- Stage 1 ---
  # Regress endogenous vars (ln_GDP, ln_GDP2) on Instruments + Controls
  f1 <- as.formula(paste("ln_GDP ~", iv_name, "+", iv_sq_name, "+", controls_str))
  f2 <- as.formula(paste("ln_GDP2 ~", iv_name, "+", iv_sq_name, "+", controls_str))
  
  stage1_gdp <- lm(f1, data = dataset)
  stage1_gdp2 <- lm(f2, data = dataset)
  
  # --- Extract Fitted Values ---
  # Create a copy of data to store fitted values
  data_stage2 <- dataset
  data_stage2$ln_GDP_hat <- fitted(stage1_gdp)
  data_stage2$ln_GDP2_hat <- fitted(stage1_gdp2)
  
  # --- Stage 2 ---
  # Regress Y on Fitted Values + Controls
  f_stage2 <- as.formula(paste("ln_CO2 ~ ln_GDP_hat + ln_GDP2_hat +", controls_str))
  model_2sls <- lm(f_stage2, data = data_stage2)
  
  return(model_2sls)
}

# Run for each instrument
iv_model_int <- run_manual_2sls("INT", "INT2", df_iv)
iv_model_chi <- run_manual_2sls("CHI", "CHI2", df_iv)
iv_model_dis <- run_manual_2sls("DIS", "DIS2", df_iv)

# Re-run OLS on the IV subset (should match M2 if no extra NAs were dropped)
ols_model <- lm(as.formula(paste("ln_CO2 ~ ln_GDP + ln_GDP2 +", controls_str)), data = df_iv)

# Generate Table
stargazer(ols_model, iv_model_int, iv_model_chi, iv_model_dis,
          type = "latex", header = FALSE,
          title = "2SLS Estimation Results (Manual)",
          label = "tab:iv_results_manual",
          column.labels = c("OLS", "IV: INT", "IV: CHI", "IV: DIS"),
          covariate.labels = c("ln(GDP) (fitted)", "ln(GDP)2 (fitted)", 
                               "ln(POP)", "IND", "URB", "ln(ENG)", "ln(MOT)"),
          dep.var.labels = "ln(CO2)")


# -----------------------------------------------------------------------------
# Question 8f: IV Estimation with Built-in Package (ivreg)
# -----------------------------------------------------------------------------

# We focus on INT as the valid instrument based on Q8d.
# Formula syntax: y ~ regressors | instruments (exogenous vars + excluded instruments)

# Exogenous controls: ln_POP + IND + URB + ln_ENG + ln_MOT
# Endogenous regressors: ln_GDP + ln_GDP2
# Excluded Instruments: INT + INT2

formula_iv <- ln_CO2 ~ ln_GDP + ln_GDP2 + ln_POP + IND + URB + ln_ENG + ln_MOT | 
  ln_POP + IND + URB + ln_ENG + ln_MOT + INT + INT2

# Estimate using ivreg
iv_model_proper <- ivreg(formula_iv, data = df_iv)

# Summary of proper IV
summary(iv_model_proper)

# -----------------------------------------------------------------------------
# Comparison: Manual vs Proper IV
# -----------------------------------------------------------------------------
# Retrieve Manual 2SLS model (from previous step 'iv_model_int')
# If not in memory, re-run the manual step from 8e.

cat("\n--- Comparison of Results (INT Instrument) ---\n")

# Extract coefficients and SEs
coef_manual <- coef(iv_model_int)
se_manual   <- summary(iv_model_int)$coefficients[, "Std. Error"]

coef_proper <- coef(iv_model_proper)
se_proper   <- summary(iv_model_proper)$coefficients[, "Std. Error"]

# Note: Manual model has 'fitted' names (e.g., "ln_GDP_hat"), proper has normal names ("ln_GDP")
# We align them by index assuming same order of variables
comparison_table <- data.frame(
  Variable = names(coef_proper),
  Coef_Manual = coef_manual,
  Coef_Proper = coef_proper,
  Diff_Coef = coef_manual - coef_proper,
  SE_Manual = se_manual,
  SE_Proper = se_proper,
  Ratio_SE = se_proper / se_manual
)

print(comparison_table)

# Generate Stargazer table for the document
stargazer(iv_model_int, iv_model_proper,
          type = "latex", header = FALSE,
          title = "Comparison of Manual and Built-in IV Estimation (Instrument: INT)",
          label = "tab:iv_comparison",
          column.labels = c("Manual 2SLS", "Built-in ivreg"),
          dep.var.labels = "ln(CO2)")

# -----------------------------------------------------------------------------
# Question 8g: Wu-Hausman Specification Test
# -----------------------------------------------------------------------------

# We test for endogeneity of ln(GDP) and ln(GDP)^2 using INT and INT^2 as instruments.

# --- Step 1: Manual Wu-Hausman Test ---
# We already have the first-stage models from Q8c (or re-run them here for clarity).
f1 <- ln_GDP ~ ln_POP + IND + URB + ln_ENG + ln_MOT + INT + INT2
f2 <- ln_GDP2 ~ ln_POP + IND + URB + ln_ENG + ln_MOT + INT + INT2

fs_gdp <- lm(f1, data = df_iv)
fs_gdp2 <- lm(f2, data = df_iv)

# Extract residuals (v1_hat and v2_hat)
df_iv$resid_gdp <- resid(fs_gdp)
df_iv$resid_gdp2 <- resid(fs_gdp2)

# --- Step 2: Auxiliary Regression ---
# Include the first-stage residuals in the original structural equation (M2)
# H0: Coefficients on resid_gdp and resid_gdp2 are BOTH zero.
hausman_aux_formula <- ln_CO2 ~ ln_GDP + ln_GDP2 + ln_POP + IND + URB + ln_ENG + ln_MOT + resid_gdp + resid_gdp2
hausman_aux_model <- lm(hausman_aux_formula, data = df_iv)

# --- Step 3: F-Test on Residual Coefficients ---
# Test joint significance of resid_gdp and resid_gdp2
wh_test <- linearHypothesis(hausman_aux_model, c("resid_gdp=0", "resid_gdp2=0"))

print(wh_test)

# Report Diagnostics
cat("\n--- Wu-Hausman Test Results ---\n")
cat("F-statistic:", wh_test[2, "F"], "\n")
cat("p-value:", wh_test[2, "Pr(>F)"], "\n")

if(wh_test[2, "Pr(>F)"] < 0.05){
  cat("Conclusion: REJECT H0. Endogeneity is present. OLS is inconsistent.\n")
} else {
  cat("Conclusion: FAIL TO REJECT H0. No evidence of endogeneity. OLS is consistent and preferred (more efficient).\n")
}

# -----------------------------------------------------------------------------
# Question 8h: Joint IV Estimation and Sargan Test
# -----------------------------------------------------------------------------

# We select the two "best" instruments: INT and CHI.
# (DIS was found to be very weak and theoretically invalid).

#Prepare Data (Ensure squared CHI exists)
if(!"CHI2" %in% names(df_iv)) {
  df_iv$CHI2 <- df_iv$CHI^2
}

#Define Formula
# Endogenous: ln_GDP, ln_GDP2
# Exogenous Controls: ln_POP, IND, URB, ln_ENG, ln_MOT
# Instruments (Excluded): INT, INT2, CHI, CHI2

formula_joint_iv <- ln_CO2 ~ ln_GDP + ln_GDP2 + ln_POP + IND + URB + ln_ENG + ln_MOT | 
  ln_POP + IND + URB + ln_ENG + ln_MOT + INT + INT2 + CHI + CHI2

#Estimate Model
iv_model_joint <- ivreg(formula_joint_iv, data = df_iv)

#Report Results and Diagnostics
# 'diagnostics = TRUE' reports Weak instruments, Wu-Hausman, and Sargan tests
summary_joint <- summary(iv_model_joint, diagnostics = TRUE)
print(summary_joint)

# Extract Sargan specific values for manual checking
sargan_test <- summary_joint$diagnostics["Sargan", ]
cat("\n--- Sargan Test Results ---\n")
cat("Statistic:", sargan_test["statistic"], "\n")
cat("P-value:", sargan_test["p-value"], "\n")

if(sargan_test["p-value"] < 0.05){
  cat("Conclusion: REJECT H0. At least one instrument is invalid.\n")
} else {
  cat("Conclusion: FAIL TO REJECT H0. The overidentifying restrictions are valid.\n")
}

#Generate Table comparing Single IV (INT) vs Joint IV (INT+CHI)
# We assume 'iv_model_proper' (from Q8f) exists. If not, re-run Q8f code first.
stargazer(iv_model_proper, iv_model_joint,
          type = "latex", header = FALSE,
          title = "IV Estimation: Single vs Joint Instruments",
          label = "tab:iv_joint_results",
          column.labels = c("IV: INT", "IV: INT + CHI"),
          dep.var.labels = "ln(CO2)",
          add.lines = list(c("Sargan Test (p-val)", "NA", round(sargan_test["p-value"], 3))))



