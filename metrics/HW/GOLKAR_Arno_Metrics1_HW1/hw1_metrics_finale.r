##Homework 1 econometrics GOLKAR Arno
# -----------
rm(list = ls())
install.packages("kableExtra")
install.packages("readxl")
install.packages("purrr")
install.packages("moments")
install.packages("car")
#-----------
library(moments)
library(car)
library(readr) 
library(dplyr)
library(ggplot2)
library(broom)
library(purrr)
library(sandwich)
library(lmtest)
library(scales)
library(modelsummary)
library(kableExtra)
library(knitr)
library(readxl)
# -------------

#  M Data
congestion <- read_excel("~/Desktop/M1 APE/Econometrics /HomeWork /HW1/congestion.xlsx")
glimpse(congestion)

# ------
# Q3) Create a table of descriptive statistics for all continuous variables
# ------

# Keeping only 2003
data2003 <- congestion %>% filter(Year == 2003)

# Selecting continuous variables
cont_2003 <- data2003 %>%
  select(
    starts_with("VKT_"),
    starts_with("LANE_"),
    any_of(c("POP","ELEV","RUG","HEAT","COOL","SPW"))
  ) %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.)))) %>% # to convert the column's values into numbers. 
  #If a value can't be converted, becomes NA.
  select(where(is.numeric))

# Creating for rich descriptives
desc_fun <- function(x) {
  n   <- sum(!is.na(x))
  mn  <- mean(x, na.rm = TRUE)
  sdv <- sd(x, na.rm = TRUE)
  tibble(
    n        = n,
    mean     = mn,
    sd       = sdv,
    min      = min(x, na.rm = TRUE),
    p1       = as.numeric(quantile(x, 0.01, na.rm = TRUE, names = FALSE)),
    p25      = as.numeric(quantile(x, 0.25, na.rm = TRUE, names = FALSE)),
    median   = median(x, na.rm = TRUE),
    p75      = as.numeric(quantile(x, 0.75, na.rm = TRUE, names = FALSE)),
    p99      = as.numeric(quantile(x, 0.99, na.rm = TRUE, names = FALSE)),
    max      = max(x, na.rm = TRUE),
    IQR      = IQR(x, na.rm = TRUE),
    CV       = ifelse(is.finite(mn) & mn != 0, sdv / mn, NA_real_),
    skewness = moments::skewness(x, na.rm = TRUE),
    kurtosis = moments::kurtosis(x, na.rm = TRUE)
  )
}

## For a numeric vector x (a column):
# n: number of non-missing observations.
#	mean, sd: central tendency and dispersion.
#	min, max: range endpoints.
#	p1, p25, median, p75, p99: percentiles (1st, 25th, 50th, 75th, 99th) to show the distribution and tails.
#	IQR: interquartile range = p75 − p25 (robust dispersion).
#	CV: coefficient of variation = sd/mean (scale-free dispersion).
#	skewness, kurtosis: shape (asymmetry and tail heaviness; moments::kurtosis returns excess kurtosis where 0 ≈ normal).

# Building the table
desc_table_q3 <- map_dfr(cont_2003, desc_fun, .id = "variable") %>%
  mutate(across(where(is.numeric), ~ round(., 3)))


# Ordering like the paper
preferred_order <- c(
  "VKT_IH","VKT_IHU","VKT_IHNU",
  "LANE_IH","LANE_IHU","LANE_IHNU",
  "POP","ELEV","RUG","HEAT","COOL","SPW"
)
desc_table_q3 <- desc_table_q3 %>%
  mutate(variable = factor(variable, levels = intersect(preferred_order, variable))) %>%
  arrange(variable) %>%
  mutate(variable = as.character(variable))

# Display and save
print(desc_table_q3, n = nrow(desc_table_q3))
write.csv(desc_table_q3, "q3_descriptives_2003.csv", row.names = FALSE)

# ------
# Q4) a. Correlation and plot for VKT_IH vs LANE_IH by Year
# ------

# Ensure variables are numeric and Year is a factor for plotting
# We use the full 'congestion' dataframe here
congestion_q4 <- congestion %>%
  mutate(
    VKT_IH = as.numeric(VKT_IH),
    LANE_IH = as.numeric(LANE_IH),
    Year = as.factor(Year) # Treat Year as a categorical variable for colors
  )

# 1. Calculate and print empirical correlation coefficients 
cat("--- Correlation Coefficients (VKT_IH vs LANE_IH) ---\n")

# By year
correlations_by_year <- congestion_q4 %>%
  group_by(Year) %>%
  summarize(
    correlation = cor(VKT_IH, LANE_IH, use = "complete.obs")
  )
print(correlations_by_year)

# Overall correlation 
overall_correlation <- cor(congestion_q4$VKT_IH, congestion_q4$LANE_IH, use = "complete.obs")
cat("\nOverall correlation:", round(overall_correlation, 4), "\n")


# 2. Create the scatter plot 
q4a_plot <- ggplot(congestion_q4, aes(x = LANE_IH, y = VKT_IH, color = Year)) +
  geom_point(alpha = 0.6) + # Points with some transparency
  geom_smooth(method = "lm", se = FALSE, aes(group = Year)) + # Add a regression line per year
  labs(
    title = "Daily VKT vs. Lane Kilometers by Year",
    x = "Lane Kilometers (LANE_IH)",
    y = "Daily VKT (VKT_IH, in '000 km)",
    color = "Year"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) + # Format y-axis labels
  scale_x_continuous(labels = scales::comma)  # Format x-axis labels

print(q4a_plot)

# Save the plot 
ggsave("q4a_vkt_vs_lane_plot.png", plot = q4a_plot, width = 8, height = 6)

cat("\nPlot successfully saved as 'q4a_vkt_vs_lane_plot.png'\n")


# ------
# Q4) b. Correlation and plot for LANE_IH vs POP by Year
# ------

# We can re-use the 'congestion_q4' data frame from Q4a,
# but we need to make sure POP is numeric.
congestion_q4b <- congestion %>%
  mutate(
    LANE_IH = as.numeric(LANE_IH),
    POP = as.numeric(POP),
    Year = as.factor(Year) # Treat Year as a categorical variable for colors
  )

# 1. Calculate and print empirical correlation coefficients
cat("\n--- Correlation Coefficients (LANE_IH vs POP) ---\n")

# By year
correlations_b_by_year <- congestion_q4b %>%
  group_by(Year) %>%
  summarize(
    correlation = cor(LANE_IH, POP, use = "complete.obs") #only use rows where both para have values
  )
print(correlations_b_by_year)

# Overall
overall_correlation_b <- cor(congestion_q4b$LANE_IH, congestion_q4b$POP, use = "complete.obs")
cat("\nOverall correlation:", round(overall_correlation_b, 4), "\n")


# 2. Create the scatter plot
q4b_plot <- ggplot(congestion_q4b, aes(x = POP, y = LANE_IH, color = Year)) +
  geom_point(alpha = 0.6) + # Points with some transparency
  geom_smooth(method = "lm", se = FALSE, aes(group = Year)) + # Add a regression line per year
  labs(
    title = "Lane Kilometers vs. Population by Year",
    x = "Population (POP)",
    y = "Lane Kilometers (LANE_IH)",
    color = "Year"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) + # Format y-axis labels
  scale_x_continuous(labels = scales::comma)  # Format x-axis labels


print(q4b_plot)

# Save the plot 
ggsave("q4a_lane_vs_pop_plot.png", plot = q4b_plot, width = 8, height = 6)

cat("\nPlot successfully saved as 'q4a_lane_vs_pop_plot.png'\n")


# ------
# Q7) Estimate Model M1
# ------
# We assume 'congestion' is our main dataframe.
# We must re-filter from 'congestion' to get the 'DIV' column.
# The 'data2003' from Q3 only had continuous variables.

# 1. Prepare the data for M1 (using 2003 data)
data_m1 <- congestion %>%
  filter(Year == 2003) %>% 
  mutate(across(c(VKT_IH, LANE_IH, POP, ELEV, RUG, HEAT, COOL, SPW), as.numeric)) %>%
  mutate(
    log_vkt_ih = log(VKT_IH),
    log_lane_ih = log(LANE_IH),
    log_pop = log(POP),
    DIV_factor = as.factor(DIV) # Treat DIV as categorical
  )

# 2. Define the model formula
# We know R will automatically handle the dummy trap (Q5) by dropping one DIV level
formula_m1 <- log_vkt_ih ~ log_lane_ih + log_pop +
  ELEV + RUG + HEAT + COOL + SPW +
  DIV_factor

# 3. Estimate the OLS model
model_m1 <- lm(formula_m1, data = data_m1)

# 4. Report the results (for the LaTeX table)
# We use 'modelsummary' 
modelsummary(model_m1,
             output = "q7_model_m1_results.tex",
             title = "OLS Estimation of Model M1 (2003)",
             stars = TRUE,
             coef_rename = c("log_lane_ih" = "ln(LANE_IH)", "log_pop" = "ln(POP)"),
             gof_map = c("nobs", "r.squared", "adj.r.squared", "f", "p.value"))

cat("\n--- Q7 & Q7a: Model M1 Summary (Use these values for LaTeX) ---\n")
print(summary(model_m1))


# ------
# Q7) a. Manual F-Test Calculation
# ------

# 1. Get values from the software summary
summary_m1 <- summary(model_m1)

# 2. Get RSS (unrestricted model M1)
RSS_unrestricted <- sum(residuals(model_m1)^2)
cat("\nRSS (Unrestricted, M1):", RSS_unrestricted, "\n")

# 3. Estimate the restricted model (M0: intercept only)
model_restricted <- lm(log_vkt_ih ~ 1, data = data_m1)
RSS_restricted <- sum(residuals(model_restricted)^2) # This is also the Total Sum of Squares (TSS)
cat("RSS (Restricted, M0 / TSS):", RSS_restricted, "\n")

# 4. Get parameters for the F-statistic formula
n <- nobs(model_m1)         # Number of observations
k <- model_m1$rank          # Number of parameters (K) in the unrestricted model
q <- k - 1                   # Number of restrictions (all slopes = 0)
df_denom <- n - k            # Denominator degrees of freedom

cat("n (observations):", n, "\n")
cat("k (parameters, incl. intercept):", k, "\n")
cat("q (restrictions, k-1):", q, "\n")
cat("n-k (denominator df):", df_denom, "\n")

# 5. F-statistic formula: F = [(TSS - RSS) / q] / [RSS / (n - k)]
f_statistic_manual <- ((RSS_restricted - RSS_unrestricted) / q) / (RSS_unrestricted / df_denom)
cat("Manually calculated F-statistic:", f_statistic_manual, "\n")

# 6. Get the critical value at 5%
alpha <- 0.05
critical_value <- qf(1 - alpha, q, df_denom)
cat("Critical F-value at 5% (F_crit):", critical_value, "\n")

cat("\n--- End of Q7a Calculation ---\n")

# Code to Export Q7 Table as PNG
table_object <- modelsummary(
  model_m1,
  output = "latex_tabular", 
  stars = TRUE,
  coef_rename = c("log_lane_ih" = "ln(LANE_IH)", 
                  "log_pop" = "ln(POP)",
                  "DIV_factor2" = "DIV 2",
                  "DIV_factor3" = "DIV 3",
                  "DIV_factor4" = "DIV 4",
                  "DIV_factor5" = "DIV 5",
                  "DIV_factor6" = "DIV 6",
                  "DIV_factor7" = "DIV 7",
                  "DIV_factor8" = "DIV 8",
                  "DIV_factor9" = "DIV 9"
  ),
  gof_map = c("nobs", "r.squared", "adj.r.squared", "f", "p.value")
)


latex_string_output <- capture.output(print(table_object))

writeLines(latex_string_output, "q7_model_m1_final.tex")

cat("File 'q7_model_m1_final.tex' has been created.\n")


# ------
# Q7) d. Test H0: beta_1 = 1
# ------

# We test the hypothesis on our existing model 'model_m1'
# The hypothesis is that the coefficient for "log_lane_ih" is 1.

cat("\n--- Q7d: Test H0: log_lane_ih = 1 ---\n")

# Method 1: Using the F-test.
# This is asymptotically valid even without normality.
f_test_q7d <- linearHypothesis(model_m1, "log_lane_ih = 1", test = "F")
print("--- F-test result ---")
print(f_test_q7d)


# Method 2: Calculating the t-statistic manually (just to flex)
cat("\n--- Manual t-test calculation ---\n")
summary_m1 <- summary(model_m1)
beta_1 <- coef(summary_m1)["log_lane_ih", "Estimate"]
se_beta_1 <- coef(summary_m1)["log_lane_ih", "Std. Error"]
print(se_beta_1)

# The t-statistic for H0: beta_1 = 1
t_stat_manual <- (beta_1 - 1) / se_beta_1

cat("Estimate (beta_1):", beta_1, "\n")
cat("Std. Error (se_beta_1):", se_beta_1, "\n")
cat("Manual t-statistic:", t_stat_manual, "\n")

# Get the 1% critical value, normal distribution being two-sided
alpha <- 0.01
# Asymptotic N(0,1) critical value
critical_value_z <- qnorm(1 - alpha/2)
cat("Asymptotic N(0,1) critical value at 1% (z_crit):", critical_value_z, "\n")


# ------
# Q8) Investigating Urban Sprawl (SPW)
# ------
# We choose the 'IHU' (urbanized interstates) category.
# This choice is justified as Question 8 investigates "urban sprawl" (SPW),
# a concept related to the urbanized portion of an MSA.

# ------
# Q8) a. Prepare data, dropping zero-value observations
# ------

cat("\n--- Q8a: Dropping zero-value observations ---\n")

# 1. Start from the full 2003 data
# We ensure all columns (including DIV) are numeric for R to handle
data_2003_full <- congestion %>%
  filter(Year == 2003) %>%
  mutate(
    across(c(VKT_IH, LANE_IH, VKT_IHU, LANE_IHU, VKT_IHNU, LANE_IHNU, 
             POP, ELEV, RUG, HEAT, COOL, SPW, DIV), as.numeric)
  )

n_before <- nrow(data_2003_full)
cat("Original number of MSAs in 2003:", n_before, "\n")

# 2. Drop obs where VKT_IHU or LANE_IHU are zero (for log())
data_q8 <- data_2003_full %>%
  filter(VKT_IHU > 0 & LANE_IHU > 0)

n_after <- nrow(data_q8)
n_dropped <- n_before - n_after
cat("MSAs after dropping zeros for IHU:", n_after, "\n")
cat("Total MSAs dropped:", n_dropped, "\n")

# 3. No new log variables are created. 'data_q8' is ready.

# ------
# Q8) b. Create sprawl dummy variable
# ------
cat("\n--- Q8b: Creating Sprawl Dummy ---\n")

# 1. Calculate the median value of SPW
median_spw <- median(data_q8$SPW, na.rm = TRUE)
cat("Median value of SPW:", median_spw, "\n")

# 2. Create the new dummy variable 'SPW_high'
# This variable is ONLY for splitting the data, not for the regression.
data_q8 <- data_q8 %>%
  mutate(SPW_high = as.factor(ifelse(SPW >= median_spw, 1, 0)))

# 3. Report the number of MSAs in each group
sprawl_groups <- data_q8 %>%
  group_by(SPW_high) %>%
  summarize(
    count = n()
  )

cat("Number of MSAs in each group:\n")
print(sprawl_groups)


# ------
# Q8) c. Chow Test for structural break
# ------
cat("\n--- Q8c: Chow Test for Structural Break ---\n")

# 1. Define the model formula (This is Model M1, for IHU)
# We apply log() directly to the variables.
# We include SPW as a regressor, as it is part of Model M1.
# We use as.factor(DIV) to let R handle the dummies.
formula_q8_corrected <- log(VKT_IHU) ~ log(LANE_IHU) + log(POP) +
  ELEV + RUG + HEAT + COOL + SPW +
  as.factor(DIV)

# 2. Estimate the POOLED model (Restricted Model)
# This model assumes coefficients are the same for both groups.
model_pooled <- lm(formula_q8_corrected, data = data_q8)

# Get the RSS for the Restricted model
RSS_R <- sum(residuals(model_pooled)^2)
cat("RSS (Restricted / Pooled):", RSS_R, "\n")

# 3. Estimate models for each subgroup (Unrestricted Model)
# This allows coefficients to be different for each group.

# Model for Low-Sprawl group (SPW_high == 0)
model_low_sprawl <- lm(formula_q8_corrected, 
                       data = data_q8, 
                       subset = (SPW_high == 0)) # Using subset is cleaner
RSS_low <- sum(residuals(model_low_sprawl)^2)
cat("RSS (Low Sprawl group):", RSS_low, "\n")

# Model for High-Sprawl group (SPW_high == 1)
model_high_sprawl <- lm(formula_q8_corrected, 
                        data = data_q8, 
                        subset = (SPW_high == 1))
RSS_high <- sum(residuals(model_high_sprawl)^2)
cat("RSS (High Sprawl group):", RSS_high, "\n")

# The RSS for the Unrestricted model is the sum of the subgroup RSS
RSS_UR <- RSS_low + RSS_high
cat("RSS (Unrestricted / Sum of subgroups):", RSS_UR, "\n")


# 4. Calculate the Chow F-statistic
n <- nobs(model_pooled) # Total observations (e.g., 214)
k <- model_pooled$rank   # Number of parameters (it will be 16)

cat("Total observations (n):", n, "\n")
cat("Number of parameters (k):", k, "\n")

# The Chow test F-statistic formula:
# F = [ (RSS_R - RSS_UR) / q ] / [ RSS_UR / (n - 2k) ]
# Here, the number of restrictions 'q' is equal to 'k'.
q <- k 
df_num <- q         # Numerator degrees of freedom
df_den <- n - (2*k) # Denominator degrees of freedom

chow_f_stat <- ((RSS_R - RSS_UR) / df_num) / (RSS_UR / df_den)

cat("Chow F-statistic:", chow_f_stat, "\n")

# 5. Calculate the p-value for the F-statistic
p_value_chow <- pf(chow_f_stat, df_num, df_den, lower.tail = FALSE)
cat("p-value for Chow test:", p_value_chow, "\n")

cat("\n--- End of Q8c Calculation (Corrected) ---\n")


# ------
# Q9) b. White Test for Heteroskedasticity
# ------
cat("\n--- Q9b: White Test ---\n")

# The full test would regress squared residuals on all
# regressors, their squares, and all cross-products.
# Our model (model_m1) has k=16 parameters, meaning 15 regressors (X).
# The full auxiliary regression would have:
# 15 (linear) + 15 (squared) + (15 choose 2) cross-products
# (15*14)/2 = 105 cross-products.
# Total regressors in aux model = 15 + 15 + 105 = 135
# Our sample size is n = 228.
# This leaves only n - k_aux - 1 = 228 - 135 - 1 = 92 degrees of freedom.
# This test *consumes* a huge number of degrees of freedom and is
# very likely to overfit, making it unreliable.
# This is the answer to "Why might the results from this test be unreliable?".

# --- Simplified White Test (using fitted values) ---
# The prompt asks us to perform this test instead.
# Auxiliary regression: u_hat^2 ~ y_hat + y_hat^2
# This is a special case of the Breusch-Pagan test.
# We must run this auxiliary regression manually.

cat("\n--- Simplified White Test (Fitted Values) ---\n")

#  Get u_hat^2 (squared residuals from model_m1)
u_hat_sq <- residuals(model_m1)^2

#  Get y_hat (fitted values from model_m1)
y_hat <- fitted.values(model_m1)

#  Run the auxiliary regression
aux_reg_simple <- lm(u_hat_sq ~ y_hat + I(y_hat^2))
aux_summary <- summary(aux_reg_simple)

#  Get the R-squared from the auxiliary regression
aux_R_squared <- aux_summary$r.squared
cat("Auxiliary Regression R-squared:", aux_R_squared, "\n")

#  Get n
n <- nobs(model_m1)
cat("n (observations):", n, "\n")

#  Calculate the LM statistic: LM = n * R^2
q_simple <- 2 # degrees of freedom (for y_hat and y_hat^2)
LM_stat_simple <- n * aux_R_squared
cat("LM Statistic (n * R^2):", LM_stat_simple, "\n")

#  Calculate the p-value for the LM statistic
p_value_simple <- pchisq(LM_stat_simple, df = q_simple, lower.tail = FALSE)

#    This is often preferred in finite samples.
f_test_stat <- aux_summary$fstatistic[1] # The F-value
df_num <- aux_summary$fstatistic[2]      # numerator df (q=2)
df_den <- aux_summary$fstatistic[3]      # denominator df (n-q-1)
f_p_value <- pf(f_test_stat, df_num, df_den, lower.tail = FALSE)
cat("F-statistic from auxiliary regression:", f_test_stat, "\n")
cat("p-value for F-statistic:", f_p_value, "\n")

cat("\n--- End of Q9b Calculation ---\n")

# ------
# Q9) d. Re-estimate M1 with Robust Standard Errors
# ------

cat("\n--- Q9d: Re-estimation with Robust SEs ---\n")

# 1. We start with our original OLS model, 'model_m1'
# The beta coefficients (0.749, 0.492) are unchanged.

# 2. Calculate the new, robust variance-covariance matrix
#    We can just use the "sandwich" formula
#    The 'vcovHC' function calculates White's estimator
robust_vcov <- vcovHC(model_m1, type = "HC1")

# 3. Get the new (robust) model summary table
#    We use 'coeftest()' from lmtest, which allows us to plug in our new robust variance matrix.
robust_summary <- coeftest(model_m1, vcov = robust_vcov)

cat("--- Original OLS Summary (for comparison) ---\n")
print(summary(model_m1))

cat("\n--- NEW Robust Summary (for Q9d) ---\n")
print(robust_summary)

cat("\n--- End of Q9d Calculation ---\n")

# Code to Export Q9d Robust Table
table_object_robust <- modelsummary(
  model_m1,
  output = "latex_tabular",  
  stars = TRUE,
  vcov = robust_vcov, 
  coef_rename = c("log_lane_ih" = "ln(LANE_IH)", 
                  "log_pop" = "ln(POP)",
                  "DIV_factor2" = "DIV 2",
                  "DIV_factor3" = "DIV 3",
                  "DIV_factor4" = "DIV 4",
                  "DIV_factor5" = "DIV 5",
                  "DIV_factor6" = "DIV 6",
                  "DIV_factor7" = "DIV 7",
                  "DIV_factor8" = "DIV 8",
                  "DIV_factor9" = "DIV 9"
  ),
  gof_map = c("nobs", "r.squared", "adj.r.squared") 
)

latex_string_robust <- capture.output(print(table_object_robust))

writeLines(latex_string_robust, "q9d_robust_final.tex")

cat("File 'q9d_robust_final.tex' has been created.\n")

# ------
# Q9) e. Breusch-Pagan Test for POP
# ------

cat("\n--- Q9e: Breusch-Pagan Test ---\n")

# We suspect variance depends on "an MSA's population", which is the variable 'POP'.
# The test will run an auxiliary regression of: u_hat^2 ~ POP
# We must explicitly pass 'data = data_m1' to the function,
# because the 'model_m1' object itself does not contain the 'POP' column (it only contains 'log_pop').

bp_test_pop <- bptest(model_m1, varformula = ~ POP, data = data_m1)

cat("--- Breusch-Pagan test result (vs POP) ---\n")
print(bp_test_pop)

# For completeness, it's also wise to test against 'log_pop',the variable actually included in our regression.
bp_test_logpop <- bptest(model_m1, varformula = ~ log_pop, data = data_m1)

cat("\n--- Breusch-Pagan test result (vs log_pop) ---\n")
print(bp_test_logpop)

cat("\n--- End of Q9e Calculation ---\n")


# ------
# Q9) f. Plot Residuals vs. Population (POP)
# ------
cat("\n--- Q9f: Plotting Residuals vs. POP ---\n")

# 1. Create a new dataframe for plotting
# We add the residuals from 'model_m1' as a new column
# to our 'data_m1' dataframe, which contains the 'POP' variable.
plot_data_q9f <- data_m1 %>%
  mutate(Residuals = residuals(model_m1))

# 2. Create the scatter plot
q9f_plot <- ggplot(plot_data_q9f, aes(x = POP, y = Residuals)) +
  geom_point(alpha = 0.6) + # Scatter plot points
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Add a y=0 line
  geom_smooth(method = "loess", se = FALSE, color = "blue", linewidth = 0.5) + # Add a trend line
  labs(
    #title = "Plot of Residuals vs. Population",
    x = "Population (POP)",
    y = "Residuals (from Model M1)"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma) # Format x-axis numbers

print(q9f_plot)

ggsave("q9f_resid_vs_pop.png", plot = q9f_plot, width = 8, height = 6)

cat("Plot 'q9f_resid_vs_pop.png' has been saved to your directory.\n")
cat("\n--- End of Q9f Calculation ---\n")

# ------
# Q9) g. Goldfeld-Quandt Test
# ------

cat("\n--- Q9g: Goldfeld-Quandt Test (vs POP) ---\n")

# 1. Perform the GQ test
# We test the hypothesis based on our conclusion in Q9f:
# We saw *decreasing* variance as POP increases.
#
# The GQ test sorts the data by 'order.by' (POP).
# It runs a model on the low-POP group (getting RSS1)
# and a model on the high-POP group (getting RSS2).
#
# The test statistic is F = RSS2 / RSS1.
#
# If variance decreases, the low-POP group (Group 1) has *high* variance
# and the high-POP group (Group 2) has *low* variance.
# This means we expect RSS1 > RSS2.
# Therefore, we expect the F-statistic (RSS2 / RSS1) to be < 1.
#
# Our alternative hypothesis is "less".

gq_test_pop <- gqtest(model_m1, 
                      order.by = ~ POP, 
                      fraction = 0.2,       # As per the hint
                      alternative = "less", # Test for decreasing variance
                      data = data_m1)

print(gq_test_pop)

cat("\n--- End of Q9g Calculation ---\n")


# ------
# Q9) h. Weighted Least Squares (WLS) Estimation
# ------

cat("\n--- Q9h: Weighted Least Squares (WLS) Estimation ---\n")

# 1. Estimate the WLS model
# We assume variance is proportional to POP, so weights = 1/POP.
# We use the formula 'formula_m1' and data 'data_m1' from Question 7.
model_wls <- lm(formula_m1, data = data_m1, weights = 1/POP)

# 2. Display the WLS model summary
cat("\n--- WLS Model Summary (Q9h) ---\n")
print(summary(model_wls))

# 3. Create and print the comparison table to the console
# Includes: Classic OLS (Q7), OLS with Robust SEs (Q9d), and WLS (Q9h)
# We calculate the robust matrix for the OLS model first
robust_vcov_matrix <- vcovHC(model_m1, type = "HC1")

cat("\n--- Comparison Table (OLS, OLS-Robust, WLS) ---\n")
modelsummary(
  list(
    "OLS (Q7)" = model_m1,
    "OLS-Robust (Q9d)" = model_m1, # Use same model, but specify vcov below
    "WLS (Q9h)" = model_wls
  ),
  stars = TRUE,
  # Apply robust SEs only to the second model in the list
  vcov = list(NULL, robust_vcov_matrix, NULL),
  coef_rename = c("log_lane_ih" = "ln(LANE_IH)", "log_pop" = "ln(POP)"),
  gof_map = c("nobs", "r.squared"),
  # Output directly to console (default R Markdown table format)
  output = "markdown",
  title = "Comparison of OLS, OLS-Robust, and WLS Estimators"
)

cat("\n--- End of Q9h Calculation ---\n")

# Code to Export Q9h Comparison Table 
table_object_comparison <- modelsummary(
  list(
    "OLS (Q7)" = model_m1,
    "OLS-Robust (Q9d)" = model_m1,
    "WLS (Q9h)" = model_wls
  ),
  output = "latex_tabular",
  stars = TRUE,
  vcov = list(NULL, robust_vcov_matrix, NULL),
  coef_rename = c("log_lane_ih" = "ln(LANE_IH)", "log_pop" = "ln(POP)",
                  "DIV_factor2" = "DIV 2", "DIV_factor3" = "DIV 3",
                  "DIV_factor4" = "DIV 4", "DIV_factor5" = "DIV 5",
                  "DIV_factor6" = "DIV 6", "DIV_factor7" = "DIV 7",
                  "DIV_factor8" = "DIV 8", "DIV_factor9" = "DIV 9"),
  gof_map = c("nobs", "r.squared"),
  title = "Comparison of OLS, OLS-Robust, and WLS Estimators"
)

latex_string_comparison <- capture.output(print(table_object_comparison))

writeLines(latex_string_comparison, "q9h_comparison_final.tex")

cat("File 'q9h_comparison_final.tex' has been created.\n")

# ------
# Q9) h. Weighted Least Squares (WLS) Estimation (Corrected Logic)
# ------

cat("\n--- Q9h: Weighted Least Squares (WLS) Estimation ---\n")

# 1. Estimate the WLS model
# Our tests in Q9f and Q9g showed DECREASING variance with POP.

model_wls <- lm(formula_m1, data = data_m1, weights = POP)

# 2. Display the WLS model summary
cat("\n--- WLS Model Summary (Q9h) ---\n")
print(summary(model_wls))

# 3. Create and print the comparison table to the console
robust_vcov_matrix <- vcovHC(model_m1, type = "HC1")

cat("\n--- Comparison Table (OLS, OLS-Robust, WLS) ---\n")
modelsummary(
  list(
    "OLS (Q7)" = model_m1,
    "OLS-Robust (Q9d)" = model_m1, 
    "WLS (Q9h)" = model_wls
  ),
  stars = TRUE,
  vcov = list(NULL, robust_vcov_matrix, NULL),
  coef_rename = c("log_lane_ih" = "ln(LANE_IH)", "log_pop" = "ln(POP)"),
  gof_map = c("nobs", "r.squared"),
  output = "markdown",
  title = "Comparison of OLS, OLS-Robust, and WLS Estimators"
)

cat("\n--- End of Q9h Calculation ---\n")

# Code to Export Q9h Comparison Table 
table_object_comparison <- modelsummary(
  list(
    "OLS (Q7)" = model_m1,
    "OLS-Robust (Q9d)" = model_m1,
    "WLS (Q9h)" = model_wls
  ),
  output = "latex_tabular",
  stars = TRUE,
  vcov = list(NULL, robust_vcov_matrix, NULL),
  coef_rename = c("log_lane_ih" = "ln(LANE_IH)", "log_pop" = "ln(POP)",
                  "DIV_factor2" = "DIV 2", "DIV_factor3" = "DIV 3",
                  "DIV_factor4" = "DIV 4", "DIV_factor5" = "DIV 5",
                  "DIV_factor6" = "DIV 6", "DIV_factor7" = "DIV 7",
                  "DIV_factor8" = "DIV 8", "DIV_factor9" = "DIV 9"),
  gof_map = c("nobs", "r.squared"),
  title = "Comparison of OLS, OLS-Robust, and WLS Estimators"
)

latex_string_comparison <- capture.output(print(table_object_comparison))

writeLines(latex_string_comparison, "q9h_comparison_final.tex")

cat("File 'q9h_comparison_final.tex' has been created.\n")

# ------
# Q9) h. Feasible Weighted Least Squares (FGLS) Estimation
# ------

cat("\n--- Q9h: Feasible WLS (FGLS) Estimation ---\n")

# --- Step 1: Get the log of squared residuals from the OLS model (model_m1) ---
# (model_m1 was already estimated in Q7)
log_u_hat_sq <- log(residuals(model_m1)^2)

# --- Step 2: Estimate the variance function ---
# We regress the log squared residuals on log(POP) from our data_m1,
# because Q9e showed this relationship was significant.
# (We must add log_u_hat_sq to data_m1 so lm() can find it)
data_m1_fgls <- data_m1 %>%
  mutate(log_u_hat_sq = log_u_hat_sq)

lm_var <- lm(log_u_hat_sq ~ log_pop, data = data_m1_fgls)

cat("\n--- Auxiliary Variance Model (log(u^2) ~ log(POP)) ---\n")
print(summary(lm_var))

# --- Step 3: Calculate the weights ---
# fitted_var is the predicted variance for each observation (sigma_i^2)
fitted_var <- exp(fitted(lm_var))
# The weight is the inverse of the variance
weights_fgls <- 1 / fitted_var

# --- Step 4: Estimate the M1 model using FGLS ---
# We use the *same* formula (formula_m1) and the *same* data (data_m1)
# but we add our new calculated weights
model_fgls <- lm(formula_m1, data = data_m1, weights = weights_fgls)

cat("\n--- FGLS Model Summary (Q9h) ---\n")
print(summary(model_fgls))

# --- Step 5: Final Comparison (as in your script) ---
# We need the robust matrix for the second column
robust_vcov_matrix <- vcovHC(model_m1, type = "HC1")

cat("\n--- Comparison Table (OLS, OLS-Robust, FGLS) ---\n")
modelsummary(
  list(
    "OLS (Q7)" = model_m1,
    "OLS-Robust (Q9d)" = model_m1,
    "FGLS (Q9h)" = model_fgls # Here is our new FGLS model
  ),
  stars = TRUE,
  # Apply robust SEs only to the 2nd model
  vcov = list(NULL, robust_vcov_matrix, NULL),
  coef_rename = c("log_lane_ih" = "ln(LANE_IH)", "log_pop" = "ln(POP)"),
  gof_map = c("nobs", "r.squared"),
  output = "markdown", 
  title = "Comparison of OLS, OLS-Robust, and FGLS Estimators"
)

cat("\n--- Exporting LaTeX comparison table ---\n")

table_object_comparison <- modelsummary(
  list(
    "OLS (Q7)" = model_m1,
    "OLS-Robust (Q9d)" = model_m1,
    "FGLS (Q9h)" = model_fgls
  ),
  output = "latex_tabular",
  stars = TRUE,
  vcov = list(NULL, robust_vcov_matrix, NULL),
  coef_rename = c("log_lane_ih" = "ln(LANE_IH)", "log_pop" = "ln(POP)",
                  "DIV_factor2" = "DIV 2", "DIV_factor3" = "DIV 3",
                  "DIV_factor4" = "DIV 4", "DIV_factor5" = "DIV 5",
                  "DIV_factor6" = "DIV 6", "DIV_factor7" = "DIV 7",
                  "DIV_factor8" = "DIV 8", "DIV_factor9" = "DIV 9"),
  gof_map = c("nobs", "r.squared"),
  title = "Comparison of OLS, OLS-Robust, and FGLS Estimators"
)

latex_string_comparison <- capture.output(print(table_object_comparison))
writeLines(latex_string_comparison, "q9h_comparison_final.tex")
cat("File 'q9h_comparison_final.tex' created.\n")

cat("\n--- End of Q9h (FGLS) Calculation ---\n")