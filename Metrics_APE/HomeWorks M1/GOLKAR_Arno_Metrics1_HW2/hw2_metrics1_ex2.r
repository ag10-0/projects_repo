# ==============================================================================
# Econometrics 1 - Homework 2 - Exercise 2
# Simulation: To Work or Not to Work?
# ==============================================================================

# 0. SETUP ---------------------------------------------------------------------
rm(list = ls())
library(AER)      
library(ggplot2)  
library(dplyr)    
library(tidyr)    

set.seed(123)     

# ==============================================================================
# 1. CALIBRATION & PARAMETERS 
# ==============================================================================

# --- From Table 1 (Descriptive Statistics) ---
# Calculation: Variance = (SE * sqrt(N))^2
N_paper <- 3179

mu_BirthW  <- 3.409
se_BirthW  <- 0.010
var_BirthW <- (se_BirthW * sqrt(N_paper))^2  # approx 0.318
sd_BirthW  <- sqrt(var_BirthW)

mu_MAge    <- 34.63
se_MAge    <- 0.102
var_MAge   <- (se_MAge * sqrt(N_paper))^2    # approx 33.06
sd_MAge    <- sqrt(var_MAge)

# --- From Table 5 (Robustness / IV Results) ---
# "True" Coefficients
alpha1 <- -0.1    # (Assumed) Effect of BirthW on CDev
beta1  <- -0.181  # Effect of CDev on MLFP (Updated from Table)
beta2  <- 0.5     # (Assumed) Effect of BirthW on MLFP
beta3  <- 1.2     # (Assumed) Effect of MAge on MLFP
beta4  <- -0.02   # (Assumed) Effect of MAge^2 on MLFP

# --- Fixed parameters from Homework PDF ---
mu_MEduc  <- 12
var_MEduc <- 4
sd_MEduc  <- sqrt(var_MEduc)
p_Left    <- 0.1
var_nu    <- 0.5
sd_nu     <- sqrt(var_nu)
var_eps   <- 1
sd_eps    <- sqrt(var_eps)

# ==============================================================================
# 2. DGP FUNCTION
# ==============================================================================
generate_data <- function(n, type = "original") {
  
  # Exogenous variables
  BirthW <- rnorm(n, mean = mu_BirthW, sd = sd_BirthW)
  MEduc  <- rnorm(n, mean = mu_MEduc, sd = sd_MEduc)
  MAge   <- rnorm(n, mean = mu_MAge, sd = sd_MAge)
  MAge2  <- (MAge - mu_MAge)^2
  Left   <- rbinom(n, size = 1, prob = p_Left)
  
  # Error terms
  nu  <- rnorm(n, mean = 0, sd = sd_nu)
  eps <- rnorm(n, mean = 0, sd = sd_eps)
  
  # Endogenous Variable: CDev (Child Development)
  if (type == "original") {
    # Eq 2: MEduc is included (-0.303 * MEduc)
    CDev <- 4.9 + alpha1 * BirthW - 0.303 * MEduc + 0.319 * Left + nu
  } else {
    # Eq 2': MEduc is excluded (Question 7)
    CDev <- 4.9 + alpha1 * BirthW + 0.319 * Left + nu
  }
  
  # Outcome Variable: MLFP (Maternal Labor Force Participation)
  # Equation (1)
  MLFP <- 28 + beta1 * CDev + beta2 * BirthW + 0.173 * MEduc + 
    beta3 * MAge + beta4 * MAge2 + eps
  
  # Return data frame
  return(data.frame(MLFP, CDev, BirthW, MEduc, MAge, MAge2, Left))
}

# ==============================================================================
# 3. SINGLE SAMPLE ESTIMATION (QUESTION 5)
# ==============================================================================
cat("\n--- QUESTION 5: Single Sample Estimation ---\n")

# 5. Simulate dataset
df_sample <- generate_data(n = 3000, type = "original")

# 5a. OLS Model 3 (Full specification)
ols_full <- lm(MLFP ~ CDev + BirthW + MEduc + MAge + MAge2, data = df_sample)
cat("\n(5a) OLS Full Model (Eq 3):\n")
print(summary(ols_full)$coefficients[, 1:2]) 

# 5c. OLS Model 4 (Omitted MEduc)
ols_omitted <- lm(MLFP ~ CDev + BirthW + MAge + MAge2, data = df_sample)
cat("\n(5c) OLS Omitted Variable Model (Eq 4 - No MEduc):\n")
print(summary(ols_omitted)$coefficients[, 1:2])

# 5f. IV Estimator (Instrumenting CDev with Left)
iv_model <- ivreg(MLFP ~ CDev + BirthW + MEduc + MAge + MAge2 | 
                    BirthW + MEduc + MAge + MAge2 + Left, 
                  data = df_sample)
cat("\n(5f) IV Estimator (2SLS):\n")
print(summary(iv_model)$coefficients[, 1:2])

# ==============================================================================
#Hypothesis Testing for Question 5
# ==============================================================================
true_beta1 <- beta1 

cat("\n--- HYPOTHESIS TESTS (True Beta1 =", true_beta1, ") ---\n")

# --- Q5b: Test OLS Full vs True Value ---
est_full <- coef(ols_full)["CDev"]
se_full  <- summary(ols_full)$coefficients["CDev", "Std. Error"]
t_stat_full <- (est_full - true_beta1) / se_full
p_val_full  <- 2 * (1 - pt(abs(t_stat_full), df = df.residual(ols_full)))

cat("\n[Q5b] OLS Full vs True:\n")
cat("Estimate:", est_full, "| t-stat:", t_stat_full, "| p-value:", p_val_full, "\n")

# --- Q5d: Test OLS Omitted vs True Value ---
est_omit <- coef(ols_omitted)["CDev"]
se_omit  <- summary(ols_omitted)$coefficients["CDev", "Std. Error"]
t_stat_omit <- (est_omit - true_beta1) / se_omit
p_val_omit  <- 2 * (1 - pt(abs(t_stat_omit), df = df.residual(ols_omitted)))

cat("\n[Q5d] OLS Omitted vs True:\n")
cat("Estimate:", est_omit, "| t-stat:", t_stat_omit, "| p-value:", p_val_omit, "\n")

# --- Q5f: Test IV vs True Value ---
est_iv <- coef(iv_model)["CDev"]
se_iv  <- summary(iv_model)$coefficients["CDev", "Std. Error"]
t_stat_iv <- (est_iv - true_beta1) / se_iv
p_val_iv  <- 2 * (1 - pt(abs(t_stat_iv), df = df.residual(iv_model)))

cat("\n[Q5f] IV vs True:\n")
cat("Estimate:", est_iv, "| t-stat:", t_stat_iv, "| p-value:", p_val_iv, "\n")

# ==============================================================================
# 4. DIAGNOSTICS (Weak IV & Hausman)
# ==============================================================================
cat("\n--- DIAGNOSTICS ---\n")

# First Stage F-Statistic
first_stage <- lm(CDev ~ Left + BirthW + MEduc + MAge + MAge2, data = df_sample)
f_stat <- summary(first_stage)$fstatistic
cat("\n[First Stage F-statistic]\n")
cat("F-stat:", f_stat[1], "\n")
if(f_stat[1] > 10) cat("Instrument is STRONG.\n") else cat("Instrument is WEAK.\n")

# Hausman Test
cat("\n[Hausman Test]\n")
hausman_test <- summary(iv_model, diagnostics = TRUE)
print(hausman_test$diagnostics)

# ==============================================================================
# 5. MONTE CARLO SIMULATION (QUESTION 6) - Original DGP
# ==============================================================================
cat("\n--- QUESTION 6: Monte Carlo Simulation (Original DGP) ---\n")

R <- 2000 # Number of replications
n <- 3000 # Sample size

# Vectors to store results
coef_ols_full    <- numeric(R)
coef_ols_omitted <- numeric(R)
coef_iv          <- numeric(R)

for (i in 1:R) {
  # 1. Generate data
  sim_data <- generate_data(n, type = "original")
  
  # 2. OLS Full
  m1 <- lm(MLFP ~ CDev + BirthW + MEduc + MAge + MAge2, data = sim_data)
  coef_ols_full[i] <- coef(m1)["CDev"]
  
  # 3. OLS Omitted
  m2 <- lm(MLFP ~ CDev + BirthW + MAge + MAge2, data = sim_data)
  coef_ols_omitted[i] <- coef(m2)["CDev"]
  
  # 4. IV
  m3 <- ivreg(MLFP ~ CDev + BirthW + MEduc + MAge + MAge2 | 
                BirthW + MEduc + MAge + MAge2 + Left, data = sim_data)
  coef_iv[i] <- coef(m3)["CDev"]
}

# Report Mean and Variance
results_q6 <- data.frame(
  Estimator = c("OLS_Full", "OLS_Omitted", "IV"),
  Mean_Est  = c(mean(coef_ols_full), mean(coef_ols_omitted), mean(coef_iv)),
  Variance  = c(var(coef_ols_full), var(coef_ols_omitted), var(coef_iv))
)
print(results_q6)

# Visualization Q6 (Bias-Variance Tradeoff)
df_sim_results <- data.frame(
  Estimate = c(coef_ols_full, coef_ols_omitted, coef_iv),
  Estimator = rep(c("1. OLS Full", "2. OLS Omitted", "3. IV"), each = R)
)

plot_q6 <- ggplot(df_sim_results, aes(x = Estimate, fill = Estimator)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = beta1, linetype = "dashed", size = 1, color = "black") +
  labs(title = "Simulation Results: Bias vs. Efficiency (Q6)",
       subtitle = paste("True Beta =", beta1),
       x = "Estimated Beta for CDev", y = "Density") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(plot_q6)

# ==============================================================================
# 6. MISSPECIFIED DGP SIMULATION (QUESTION 7) - New DGP
# ==============================================================================
cat("\n--- QUESTION 7: Monte Carlo Simulation (Misspecified DGP 2') ---\n")

# Vectors to store new results
coef_ols_full_7    <- numeric(R)
coef_ols_omitted_7 <- numeric(R)
coef_iv_7          <- numeric(R)

for (i in 1:R) {
  # 1. Generate data (Equation 2')
  sim_data <- generate_data(n, type = "misspecified")
  
  # 2. OLS Full
  m1 <- lm(MLFP ~ CDev + BirthW + MEduc + MAge + MAge2, data = sim_data)
  coef_ols_full_7[i] <- coef(m1)["CDev"]
  
  # 3. OLS Omitted
  m2 <- lm(MLFP ~ CDev + BirthW + MAge + MAge2, data = sim_data)
  coef_ols_omitted_7[i] <- coef(m2)["CDev"]
  
  # 4. IV
  m3 <- ivreg(MLFP ~ CDev + BirthW + MEduc + MAge + MAge2 | 
                BirthW + MEduc + MAge + MAge2 + Left, data = sim_data)
  coef_iv_7[i] <- coef(m3)["CDev"]
}

# Report Results for Q7
results_q7 <- data.frame(
  Estimator  = c("OLS_Full", "OLS_Omitted", "IV"),
  Mean_Est   = c(mean(coef_ols_full_7), mean(coef_ols_omitted_7), mean(coef_iv_7)),
  Variance   = c(var(coef_ols_full_7), var(coef_ols_omitted_7), var(coef_iv_7))
)
print(results_q7)

# Visualization Q7
df_sim_results_7 <- data.frame(
  Estimate = c(coef_ols_full_7, coef_ols_omitted_7, coef_iv_7),
  Estimator = rep(c("1. OLS Full", "2. OLS Omitted", "3. IV"), each = R)
)

plot_q7 <- ggplot(df_sim_results_7, aes(x = Estimate, fill = Estimator)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = beta1, linetype = "dashed", size = 1, color = "black") +
  labs(title = "Simulation Results: No Correlation (Q7)",
       subtitle = paste("True Beta =", beta1),
       x = "Estimated Beta for CDev", y = "Density") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(plot_q7)

# Save the plot for Question 6
ggsave("plot_q6.png", plot = plot_q6, width = 6, height = 4)

# Save the plot for Question 7
ggsave("plot_q7.png", plot = plot_q7, width = 6, height = 4)