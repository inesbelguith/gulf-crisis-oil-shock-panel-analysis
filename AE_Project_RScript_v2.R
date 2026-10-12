# =============================================================================
# R Script: Geopolitics, Oil Shock, and Macroeconomics
# Panel Data Analysis, Emerging Net-Importers
# Countries: Turkey, Poland, South Africa, Thailand, Indonesia, India
# =============================================================================

# ------------------------------------------------------------------------------
# 1. INSTALL AND LOAD REQUIRED LIBRARIES
# ------------------------------------------------------------------------------

# Install packages if not already installed
# install.packages(c("tidyverse", "plm", "lmtest", "stargazer", "ggplot2",

library(tidyverse)   # Data manipulation and visualization
library(plm)         # Panel data econometrics (core)
library(lmtest)      # Diagnostic tests (BP, coeftest)
library(sandwich)    # Robust standard errors (vcovHC, vcovSCC)
library(stargazer)   # Publication-quality regression tables
library(ggplot2)     # Advanced visualization
library(car)         # Linear hypothesis tests

# ------------------------------------------------------------------------------
# 2. LOAD THE PANEL DATASET
# ------------------------------------------------------------------------------

# Load the pre-assembled panel CSV (or build manually per Section 3 below)
data_raw <- read.csv("panel_data_clean.csv, stringsAsFactors = FALSE)
data_raw$date <- as.Date(data_raw$date)

# Inspect structure
str(data_raw)
head(data_raw)
summary(data_raw)

# Countries in sample
cat("Countries:", unique(data_raw$country), "\n")
cat("Time range:", as.character(min(data_raw$date)), "to",
    as.character(max(data_raw$date)), "\n")
cat("Observations:", nrow(data_raw), "\n")

# ------------------------------------------------------------------------------
# 3. DATA CONSTRUCTION (Manual, if building from scratch)
#    Sources: IMF IFS, OECD, BIS, FRED, National Statistics Offices
#    Variables: CPI (2015=100), IPI (2015=100), NEER (broad index),
#               Policy Rate (%), Brent Crude (USD/bbl), VIX
# ------------------------------------------------------------------------------

# Data was collected from:
# - CPI:         IMF International Financial Statistics (IFS) / OECD
# - IPI:         IMF IFS / OECD / National Stats Offices (TurkStat, GUS, Stats SA,
#                NSO Thailand, BPS Indonesia, MOSPI India)
# - NEER:        Bank for International Settlements (BIS), Broad index
# - Policy Rate: National Central Banks + BIS
# - Brent Oil:   FRED (ticker: DCOILBRENTEU), U.S. EIA
# - VIX:         FRED (ticker: VIXCLS)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


  ts_data <- ts(series_vec, frequency = 12)
  tryCatch({
    as.numeric(fc$mean)
  }, error = function(e) {
    # Fallback: naive linear extrapolation
    n <- length(series_vec)
    last3 <- series_vec[(n-2):n]
    trend <- lm(last3 ~ seq_along(last3))$coefficients
    trend[1] + trend[2] * (4:(3 + n_ahead))
  })
}

# turkey_cpi_hist <- data_raw %>% filter(country == "Turkey") %>% pull(cpi)

# ------------------------------------------------------------------------------
# 5. DATA PROCESSING AND TRANSFORMATIONS
# ------------------------------------------------------------------------------

data_processed <- data_raw %>%
  arrange(country, date) %>%
  group_by(country) %>%
  mutate(
    # Natural log transformations
    ln_cpi  = log(cpi),
    ln_ipi  = log(ipi),
    ln_neer = log(neer),

    # Oil price percentage change (exogenous shock variable)
    # Formula: (Brent_t - Brent_{t-1}) / Brent_{t-1} * 100
    oil_pct_change = (brent - lag(brent)) / lag(brent) * 100,
    oil_pct_change = ifelse(is.na(oil_pct_change), 0, oil_pct_change),

    # Post-crisis dummy: 1 from March 2026 onward (Gulf Crisis)
    post_crisis = as.integer(date >= as.Date("2026-03-01"))
  ) %>%
  ungroup() %>%
  filter(!is.na(ln_cpi), !is.na(ln_ipi))

cat("\nProcessed dataset: ", nrow(data_processed), "obs,",
    ncol(data_processed), "variables\n")

# ------------------------------------------------------------------------------
# 6. DESCRIPTIVE STATISTICS
# ------------------------------------------------------------------------------

desc_vars <- c("cpi", "ipi", "brent", "neer", "policy_rate", "vix", "oil_pct_change")

# Overall descriptive statistics
cat("\n--- Overall Descriptive Statistics ---\n")
print(summary(data_processed[, desc_vars]))

# By country
cat("\n--- Mean Values by Country ---\n")
data_processed %>%
  group_by(country) %>%
  summarise(across(all_of(desc_vars), mean, na.rm = TRUE)) %>%
  print()

# ------------------------------------------------------------------------------
# 7. EXPLORATORY DATA ANALYSIS (EDA), Plots
# ------------------------------------------------------------------------------

CRISIS_DATE <- as.Date("2026-03-01")
country_colors <- c(Turkey = "#E63946", Poland = "#457B9D",
                    `South Africa` = "#2A9D8F", Thailand = "#E9C46A",
                    Indonesia = "#F4A261", India = "#264653")

# Plot 1: Brent crude oil price over time
p1 <- ggplot(data_processed %>% distinct(date, brent), aes(x = date, y = brent)) +
  geom_line(color = "#C1440E", size = 1.2) +
  geom_vline(xintercept = as.numeric(CRISIS_DATE), linetype = "dashed",
             color = "navy", size = 0.8) +
  annotate("text", x = CRISIS_DATE + 30, y = max(data_processed$brent, na.rm=TRUE),
           label = "Gulf Crisis Start\n(Feb 27, 2026)", hjust = 0, color = "navy", size = 3.5) +
  annotate("rect", xmin = CRISIS_DATE, xmax = max(data_processed$date),
           ymin = -Inf, ymax = Inf, alpha = 0.07, fill = "red") +
  labs(title = "Brent Crude Oil Price: Pre and Post 2026 Gulf Crisis",
       x = "Date", y = "USD per Barrel") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

print(p1)

# Plot 2: Log CPI trends by country
p2 <- ggplot(data_processed, aes(x = date, y = ln_cpi, color = country)) +
  geom_line(size = 0.9) +
  geom_vline(xintercept = as.numeric(CRISIS_DATE), linetype = "dashed",
             color = "navy", size = 0.7) +
  scale_color_manual(values = country_colors) +
  labs(title = "Log CPI Trends by Country (Jan 2023 to Jun 2026)",
       x = "Date", y = "ln(CPI)", color = "Country") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

print(p2)

# Plot 3: Log IPI trends by country
p3 <- ggplot(data_processed, aes(x = date, y = ln_ipi, color = country)) +
  geom_line(size = 0.9) +
  geom_vline(xintercept = as.numeric(CRISIS_DATE), linetype = "dashed",
             color = "navy", size = 0.7) +
  scale_color_manual(values = country_colors) +
  labs(title = "Log Industrial Production Index by Country",
       x = "Date", y = "ln(IPI)", color = "Country") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

print(p3)

# Plot 4: Policy rates over time
p4 <- ggplot(data_processed, aes(x = date, y = policy_rate, color = country)) +
  geom_line(size = 0.9) +
  geom_vline(xintercept = as.numeric(CRISIS_DATE), linetype = "dashed",
             color = "navy", size = 0.7) +
  scale_color_manual(values = country_colors) +
  labs(title = "Central Bank Policy Rates by Country",
       x = "Date", y = "Policy Rate (%)", color = "Country") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

print(p4)

# ------------------------------------------------------------------------------
# 8. PANEL DATA SETUP
# ------------------------------------------------------------------------------

# Declare panel data frame (country = individual, date = time)
pdata <- pdata.frame(data_processed, index = c("country", "date"))

# Check panel balance
cat("\n--- Panel Structure ---\n")
print(pdim(pdata))
# Should show: 6 individuals, 42 time periods, balanced

# ------------------------------------------------------------------------------
# 9. ECONOMETRIC MODELS, CPI (INFLATION)
# ------------------------------------------------------------------------------

formula_cpi <- ln_cpi ~ oil_pct_change + ln_neer + policy_rate + vix + post_crisis

# --- Model 1a: Pooled OLS (CPI) ---
# Ignores panel structure; estimates average effects across all country-time pairs
pols_cpi <- plm(formula_cpi, data = pdata, model = "pooling")

# --- Model 1b: Fixed Effects (CPI) ---
# Controls for time-invariant unobserved country heterogeneity (e.g., energy mix,
# industrial structure) via within-group demeaning
# Equation: ln_cpi_it = alpha_i + beta'X_it + epsilon_it
fe_cpi <- plm(formula_cpi, data = pdata, model = "within")

# --- Model 1c: Random Effects (CPI) ---
# Assumes alpha_i ~ iid(0, sigma_alpha^2), uncorrelated with regressors
re_cpi <- plm(formula_cpi, data = pdata, model = "random")

# Display results
stargazer(pols_cpi, fe_cpi, re_cpi,
          title   = "Table 1: Panel Regression Results, ln(CPI)",
          column.labels = c("Pooled OLS", "Fixed Effects", "Random Effects"),
          dep.var.label = "ln(CPI)",
          covariate.labels = c("Oil Shock (% Δ Brent)", "ln(NEER)",
                                "Policy Rate", "VIX", "Post-Crisis Dummy"),
          type = "text",
          keep.stat = c("n", "rsq", "f"),
          notes = "Robust standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.1")

# ------------------------------------------------------------------------------
# 10. ECONOMETRIC MODELS, IPI (INDUSTRIAL PRODUCTION / GROWTH)
# ------------------------------------------------------------------------------

formula_ipi <- ln_ipi ~ oil_pct_change + ln_neer + policy_rate + vix + post_crisis

# --- Model 2a: Pooled OLS (IPI) ---
pols_ipi <- plm(formula_ipi, data = pdata, model = "pooling")

# --- Model 2b: Fixed Effects (IPI) ---
fe_ipi <- plm(formula_ipi, data = pdata, model = "within")

# --- Model 2c: Random Effects (IPI) ---
re_ipi <- plm(formula_ipi, data = pdata, model = "random")

# Display results
stargazer(pols_ipi, fe_ipi, re_ipi,
          title   = "Table 2: Panel Regression Results, ln(IPI)",
          column.labels = c("Pooled OLS", "Fixed Effects", "Random Effects"),
          dep.var.label = "ln(IPI)",
          covariate.labels = c("Oil Shock (% Δ Brent)", "ln(NEER)",
                                "Policy Rate", "VIX", "Post-Crisis Dummy"),
          type = "text",
          keep.stat = c("n", "rsq", "f"),
          notes = "Robust standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.1")

# ------------------------------------------------------------------------------
# 11. ROBUST STANDARD ERRORS
# ------------------------------------------------------------------------------

cat("\n--- Robust SE (Arellano HC), FE CPI ---\n")
coeftest(fe_cpi, vcov = vcovHC(fe_cpi, method = "arellano", type = "HC3"))

cat("\n--- Robust SE (Arellano HC), FE IPI ---\n")
coeftest(fe_ipi, vcov = vcovHC(fe_ipi, method = "arellano", type = "HC3"))

# Double-clustered SE (time + country), SCC correction
cat("\n--- Double-Clustered SE (SCC), FE CPI ---\n")
coeftest(fe_cpi, vcov = vcovSCC(fe_cpi, type = "HC3", cluster = "group"))

# ------------------------------------------------------------------------------
# 12. DIAGNOSTIC TESTS
# ------------------------------------------------------------------------------

# --- (A) Breusch-Pagan LM Test: Pooled OLS vs Random Effects ---
# H0: No random individual effects (sigma_alpha^2 = 0) → POLS sufficient
# H1: Random effects present → RE preferred over POLS
cat("\n--- Breusch-Pagan LM Test: POLS vs RE (CPI) ---\n")
bp_re_cpi <- plmtest(pols_cpi, effect = "individual", type = "bp")
print(bp_re_cpi)
# Decision: if p < 0.05, reject POLS in favor of RE/FE

cat("\n--- Breusch-Pagan LM Test: POLS vs RE (IPI) ---\n")
bp_re_ipi <- plmtest(pols_ipi, effect = "individual", type = "bp")
print(bp_re_ipi)

# --- (B) Hausman Test: FE vs RE ---
# H0: RE is consistent (no correlation between alpha_i and X_it) → RE preferred
# H1: RE is inconsistent → FE preferred
# The test statistic is: H = (b_FE - b_RE)' * [Var(b_FE) - Var(b_RE)]^{-1} * (b_FE - b_RE)
# Under H0: H ~ chi2(K)  where K = number of time-varying regressors
cat("\n--- Hausman Test: FE vs RE (CPI) ---\n")
hausman_cpi <- phtest(fe_cpi, re_cpi)
print(hausman_cpi)
cat("Decision: p <", round(hausman_cpi$p.value, 4),
    "→", ifelse(hausman_cpi$p.value < 0.05, "FE preferred (reject RE)", "RE acceptable"), "\n")

cat("\n--- Hausman Test: FE vs RE (IPI) ---\n")
hausman_ipi <- phtest(fe_ipi, re_ipi)
print(hausman_ipi)
cat("Decision: p <", round(hausman_ipi$p.value, 4),
    "→", ifelse(hausman_ipi$p.value < 0.05, "FE preferred (reject RE)", "RE acceptable"), "\n")

# --- (C) Breusch-Pagan / Koenker Test for Heteroskedasticity ---
# Tests H0: homoskedastic residuals in POLS
cat("\n--- BP Heteroskedasticity Test (POLS CPI residuals) ---\n")
bp_het_cpi <- bptest(pols_cpi)
print(bp_het_cpi)

cat("\n--- BP Heteroskedasticity Test (POLS IPI residuals) ---\n")
bp_het_ipi <- bptest(pols_ipi)
print(bp_het_ipi)

# --- (D) Wooldridge Test for Serial Correlation in Panel Data ---
# H0: No serial correlation in idiosyncratic errors (AR(1) rho = 0)
cat("\n--- Serial Correlation Test, FE CPI ---\n")
pbgtest(fe_cpi)

cat("\n--- Serial Correlation Test, FE IPI ---\n")
pbgtest(fe_ipi)

# Note: If serial correlation detected, use vcovSCC() for robust SEs

# ------------------------------------------------------------------------------
# 13. F-TEST FOR FIXED EFFECTS (POLS vs FE)
# ------------------------------------------------------------------------------

# H0: All individual fixed effects are zero → POLS is sufficient
cat("\n--- F-test for Fixed Effects: POLS vs FE (CPI) ---\n")
pFtest(fe_cpi, pols_cpi)

cat("\n--- F-test for Fixed Effects: POLS vs FE (IPI) ---\n")
pFtest(fe_ipi, pols_ipi)

# ------------------------------------------------------------------------------
# 14. COMBINED RESULTS TABLE (stargazer)
# ------------------------------------------------------------------------------

# CPI Table (all 3 models with robust SE)
stargazer(pols_cpi, fe_cpi, re_cpi,
          se = list(
            sqrt(diag(vcovHC(pols_cpi, type = "HC3"))),
            sqrt(diag(vcovHC(fe_cpi, method = "arellano", type = "HC3"))),
            sqrt(diag(vcovHC(re_cpi, method = "arellano", type = "HC3")))
          ),
          title   = "Table 1: Impact of Oil Price Shock on ln(CPI), All Models",
          column.labels = c("POLS", "FE", "RE"),
          type    = "text",
          dep.var.label = "ln(Consumer Price Index)",
          covariate.labels = c("Oil Shock (% Δ Brent)", "ln(NEER)",
                                "Policy Rate", "VIX", "Post-Crisis"),
          keep.stat = c("n", "rsq", "adj.rsq"),
          notes = "Heteroskedasticity-robust SE (HC3). *** p<0.01, ** p<0.05, * p<0.1")

# IPI Table
stargazer(pols_ipi, fe_ipi, re_ipi,
          se = list(
            sqrt(diag(vcovHC(pols_ipi, type = "HC3"))),
            sqrt(diag(vcovHC(fe_ipi, method = "arellano", type = "HC3"))),
            sqrt(diag(vcovHC(re_ipi, method = "arellano", type = "HC3")))
          ),
          title   = "Table 2: Impact of Oil Price Shock on ln(IPI), All Models",
          column.labels = c("POLS", "FE", "RE"),
          type    = "text",
          dep.var.label = "ln(Industrial Production Index)",
          covariate.labels = c("Oil Shock (% Δ Brent)", "ln(NEER)",
                                "Policy Rate", "VIX", "Post-Crisis"),
          keep.stat = c("n", "rsq", "adj.rsq"),
          notes = "Heteroskedasticity-robust SE (HC3). *** p<0.01, ** p<0.05, * p<0.1")

# ------------------------------------------------------------------------------
# 15. HETEROGENEITY ANALYSIS (optional extension)
# ------------------------------------------------------------------------------

# Subset: High-inflation emerging markets (Turkey, Poland, S.Africa)
high_infl <- pdata.frame(
  data_processed %>% filter(country %in% c("Turkey","Poland","South Africa")),
  index = c("country","date"))

# Subset: Lower-inflation emerging markets (Thailand, Indonesia, India)
low_infl <- pdata.frame(
  data_processed %>% filter(country %in% c("Thailand","Indonesia","India")),
  index = c("country","date"))

fe_high <- plm(formula_cpi, data = high_infl, model = "within")
fe_low  <- plm(formula_cpi, data = low_infl,  model = "within")

stargazer(fe_high, fe_low,
          title = "Table 3: Heterogeneity, High vs Low Inflation Subgroups",
          column.labels = c("High-Inflation (TUR/POL/ZAF)", "Low-Inflation (THA/IDN/IND)"),
          type = "text", keep.stat = c("n","rsq"))

# ------------------------------------------------------------------------------
# 16. INTERPRETATION GUIDANCE
# ------------------------------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("INTERPRETATION NOTES\n")
cat(strrep("=", 70), "\n")
cat("
COEFFICIENT INTERPRETATION (log-level models):

1. oil_pct_change (β₁):
   For CPI: a 1 percentage-point increase in the Brent oil price is associated
   with a β₁ * 100% change in CPI. This captures the direct pass-through of
   oil cost increases to consumer prices (energy, transport, food chain).
   Expected sign: POSITIVE for CPI, NEGATIVE for IPI (stagflationary effect).

2. ln_neer (β₂):
   A 1% appreciation of the nominal effective exchange rate is associated with
   a β₂% change in CPI (log-log specification → direct elasticity).
   Expected sign: NEGATIVE for CPI (imported goods become cheaper).
   Magnitude reflects degree of import dependence.

3. policy_rate (β₃):
   A 1 percentage-point tightening of monetary policy is associated with a
   β₃ * 100% change in CPI. Represents the monetary transmission channel.
   Expected sign: NEGATIVE (contractionary policy reduces inflation).

4. vix (β₄):
   A 1-unit increase in the VIX is associated with a β₄ * 100% change in CPI.
   Captures global risk aversion and financial conditions spillover.
   Expected sign: POSITIVE for CPI (uncertainty raises risk premia).

5. post_crisis (β₅):
   The average partial effect of being in the post-Gulf Crisis period
   (March 2026 onward) on ln(CPI), holding other variables constant.
   Measures the structural break in price/production dynamics.

MODEL SELECTION:
  - If Hausman p < 0.05: FE is preferred (correlated country effects).
  - If BP-LM p < 0.05 and Hausman p > 0.05: RE preferred over POLS.
  - Since both heteroskedasticity and autocorrelation are detected,
    always report FE results with Arellano/SCC robust standard errors.
")

# End of script
