
# DATA DOWNLOAD --------------------------------------------------------------------

library(kableExtra)
library(tidyverse)
library(readrba)
library(PerformanceAnalytics)
library(quantmod)
library(stargazer)
library(lmtest)
library(sandwich)
library(dplyr)
library(knitr)


#Stocks 
options("getSymbols.warning4.0"=FALSE)
getSymbols(Symbols = c("CBA.AX", "BHP.AX", "WES.AX", "CSL.AX" ), from="2014-01-01", to="2024-12-31",
           auto.assign=TRUE, warnings=FALSE)

# Market benchmark - ASX 200
getSymbols(Symbols = "^AXJO", from = "2014-01-01", to = "2024-12-31",
           auto.assign = TRUE, warnings = FALSE)


#Daily adjusted - for time series and EMH assumptions
cba_D_P = to.daily(CBA.AX$CBA.AX.Adjusted, OHLC=FALSE) 
bhp_D_P = to.daily(BHP.AX$BHP.AX.Adjusted, OHLC=FALSE) 
wes_D_P = to.daily(WES.AX$WES.AX.Adjusted, OHLC=FALSE) 
csl_D_P = to.daily(CSL.AX$CSL.AX.Adjusted, OHLC=FALSE) 
axjo_D_P <- to.daily(AXJO$AXJO.Adjusted, OHLC = FALSE)
colnames(axjo_D_P) <- "ASX200"

#Merge daily price series
daily_P = merge(cba_D_P,bhp_D_P,wes_D_P,csl_D_P)
colnames(daily_P) <- c("cba","bhp","wes","csl")
head(daily_P)
tail(daily_P)



#Log returns
lr = na.omit(diff(log(daily_P)))
colnames(lr) <- c("cba","bhp","wes","csl")
head(lr)  
tail(lr)

market_lr <- na.omit(diff(log(axjo_D_P)))
colnames(market_lr) <- "ASX200"
head(market_lr)
tail(market_lr)

#ASX coded to match dates with stock data 
common_dates   <- intersect(index(lr), index(market_lr))
lr             <- lr[common_dates]
market_lr      <- market_lr[common_dates]

# Momentum and overreaction
reg_data <- data.frame(
  date      = index(lr),
  coredata(lr),
  market_lr = coredata(market_lr)
)

# Clean column names
colnames(reg_data) <- c("date", "cba", "bhp", "wes", "csl", "market_lr")

# Check it looks right
head(reg_data)
nrow(reg_data)

# Create momentum and overreaction variables for all stocks
for (i in seq_along(colnames(lr))) {
  name <- colnames(lr)[i]
  ret  <- reg_data[[name]]
  sd_r <- sd(ret, na.rm = TRUE)
  
  # Momentum: past 20-day cumulative return lagged 1 day
  reg_data[[paste0(name, "_momentum")]] <- lag(
    rollsum(ret, k = 20, fill = NA, align = "right"), 1
  )
  
  # Overreaction: flags returns beyond ±2 standard deviations
  reg_data[[paste0(name, "_large_pos")]] <- lag(
    ifelse(ret >  2 * sd_r, 1, 0), 1
  )
  reg_data[[paste0(name, "_large_neg")]] <- lag(
    ifelse(ret < -2 * sd_r, 1, 0), 1
  )
}

# RBA Variable 
rba_rate <- read_rba(series_id = "FIRMMCRTD")

# Clean it up
rba_rate <- rba_rate %>%
  select(date, value) %>%
  rename(cash_rate = value) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2014-01-01") &
           date <= as.Date("2024-12-31"))

head(rba_rate)
tail(rba_rate)

# Binary 
rba_rate <- rba_rate %>%
  arrange(date) %>%
  mutate(
    rate_change = cash_rate - lag(cash_rate),           # actual size of change
    hike        = ifelse(rate_change > 0, 1, 0),        # 1 = rate hike
    cut         = ifelse(rate_change < 0, 1, 0),        # 1 = rate cut
    change      = ifelse(rate_change != 0, 1, 0)        # 1 = any change
  )

# Check counts of hikes 
cat("Rate hikes:", sum(rba_rate$hike,   na.rm = TRUE), "\n")
cat("Rate cuts:",  sum(rba_rate$cut,    na.rm = TRUE), "\n")
cat("No change:",  sum(rba_rate$change == 0, na.rm = TRUE), "\n")

# Sanity check
cat("Hike dates:\n")
print(rba_rate$date[rba_rate$hike == 1])

cat("\nCut dates:\n")
print(rba_rate$date[rba_rate$cut == 1])

#  Merge into reg_data by date
reg_data <- reg_data %>%
  left_join(rba_rate %>% 
              select(date, rate_change, hike, cut, change),
            by = "date") %>%
  mutate(
    rate_change = replace_na(rate_change, 0),
    hike        = replace_na(hike, 0),
    cut         = replace_na(cut, 0),
    change      = replace_na(change, 0)
  )

# Step 3: Add post announcement days
reg_data <- reg_data %>%
  mutate(
    hike_post1 = lag(hike, 1, default = 0),
    hike_post2 = lag(hike, 2, default = 0),
    cut_post1  = lag(cut,  1, default = 0),
    cut_post2  = lag(cut,  2, default = 0)
  )

# Step 4: Verify it worked
cat("\nIn reg_data:\n")
cat("Hike days:",      sum(reg_data$hike),       "\n")
cat("Cut days:",       sum(reg_data$cut),        "\n")
cat("Post hike days:", sum(reg_data$hike_post1), "\n")
cat("Post cut days:",  sum(reg_data$cut_post1),  "\n")

# Sanity check
# Find which cut date didn't match a trading day
rba_cut_dates <- rba_rate$date[rba_rate$cut == 1]
cat("Cut dates not in reg_data:\n")
print(rba_cut_dates[!rba_cut_dates %in% reg_data$date])


# Risk free rate and excess returns 
# Download 90-day bank bill rate from RBA
rf_data <- read_rba(series_id = "FIRMMBAB90")

# Cleaning rf data 
rf_data <- rf_data %>%
  select(date, value) %>%
  rename(rf_annual = value) %>%
  mutate(
    date     = as.Date(date),
    # Convert annual rate to daily
    rf_daily = rf_annual / 100 / 252
  ) %>%
  filter(date >= as.Date("2014-01-01") &
           date <= as.Date("2024-12-31"))

head(rf_data)

reg_data <- reg_data %>%
  left_join(rf_data %>% select(date, rf_daily),
            by = "date")

# Forward fill any missing days
reg_data <- reg_data %>%
  fill(rf_daily, .direction = "down")

# calculate excess returns with Rf
reg_data$cba_excess    <- reg_data$cba    - reg_data$rf_daily
reg_data$bhp_excess    <- reg_data$bhp    - reg_data$rf_daily
reg_data$wes_excess    <- reg_data$wes    - reg_data$rf_daily
reg_data$csl_excess    <- reg_data$csl    - reg_data$rf_daily
reg_data$market_excess <- reg_data$market_lr - reg_data$rf_daily

# Summary Stats 
summary(lr)
summary(market_lr)
stock_sds <- apply(lr, 2, sd)
print(stock_sds)
sd(market_lr)

# Regression models 
capm_cba <- lm(cba_excess ~ market_excess + hike + cut +
                 hike_post1 + cut_post1 +
                 cba_momentum +
                 cba_large_pos + cba_large_neg,
               data = reg_data)
summary(capm_cba)
nobs(capm_cba)

### BHP ###
capm_bhp <- lm(bhp_excess ~ market_excess + hike + cut +
                 hike_post1 + cut_post1 +
                 bhp_momentum +
                 bhp_large_pos + bhp_large_neg,
               data = reg_data)
summary(capm_bhp)
nobs(capm_bhp)

### WES ###
capm_wes <- lm(wes_excess ~ market_excess + hike + cut +
                 hike_post1 + cut_post1 +
                 wes_momentum +
                 wes_large_pos + wes_large_neg,
               data = reg_data)
summary(capm_wes)

### CSL ###

capm_csl <- lm(csl_excess ~ market_excess + hike + cut +
                 hike_post1 + cut_post1 +
                 csl_momentum +
                 csl_large_pos + csl_large_neg,
               data = reg_data)
summary(capm_csl)


#Newey-west 

# CBA
T_cba   <- nobs(capm_cba)
m_cba   <- floor(0.75 * T_cba^(1/3))
NW_cba  <- NeweyWest(capm_cba, lag = m_cba - 1, prewhite = FALSE, adjust = TRUE)
cat("=== CBA — Newey-West HAC ===\n")
cat("T =", T_cba, "| m =", m_cba, "| lag =", m_cba - 1, "\n")
print(coeftest(capm_cba, vcov = NW_cba))

# BHP
T_bhp   <- nobs(capm_bhp)
m_bhp   <- floor(0.75 * T_bhp^(1/3))
NW_bhp  <- NeweyWest(capm_bhp, lag = m_bhp - 1, prewhite = FALSE, adjust = TRUE)
cat("\n=== BHP — Newey-West HAC ===\n")
cat("T =", T_bhp, "| m =", m_bhp, "| lag =", m_bhp - 1, "\n")
print(coeftest(capm_bhp, vcov = NW_bhp))

# WES
T_wes   <- nobs(capm_wes)
m_wes   <- floor(0.75 * T_wes^(1/3))
NW_wes  <- NeweyWest(capm_wes, lag = m_wes - 1, prewhite = FALSE, adjust = TRUE)
cat("\n=== WES — Newey-West HAC ===\n")
cat("T =", T_wes, "| m =", m_wes, "| lag =", m_wes - 1, "\n")
print(coeftest(capm_wes, vcov = NW_wes))

# CSL
T_csl   <- nobs(capm_csl)
m_csl   <- floor(0.75 * T_csl^(1/3))
NW_csl  <- NeweyWest(capm_csl, lag = m_csl - 1, prewhite = FALSE, adjust = TRUE)
cat("\n=== CSL — Newey-West HAC ===\n")
cat("T =", T_csl, "| m =", m_csl, "| lag =", m_csl - 1, "\n")
print(coeftest(capm_csl, vcov = NW_csl))


# Post-covid subsample 
reg_post <- reg_data %>%
  filter(date >= as.Date("2021-01-01")) %>%
  select(-cut, -cut_post1)

#CBA post subperiod 

# CBA
capm_cba_post <- lm(cba_excess ~ market_excess +
                      hike +
                      hike_post1 +
                      cba_momentum +
                      cba_large_pos +
                      cba_large_neg,
                    data = reg_post)
summary(capm_cba_post)

# BHP
capm_bhp_post <- lm(bhp_excess ~ market_excess +
                      hike +
                      hike_post1 +
                      bhp_momentum +
                      bhp_large_pos +
                      bhp_large_neg,
                    data = reg_post)
summary(capm_bhp_post)

# WES
capm_wes_post <- lm(wes_excess ~ market_excess +
                      hike +
                      hike_post1 +
                      wes_momentum +
                      wes_large_pos +
                      wes_large_neg,
                    data = reg_post)
summary(capm_wes_post)

# CSL
capm_csl_post <- lm(csl_excess ~ market_excess +
                      hike +
                      hike_post1 +
                      csl_momentum +
                      csl_large_pos +
                      csl_large_neg,
                    data = reg_post)
summary(capm_csl_post)



# Magnitude test 
reg_data %>%
  filter(rate_change != 0) %>%
  select(date, rate_change, hike, cut) %>%
  print()

# CBA
capm_cba_mag <- lm(cba_excess ~ market_excess +
                     rate_change +
                     hike_post1 +
                     cut_post1 +
                     cba_momentum +
                     cba_large_pos +
                     cba_large_neg,
                   data = reg_data)
summary(capm_cba_mag)

# BHP
capm_bhp_mag <- lm(bhp_excess ~ market_excess +
                     rate_change +
                     hike_post1 +
                     cut_post1 +
                     bhp_momentum +
                     bhp_large_pos +
                     bhp_large_neg,
                   data = reg_data)
summary(capm_bhp_mag)

# WES
capm_wes_mag <- lm(wes_excess ~ market_excess +
                     rate_change +
                     hike_post1 +
                     cut_post1 +
                     wes_momentum +
                     wes_large_pos +
                     wes_large_neg,
                   data = reg_data)
summary(capm_wes_mag)

# CSL
capm_csl_mag <- lm(csl_excess ~ market_excess +
                     rate_change +
                     hike_post1 +
                     cut_post1 +
                     csl_momentum +
                     csl_large_pos +
                     csl_large_neg,
                   data = reg_data)
summary(capm_csl_mag)










# FIGURES -------------------------------------------------------------------------

#Exclusion table (Figure 1)

raw_rows <- nrow(daily_P)
log_return_rows <- nrow(na.omit(diff(log(daily_P))))
date_match_rows <- length(common_dates)
merged_data_rows <- nrow(reg_data)
final_regression_data <- na.omit(reg_data)
final_rows <- nrow(final_regression_data)
exclusion_table <- data.frame(
  Step = c(
    "1. Raw Yahoo Finance Data",
    "2. Log-Returns (omitted N/A)",
    "3. Matching date with ASX200 Index",
    "4. Merging with RBA and Rf",
    "5. Final Regression Sample (dropped lags/momentum N/A)"
  ),
  Observations = c(
    raw_rows,
    log_return_rows,
    date_match_rows,
    merged_data_rows,
    final_rows
  )
)


exclusion_table$Excluded <- c(0, diff(-exclusion_table$Observations))


print(exclusion_table, row.names = FALSE)


if(!require(knitr)) install.packages("knitr")
library(knitr)
kable(exclusion_table, caption = "Data Clean-Up and Sample Exclusion Table", format = "markdown")

# Summary Statistics (Figure 2)

summary_df <- data.frame(
  Statistic = c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Maximum"),
  CBA       = c(-0.1054275, -0.0059223, 0.0008132, 0.0004463, 0.0075282, 0.1245326),
  BHP       = c(-0.1556533, -0.0094840, 0.0003334, 0.0003484, 0.0107676, 0.1128324),
  WES       = c(-0.1043073, -0.0057067, 0.0009176, 0.0005286, 0.0072665, 0.1069406),
  CSL       = c(-0.1092876, -0.0064749, 0.0006934, 0.0005624, 0.0081168, 0.1176935),
  ASX200    = c(-0.1020303, -0.0043284, 0.0007189, 0.0001506, 0.0051753, 0.0676648)
)

sd_row <- data.frame(
  Statistic = "Std. Deviation",
  CBA       = 0.01309,
  BHP       = 0.01776,
  WES       = 0.01260,
  CSL       = 0.01412,
  ASX200    = 0.00948
)

final_summary_table <- rbind(summary_df, sd_row)


if(!require(knitr)) install.packages("knitr")
library(knitr)

kable(
  final_summary_table, 
  digits = 5, 
  caption = "Summary Statistics of Daily Log Returns with Standard Deviation", 
  format = "markdown"
)




# Regression table (Figure 3)

sig_summary_table <- data.frame(
  Stock    = c("CBA", "CBA", "CBA", 
               "BHP", "BHP", "BHP", 
               "WES", "WES", 
               "CSL", "CSL", "CSL"),
  Variable = c("Market Excess Return (β)", "Post Hike Day 1", "Overreaction (+)",
               "Market Excess Return (β)", "Rate Cut", "Momentum",
               "Alpha (Intercept)", "Market Excess Return (β)",
               "Market Excess Return (β)", "Overreaction (+)", "Overreaction (-)"),
  Estimate = c("1.10940*** (0.01591)", "-0.00435* (0.00219)", "0.00257* (0.00113)",
               "1.21902*** (0.02736)", "-0.00929* (0.00453)", "-0.00851* (0.00358)",
               "0.00046* (0.00019)", "0.89512*** (0.01893)",
               "0.83820*** (0.02398)", "-0.00424** (0.00160)", "0.00590*** (0.00150)"),
  stringsAsFactors = FALSE
)


sig_summary_table <- sig_summary_table %>% arrange(Stock)
row_counts <- table(sig_summary_table$Stock)
row_counts <- row_counts[c("BHP", "CBA", "CSL", "WES")]

sig_summary_table %>%
  select(Variable, Estimate) %>% 
  kable(
    caption   = "Figure 3: Regression results",
    align     = c("l", "c"),
    col.names = c("Significant Factor / Variable Name", "Coefficient Estimate (Std. Error)"),
    format    = "html"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "bordered"),
    full_width        = TRUE,
    font_size         = 11
  ) %>%
  row_spec(0, bold = TRUE, background = "#2c7bb6", color = "white") %>%
  pack_rows(index = row_counts) %>%
  column_spec(1, bold = TRUE, width = "25em") %>%
  add_footnote(
    c("Values display point estimates followed by standard errors in parentheses.",
      "R-squared: CBA (0.6431), WES (0.4523), BHP (0.4245), CSL (0.3203)", 
      "*** p<0.001, ** p<0.01, * p<0.05 based on standard two-tailed OLS t-tests."),
    notation = "none"
  )





# Newey-west table (Figure 4)
library(dplyr)
library(knitr)
library(kableExtra)

comparison_data <- data.frame(
  Stock    = c("CBA", "CBA", "CBA", 
               "BHP", "BHP", "BHP", 
               "WES", "WES", 
               "CSL", "CSL", "CSL"),
  Variable = c("Market Excess Return (β)", "Post Hike Day 1", "Overreaction (+)",
               "Market Excess Return (β)", "Rate Cut", "Momentum",
               "Alpha (Intercept)", "Market Excess Return (β)",
               "Market Excess Return (β)", "Overreaction (+)", "Overreaction (-)"),
  OLS_Est  = c("1.10940*** (0.01591)", "-0.00435* (0.00219)", "0.00257* (0.00113)",
               "1.21902*** (0.02736)", "-0.00929* (0.00453)", "-0.00851* (0.00358)",
               "0.00046* (0.00019)", "0.89512*** (0.01893)",
               "0.83820*** (0.02398)", "-0.00424** (0.00160)", "0.00590*** (0.00150)"),
  HAC_Est  = c("1.10940*** (0.02971)", "-0.00435 (0.00291)", "0.00257 (0.00184)",
               "1.21902*** (0.06226)", "-0.00929 (0.00930)", "-0.00851 (0.00611)",
               "0.00046* (0.00018)", "0.89512*** (0.03281)",
               "0.83820*** (0.03762)", "-0.00424 (0.00324)", "0.00590 (0.00392)"),
  stringsAsFactors = FALSE
)


comparison_data <- comparison_data %>% arrange(Stock)


row_counts <- table(comparison_data$Stock)
row_counts <- row_counts[c("BHP", "CBA", "CSL", "WES")]

comparison_data %>%
  select(Variable, OLS_Est, HAC_Est) %>% 
  kable(
    caption   = "Figure 4: HAC VS Baseline Model",
    align     = c("l", "c", "c"),
    col.names = c("Factor / Variable Name", "Original OLS Model", "Newey-West HAC Robust Model"),
    format    = "html"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "bordered"),
    full_width        = TRUE,
    font_size         = 11
  ) %>%
  row_spec(0, bold = TRUE, background = "#2c7bb6", color = "white") %>%
  # Slice into stock specific sections
  pack_rows(index = row_counts) %>%
  column_spec(1, bold = TRUE, width = "22em") %>%
  column_spec(2, width = "16em") %>%
  column_spec(3, width = "16em") %>%
  add_footnote(
    c("Values display point estimates followed by standard errors in parentheses.",
      "R-squared: CBA (0.6431), WES (0.4523), BHP (0.4245), CSL (0.3203)",
      "*** p<0.001, ** p<0.01, * p<0.05"),
    notation = "none"
  )


# Post-covid table (Figure 5)

post_covid_comparison <- data.frame(
  Stock    = c("BHP", "BHP", "BHP", "BHP",
               "CBA", "CBA", "CBA", "CBA", 
               "CSL", "CSL", "CSL",
               "WES", "WES"),
  Variable = c("Market Excess Return (β)", "Rate Cut", "Momentum", "Overreaction (-)",
               "Alpha (Intercept)", "Market Excess Return (β)", "Post Hike Day 1", "Overreaction (+)",
               "Market Excess Return (β)", "Overreaction (+)", "Overreaction (-)",
               "Alpha (Intercept)", "Market Excess Return (β)"),
  Baseline_Est = c("1.21902*** (0.02736)", "-0.00929* (0.00453)", "-0.00851* (0.00358)", "-0.00043 (0.00255)",
                   "0.00025 (0.00016)", "1.10940*** (0.01591)", "-0.00435* (0.00219)", "0.00257* (0.00113)",
                   "0.83820*** (0.02398)", "-0.00424** (0.00160)", "0.00590*** (0.00150)",
                   "0.00046* (0.00019)", "0.89512*** (0.01893)"),
  Post_Est     = c("1.20264*** (0.05242)", "0.00000 (0.00000)", "-0.00065 (0.00616)", "-0.00839** (0.00309)",
                   "0.00079** (0.00029)", "1.03818*** (0.03385)", "-0.00490* (0.00240)", "0.00312 (0.00288)",
                   "0.72455*** (0.04396)", "0.00223 (0.00355)", "-0.00313 (0.00283)",
                   "0.00033 (0.00032)", "0.97839*** (0.03837)"),
  stringsAsFactors = FALSE
)


post_covid_comparison <- post_covid_comparison %>% arrange(Stock)


row_counts <- table(post_covid_comparison$Stock)
row_counts <- row_counts[c("BHP", "CBA", "CSL", "WES")]
post_covid_comparison %>%
  select(Variable, Baseline_Est, Post_Est) %>% 
  kable(
    caption   = "Figure 5: Subperiod Results VS Baseline Model",
    align     = c("l", "c", "c"),
    col.names = c("Factor / Variable Name", "Full Baseline Model", "Post-COVID Subperiod Model"),
    format    = "html"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "bordered"),
    full_width        = TRUE,
    font_size         = 11
  ) %>%
  row_spec(0, bold = TRUE, background = "#2c7bb6", color = "white") %>%
  # Slice data cleanly into specific stock sections
  pack_rows(index = row_counts) %>%
  column_spec(1, bold = TRUE, width = "22em") %>%
  column_spec(2, width = "16em") %>%
  column_spec(3, width = "16em") %>%
  add_footnote(
    c("Values display point estimates followed by standard errors in parentheses.",
      "R-squared: CBA (0.4868), WES (0.3923), BHP (0.3476), CSL (0.2090)", 
      "*** p<0.001, ** p<0.01, * p<0.05 based on two-tailed tests."),
    notation = "none"
  )








# Alternate specification (Figure 6)
magnitude_comparison <- data.frame(
  Stock    = c("BHP", "BHP", "BHP",
               "CBA", "CBA", "CBA",
               "CSL", "CSL", "CSL",
               "WES", "WES"),
  Variable = c("Market Excess Return (β)", "Rate Cut", "Momentum",
               "Market Excess Return (β)", "Post Hike Day 1", "Overreaction (+)",
               "Market Excess Return (β)", "Overreaction (+)", "Overreaction (-)",
               "Alpha (Intercept)", "Market Excess Return (β)"),
  Binary_Est = c("1.21902*** (0.02736)", "-0.00929* (0.00453)", "-0.00851* (0.00358)",
                 "1.10940*** (0.01591)", "-0.00435* (0.00219)", "0.00257* (0.00113)",
                 "0.83820*** (0.02398)", "-0.00424** (0.00160)", "0.00590*** (0.00150)",
                 "0.00046* (0.00019)", "0.89512*** (0.01893)"),
  Magnitude_Est = c("1.22073*** (0.02736)", "0.00147 (0.00940)", "-0.00868* (0.00358)",
                    "1.11006*** (0.01590)", "-0.00433* (0.00219)", "0.00249* (0.00113)",
                    "0.83663*** (0.02398)", "-0.00413** (0.00160)", "0.00587*** (0.00151)",
                    "0.00048* (0.00019)", "0.89463*** (0.01891)"),
  stringsAsFactors = FALSE
)


magnitude_comparison <- magnitude_comparison %>% arrange(Stock)


row_counts <- table(magnitude_comparison$Stock)[c("BHP", "CBA", "CSL", "WES")]

magnitude_comparison %>%
  select(Variable, Binary_Est, Magnitude_Est) %>% 
  kable(
    caption   = "Figure 6: Magnitude Specification VS Baseline Model",
    align     = c("l", "c", "c"),
    col.names = c("Factor / Variable Name", "Baseline Model", "Rate Change Magnitude Specification"),
    format    = "html"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "bordered"),
    full_width        = TRUE,
    font_size         = 11
  ) %>%
  row_spec(0, bold = TRUE, background = "#2c7bb6", color = "white") %>%

  pack_rows(index = row_counts) %>%
  column_spec(1, bold = TRUE, width = "22em") %>%
  column_spec(2, width = "16em") %>%
  column_spec(3, width = "16em") %>%
  add_footnote(
    c("Values display point estimates followed by standard errors in parentheses.",
      "R-squared: CBA (0.6431), WES (0.4524), BHP (0.4238), CSL (0.3195)", 
      "*** p<0.001, ** p<0.01, * p<0.05"),
    notation = "none"
  )
