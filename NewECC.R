rm(list = ls(all.names = TRUE)) # Clear global environment 

#Packages
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("readrba")
install.packages("tidyverse")
install.packages("stargazer")
install.packages("kableExtra")
install.packages("lmtest")
install.packages("sandwich")
library(kableExtra)
library(tidyverse)
library(readrba)
library(PerformanceAnalytics)
library(quantmod)
library(stargazer)
library(lmtest)
library(sandwich)

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
# Emergency RBA meeting date 

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



#Final check before EDA 
cat("Stock return rows:", nrow(lr), "\n")
cat("Market return rows:", nrow(market_lr), "\n") #Rows are equal


#### EDA ####
plot(lr, main = "Log stock returns")
plot(market_lr, main = "Log ASX return")

summary(daily_P)
summary(axjo_D_P)
summary(lr)
summary(market_lr)

# Plotted returns
par(mfrow = c(2, 2))

plot(lr[, "cba"],
     main = "CBA Daily Log Returns",
     xlab = "Date", ylab = "Log Return",
     col  = "#2c7bb6")

plot(lr[, "bhp"],
     main = "BHP Daily Log Returns",
     xlab = "Date", ylab = "Log Return",
     col  = "#d7191c")

plot(lr[, "wes"],
     main = "WES Daily Log Returns",
     xlab = "Date", ylab = "Log Return",
     col  = "#1a9641")

plot(lr[, "csl"],
     main = "CSL Daily Log Returns",
     xlab = "Date", ylab = "Log Return",
     col  = "#fdae61")

par(mfrow = c(1, 1))

#Market Returns 
plot(market_lr,
     main = "ASX 200 Daily Log Returns (2014–2024)",
     xlab = "Date",
     ylab = "Log Return",
     col  = "#636363")

## Everything looks normal, volatility around 2020 period. Some have more than others. 

#JB test for normality 
install.packages("tseries")
library(tseries)
for (i in seq_along(colnames(lr))) {
  ret  <- as.numeric(lr[, i])
  test <- jarque.bera.test(ret)
  cat(colnames(lr)[i], 
      "— JB Statistic:", round(test$statistic, 2),
      "| p-value:", format(test$p.value, scientific = TRUE, digits = 3), "\n")
}

# JB value is expected

# ACF - Under EMF there should be white noise 
# CBA
acf(as.numeric(lr[, "cba"]),
    main    = "CBA — ACF of Log Returns",
    xlab    = "Lag (Days)",
    ylab    = "Autocorrelation",
    lag.max = 20,
    col     = "#2c7bb6")

# BHP
acf(as.numeric(lr[, "bhp"]),
    main    = "BHP — ACF of Log Returns",
    xlab    = "Lag (Days)",
    ylab    = "Autocorrelation",
    lag.max = 20,
    col     = "#d7191c")

# WES
acf(as.numeric(lr[, "wes"]),
    main    = "WES — ACF of Log Returns",
    xlab    = "Lag (Days)",
    ylab    = "Autocorrelation",
    lag.max = 20,
    col     = "#1a9641")

# CSL
acf(as.numeric(lr[, "csl"]),
    main    = "CSL — ACF of Log Returns",
    xlab    = "Lag (Days)",
    ylab    = "Autocorrelation",
    lag.max = 20,
    col     = "#fdae61")

par(mfrow = c(1, 1))


# Scatter plots of each stock vs ASX 200
par(mfrow = c(2, 2))

for (i in seq_along(colnames(lr))) {
  plot(as.numeric(market_lr), as.numeric(lr[, i]),
       main = paste(colnames(lr)[i], "vs ASX 200"),
       xlab = "ASX 200 Log Return",
       ylab = paste(colnames(lr)[i], "Log Return"),
       col  = adjustcolor("steelblue", alpha.f = 0.3),
       pch  = 16, cex = 0.5)
  abline(lm(as.numeric(lr[, i]) ~ as.numeric(market_lr)),
         col = "red", lwd = 2)
}

par(mfrow = c(1, 1))


#Correlation matrix 
library(corrplot)
cor_matrix <- cor(lr)

# Print the matrix
cat("=== CORRELATION MATRIX ===\n")
print(round(cor_matrix, 3))

# Visual heatmap
corrplot(cor_matrix,
         method      = "color",
         type        = "upper",
         tl.col      = "black",
         addCoef.col = "black",
         number.cex  = 0.9,
         title       = "ASX Stock Return Correlations (2014–2024)",
         mar         = c(0, 0, 2, 0))



# ADF on log returns — expect to reject unit root (p < 0.05)
for (i in seq_along(colnames(lr))) {
  test <- adf.test(as.numeric(lr[, i]))
  cat(colnames(lr)[i],
      "— ADF p-value:", round(test$p.value, 4), "\n")
  print(test)
}

# Summary of momentum variable
cat("=== MOMENTUM VARIABLE SUMMARY ===\n")
for (i in seq_along(colnames(lr))) {
  name <- colnames(lr)[i]
  mom  <- reg_data[[paste0(name, "_momentum")]]
  cat(name, "— Mean:", round(mean(mom, na.rm = TRUE), 6),
      "| SD:", round(sd(mom, na.rm = TRUE), 6), "\n")
}

# How many overreaction days per stock?
cat("\n=== OVERREACTION DAYS ===\n")
for (i in seq_along(colnames(lr))) {
  name  <- colnames(lr)[i]
  n_pos <- sum(reg_data[[paste0(name, "_large_pos")]], na.rm = TRUE)
  n_neg <- sum(reg_data[[paste0(name, "_large_neg")]], na.rm = TRUE)
  cat(name, "— Large positive days:", n_pos,
      "| Large negative days:", n_neg, "\n")
}

# Momentum and overreaction 
par(mfrow = c(2, 2))
for (i in seq_along(colnames(lr))) {
  name <- colnames(lr)[i]
  plot(reg_data$date, reg_data[[paste0(name, "_momentum")]],
       type = "l",
       main = paste(name, "— 20-Day Momentum"),
       xlab = "Date",
       ylab = "Cumulative 20-Day Return",
       col  = "#2c7bb6")
  abline(h = 0, col = "red", lty = 2)
}
par(mfrow = c(1, 1))

# Does momentum correlate with next day returns?
cat("\n=== MOMENTUM-RETURN CORRELATION ===\n")
for (i in seq_along(colnames(lr))) {
  name <- colnames(lr)[i]
  cor_val <- cor(reg_data[[name]],
                 reg_data[[paste0(name, "_momentum")]],
                 use = "complete.obs")
  cat(name, "— Correlation:", round(cor_val, 4), "\n")
}

cat("\n=== OVERREACTION DAYS ===\n")
for (i in seq_along(colnames(lr))) {
  name  <- colnames(lr)[i]
  n_pos <- sum(reg_data[[paste0(name, "_large_pos")]], na.rm = TRUE)
  n_neg <- sum(reg_data[[paste0(name, "_large_neg")]], na.rm = TRUE)
  cat(name, "— Large positive days:", n_pos,
      "| Large negative days:", n_neg, "\n")
}


## Regression models ##

#CAPM 
capm_cba <- lm(cba_excess ~ market_excess + hike + cut +
                 hike_post1 + cut_post1 +
                 cba_momentum +
                 cba_large_pos + cba_large_neg,
               data = reg_data)
summary(capm_cba)

### BHP ###
capm_bhp <- lm(bhp_excess ~ market_excess + hike + cut +
                 hike_post1 + cut_post1 +
                 bhp_momentum +
                 bhp_large_pos + bhp_large_neg,
               data = reg_data)
summary(capm_bhp)

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

#Robustness 

# Confirming if autocorrelation and heteroskedasticity exists

bgtest(capm_cba, order = 2, type = "Chisq") # REJECT, CORRELATION EXISTS
bgtest(capm_bhp, order = 2, type = "Chisq") # REJECT, CORRELATION EXISTS
bgtest(capm_wes, order = 2, type = "Chisq")
bgtest(capm_csl, order = 2, type = "Chisq")

# Auxiliary regression 

# Create a clean version of reg_data matching model observations
reg_data_clean <- na.omit(reg_data)

# CBA
res_cba   <- residuals(capm_cba)
white_cba <- lm(I(res_cba^2) ~ market_excess + hike + cut +
                  hike_post1 + cut_post1 +
                  cba_momentum + cba_large_pos +
                  cba_large_neg +
                  I(market_excess^2) + I(cba_momentum^2) +
                  I(cba_large_pos^2) + I(cba_large_neg^2) +
                  market_excess:hike + market_excess:cut +
                  market_excess:cba_momentum +
                  market_excess:cba_large_pos +
                  market_excess:cba_large_neg,
                data = reg_data_clean)
summary(white_cba)

# BHP
res_bhp   <- residuals(capm_bhp)
white_bhp <- lm(I(res_bhp^2) ~ market_excess + hike + cut +
                  hike_post1 + cut_post1 +
                  bhp_momentum + bhp_large_pos +
                  bhp_large_neg +
                  I(market_excess^2) + I(bhp_momentum^2) +
                  I(bhp_large_pos^2) + I(bhp_large_neg^2) +
                  market_excess:hike + market_excess:cut +
                  market_excess:bhp_momentum +
                  market_excess:bhp_large_pos +
                  market_excess:bhp_large_neg,
                data = reg_data_clean)
summary(white_bhp)

# WES
res_wes   <- residuals(capm_wes)
white_wes <- lm(I(res_wes^2) ~ market_excess + hike + cut +
                  hike_post1 + cut_post1 +
                  wes_momentum + wes_large_pos +
                  wes_large_neg +
                  I(market_excess^2) + I(wes_momentum^2) +
                  I(wes_large_pos^2) + I(wes_large_neg^2) +
                  market_excess:hike + market_excess:cut +
                  market_excess:wes_momentum +
                  market_excess:wes_large_pos +
                  market_excess:wes_large_neg,
                data = reg_data_clean)
summary(white_wes)

# CSL
res_csl   <- residuals(capm_csl)
white_csl <- lm(I(res_csl^2) ~ market_excess + hike + cut +
                  hike_post1 + cut_post1 +
                  csl_momentum + csl_large_pos +
                  csl_large_neg +
                  I(market_excess^2) + I(csl_momentum^2) +
                  I(csl_large_pos^2) + I(csl_large_neg^2) +
                  market_excess:hike + market_excess:cut +
                  market_excess:csl_momentum +
                  market_excess:csl_large_pos +
                  market_excess:csl_large_neg,
                data = reg_data_clean)
summary(white_csl)

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

# Different period CAPM

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

gert::git_add(".")
gert::git_commit("Beginning EDA")
gert::git_push()
