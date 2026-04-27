rm(list = ls(all.names = TRUE)) # Clear global environment 


#Packages
install.packages("quantmod")
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(quantmod)

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

#Final check before EDA 
cat("Stock return rows:", nrow(lr), "\n")
cat("Market return rows:", nrow(market_lr), "\n") #Rows are equal




#EDA 
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

# JB value is perfect - it follows normal distribution. Can use regression. 

# ACF - Under EMF there should be white noise 
par(mfrow = c(2, 2))

for (i in seq_along(stocks)) {
  acf(as.numeric(lr[, stocks[i]]),
      main    = paste(stock_names[i], "— ACF of Log Returns"),
      xlab    = "Lag (Days)",
      ylab    = "Autocorrelation",
      lag.max = 20,
      col     = colours[i])
}

par(mfrow = c(1, 1))




# Sharpe Ratio - what does this tell us?



gert::git_add(".")
gert::git_commit("Beginning EDA")
gert::git_push()