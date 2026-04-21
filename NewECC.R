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




gert::git_add(".")
gert::git_commit("Beginning EDA")
gert::git_push()
