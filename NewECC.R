rm(list = ls(all.names = TRUE)) # Clear global environment 




install.packages("quantmod")
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(quantmod)
options("getSymbols.warning4.0"=FALSE)
getSymbols(Symbols = c("CBA.AX", "BHP.AX", "WES.AX", "CSL.AX" ), from="2014-01-01", to="2024-12-31",
           auto.assign=TRUE, warnings=FALSE)

#Daily adjusted - for time series and EMH assumptions
cba_D_P = to.daily(CBA.AX$CBA.AX.Adjusted, OHLC=FALSE) 
bhp_D_P = to.daily(BHP.AX$BHP.AX.Adjusted, OHLC=FALSE) 
wes_D_P = to.daily(WES.AX$WES.AX.Adjusted, OHLC=FALSE) 
csl_D_P = to.daily(CSL.AX$CSL.AX.Adjusted, OHLC=FALSE) 

#Merge daily price series
daily_P = merge(cba_D_P,bhp_D_P,wes_D_P,csl_D_P)
colnames(daily_P) <- c("cba","bhp","wes","csl")
head(daily_P)
tail(daily_P)

#Market returns - important for CAPM regression as a benchmark

#Log returns
lr = na.omit(diff(log(daily_P)))
colnames(lr) <- c("cba","bhp","wes","csl")
head(lr)  
tail(lr)

plot(lr)




gert::git_add(".")
gert::git_commit("Data still being constructed")
gert::git_push()

