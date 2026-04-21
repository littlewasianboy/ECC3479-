rm(list = ls(all.names = TRUE)) # Clear global environment 

#Push command 
gert::git_add(".")
gert::git_commit("Data downloaded")
gert::git_push()


install.packages("quantmod")
library(quantmod)
options("getSymbols.warning4.0"=FALSE)
getSymbols(Symbols = c("CBA.AX", "BHP.AX", "WES.AX", "CSL.AX" ), from="2014-01-01", to="2024-12-31",
           auto.assign=TRUE, warnings=FALSE)

#Daily adjusted - for time series and EMH assumptions
cba_D_P = to.daily(CBA.AX$CBA.AX.Adjusted, OHLC=FALSE) 
bhp_D_P = to.daily(BHP.AX$BHP.AX.Adjusted, OHLC=FALSE) 
wes_D_P = to.daily(WES.AX$WES.AX.Adjusted, OHLC=FALSE) 
csl_D_P = to.daily(CSL.AX$CSL.AX.Adjusted, OHLC=FALSE) 

