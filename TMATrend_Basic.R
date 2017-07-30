# New Script to use the source and save strategy function. Will run the dual moving average
# strategy with the getsymbols function from the desired source file, plot the equity curves
# and compare to the benchmark SP500 index.

# Load Quantstrat and required packages
require(quantstrat)

# Set the system time to UTC
Sys.setenv(TZ="UTC")

# Get DMA include ile with parameters and Functions
source(paste(getwd(),"/TMAInclude.R",sep=""))

# Strategy specific variables
MAfast  <- 5   # fast moving average period
MAslow  <- 35   # slow moving average period
MAmed   <- 10   # slow moving average period
atrMult <- 3   # atr multiple for stop loss and order sizing
riskpct <- 0.02 # percentage of equity to risk 
risk    <- 1

# Get all symbols, uncomment what you need
# source(paste(getwd(),"/GetCommData.R",sep=""))
# source(paste(getwd(),"/GetCurrData.R",sep=""))
 source(paste(getwd(),"/GetEqData.R",sep=""))

# Change symbol variable here if needed
# symbol <- ""

# If run previously, run this code from here down
rm.strat(portfolio.st, silent = FALSE)
rm.strat(account.st, silent = FALSE)

# Initialize the portfolio, account and orders. Starting equity and assuming data post 1995.
initPortf(portfolio.st, symbols = symbol)
initAcct(account.st, portfolios = portfolio.st, initEq = initEq)
initOrders(portfolio = portfolio.st)

# Define the strategy with a position limit to prevent multiple trades in a direction
for (sym in symbol){
  addPosLimit(portfolio = portfolio.st, sym, timestamp="2000-01-01", maxpos=100, 
              longlevels = 1, minpos=-100, shortlevels = 1)
}

# Define Strategy
strategy(strat, store = TRUE)

# Add the indicators - A fast moving average (MA), a slow MA, a medium MA and the custom indicator
add.indicator(strategy = strat,name = "SMA",arguments=list(x=quote(Cl(mktdata)[,1]),
                                                           n = MAfast), label = "nFast"
)

add.indicator(strategy = strat,name = "SMA",arguments=list(x=quote(Cl(mktdata)[,1]),
                                                           n = MAslow), label = "nSlow"
)

add.indicator(strategy = strat,name = "SMA",arguments=list(x=quote(Cl(mktdata)[,1]),
                                                           n = MAmed), label = "nMed"
)

add.indicator(strategy = strat,name = "atrStopThresh",arguments=list(HLC=quote(mktdata),
                                                                     n = 20, atr_mult=atrMult), label = "atrStopThresh"
)

# Add the signals - long on the first occurence of fastMA over medMA over slowMA and 
# short on the first occurence of fastMA below medMA below slowMA. Exit long on fastMA 
# below medMA or medMA below slowMA. Exit short on fastMA above MedMA or medMA above slowMA

add.signal(strategy=strat,name='sigFormula', arguments = 
             list(columns=c("SMA.nFast", "SMA.nSlow","SMA.nMed"),formula="(SMA.nFast > SMA.nMed) & (SMA.nMed > SMA.nSlow)",
                  cross = TRUE
             ),
           label='longEntry'
)

add.signal(strategy=strat,name='sigFormula',arguments = 
             list(columns=c("SMA.nFast", "SMA.nSlow","SMA.nMed"),formula="(SMA.nFast < SMA.nMed) & (SMA.nMed < SMA.nSlow)",
                  cross = TRUE
             ),
           label='shortEntry'
)

add.signal(strategy=strat,name='sigFormula', arguments = 
             list(columns=c("SMA.nFast", "SMA.nSlow","SMA.nMed"),formula="(SMA.nFast < SMA.nMed) | (SMA.nMed < SMA.nSlow)",
                  cross = TRUE
             ),
           label='longExit'
)

add.signal(strategy=strat,name='sigFormula',arguments = 
             list(columns=c("SMA.nFast", "SMA.nSlow","SMA.nMed"),formula="(SMA.nFast > SMA.nMed) | (SMA.nMed > SMA.nSlow)",
                  cross = TRUE
             ),
           label='shortExit'
)

# Add the rules
# a) Entry rules - enter on moving average cross, osATRsize is the order function
add.rule(strategy=strat,
         name='ruleSignal',
         arguments=list(sigcol='longEntry', sigval=TRUE, orderside='long', ordertype='market', 
                        orderqty=+100, osFUN='osATRsize', replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)

add.rule(strategy=strat,
         name='ruleSignal',
         arguments=list(sigcol='shortEntry', sigval=TRUE, orderside='short', ordertype='market', 
                        orderqty=-100, osFUN='osATRsize', replace=FALSE
         ),
         type='enter',
         label='EnterSHORT'
)

# b) Exit rules - Close on cross the other way
add.rule(strategy = strat, name='ruleSignal',
         arguments=list(sigcol='longExit' , sigval=TRUE, orderside=NULL, ordertype='market',
                        orderqty="all", replace=TRUE, orderset = "ocolong",TxnFees = 'pctFee'
         ),
         type='exit',
         label='ExitLONG'
)

add.rule(strategy = strat, name='ruleSignal',
         arguments=list(sigcol='shortExit', sigval=TRUE, orderside=NULL , ordertype='market',
                        orderqty="all", replace=TRUE, orderset = "ocoshort",TxnFees = 'pctFee'
         ),
         type='exit',
         label='ExitSHORT'
)

# c) Stoploss rules using ordersets and ATR based threshold, not enabled by default
add.rule(strategy=strat,
         name='ruleSignal',
         arguments=list(sigcol='longEntry', sigval=TRUE, orderside=NULL, ordertype='stoplimit', 
                        prefer='High', orderqty="all", replace=FALSE, orderset ="ocolong",
                        tmult=TRUE, threshold="atr.atrStopThresh",TxnFees = 'pctFee'
         ),
         type='chain', parent = "EnterLONG",
         label='StopLONG',enabled = FALSE
)

add.rule(strategy=strat,
         name='ruleSignal',
         arguments=list(sigcol='shortEntry', sigval=TRUE, orderside=NULL, ordertype='stoplimit', 
                        prefer='Low', orderqty="all", replace=FALSE, orderset ="ocoshort",
                        tmult=TRUE, threshold="atr.atrStopThresh",TxnFees = 'pctFee'
         ),
         type='chain', parent = "EnterSHORT",
         label='StopSHORT',enabled = FALSE
)

save.strategy(strat)

# Enable Stop Rules
enable.rule(strat,type = "chain",label = "StopLONG")
enable.rule(strat,type = "chain",label = "StopSHORT")

# Apply the strategy assigning the output to a variable out
out <- applyStrategy(strategy=strat , portfolios=portfolio.st)

updatePortf(Portfolio = portfolio.st) # Update the portfolio, acct and endeq
updateAcct(name = account.st)
updateEndEq(account.st)

# Plot the charts for each symbol
for (sym in symbol){
  chart.Posn(Portfolio = portfolio.st, Symbol = sym, 
             TA=list("add_SMA(n=100)","add_SMA(n=200)","add_SMA(n=300)"),
             Dates = "1995-01::2017-05") # Chart the position 
}

stats <- tradeStats(portfolio.st)

# Plot the returns vs buy and hold
eq1 <- getAccount(account.st)$summary$End.Eq
rt1 <- Return.calculate(eq1,"log")
getSymbols("^GSPC", from = '1995-01-01')
rt2 <- periodReturn(GSPC, period = "daily")
returns <- cbind(rt1,rt2)
colnames(returns) <- c("TMA","SP500")
chart.CumReturns(returns,colorset=c(2,4),legend.loc="topleft",
                 main="Moving Average Crossover to Benchmark Comparison",ylab="cum return",xlab="",
                 minor.ticks=FALSE)