# This strategy uses a simple moving average crossover (MAfast and MASlow), to either a) go long if the fast
# moving avergae is above the slow moving average or b) short if the fast moving average is below the slow 
# moving average. Has a rebalancing rule that enables us to compare being 100% invested in this strategy to 
# buy and hold. No leverage. Here the simple percentage multiple stop loss is replace with an ATR based stop
# loss, which is implemented via a custom indicator. Single equity data from yahoo. ATR variable stop loss
# Optimization of all 3 strategy variables with paramsets is working. parallel proc working in linux with doMC

# Library and time zone setup
library(quantstrat)       # Required package for strategy back testing
library(doMC)             # For parrallel optimization
#library(rgl)              # Library to load 3D trade graphs
#library(reshape2)         # Library to load 3D trade graphs
ttz<-Sys.getenv('TZ')     # Time zone to UTC, saving original time zone
Sys.setenv(TZ='UTC')

# Quantstrat general variables
strat        <- "DMA"               # Give the stratgey a name variable
portfolio.st <- "portf"             # Portfolio name
account.st   <- "accnt"             # Account name
initEq       <- 1000000             # this parameter is required to get pct equity rebalancing to work

# Strategy specific variables
MAfast  <- seq(20, 200, by = 20)        # fast moving average period
MAslow  <- seq(20, 400, by = 20)        # slow moving average period
atrMult <- seq(1, 5, by = 1)            # atr multiple to use
riskpct <- 0.02                         # amount of account to risk   

# Strategy Functions
# Custom indicator to generate the threshold multiplier to set an ATR based stop.
atrStopThresh <- function(HLC, n=14, atr_mult=2){
  ATR <- ATR(HLC = HLC, n)
  pctATR <- (atr_mult*ATR$atr)/Cl(HLC)
  pctATR
}

# Function to size order according to account balance, percentage risk desired and volatility (ATR)
# use a built in order size function instead to not utilize this functionality
osATRsize <- function(data = mktdata, timestamp=timestamp, orderqty = orderqty, acct = account.st, portfolio = portfolio, ...){
  # First set a multiplier to get order in the correct sign for short or long
  if(orderqty<0){
    sign <- -1
  }else{
    sign <- 1
  }
  # get account equity
  updatePortf(Portfolio = portfolio)
  updateAcct(name = acct)
  updateEndEq(Account = acct)
  account_eq <- getEndEq(Account = acct,Date = timestamp)
  
  # determine volatility adjusted position sizing 
  orderqty <- (account_eq * riskpct)/((data[timestamp]$atr.atrStopThresh)*(Cl(data[timestamp])))
  
  # round down, get as a number of correct sign and return
  orderqty <- (as.numeric(floor(orderqty)))*sign
  orderqty
}

# Symbols etc
currency('USD')             # set USD as a base currency
symbol <- "GSPC"            # Universe selection At this stage is only one symbol

# set the instument as a future and get the data from yahoo
stock(symbol, currency = "USD", multiplier = 1)
getSymbols("^GSPC", from = '1995-01-01')

# if run previously, run this code from here down
rm.strat(portfolio.st, silent = FALSE)
rm.strat(account.st, silent = FALSE)
delete.paramset(strategy = strat, "DMA_OPT")

# initialize the portfolio, account and orders. Starting equity and assuming data post 1995.
initPortf(portfolio.st, symbols = symbol, initDate = "1995-01-01")
initAcct(account.st, portfolios = portfolio.st, initEq = initEq, initDate = "1995-01-01")
initOrders(portfolio = portfolio.st, initDate = "1995-01-01")

# Add a position limit for the portfolio to prevent multiple trades in a direction
addPosLimit(portfolio = portfolio.st, symbol, timestamp="1995-01-01", maxpos=100, 
            longlevels = 1, minpos=-100, shortlevels = 1)

# Define Strategy
strategy(strat, store = TRUE)

# Add the indicators - A fast moving average, a slow moving average and the custom indicator
add.indicator(strategy = strat,name = "SMA",arguments=list(x=quote(Cl(mktdata)[,1]),
                                                           n = MAfast), label = "nFast"
)

add.indicator(strategy = strat,name = "SMA",arguments=list(x=quote(Cl(mktdata)[,1]),
                                                           n = MAslow), label = "nSlow"
)

add.indicator(strategy = strat,name = "atrStopThresh",arguments=list(HLC=quote(mktdata),
                                                                     n = 14, atr_mult=atrMult), label = "atrStopThresh"
)

# Add the signals - long on a cross of fast MA over slow MA and short on a cross of fast MA below slow MA.
add.signal(strategy=strat,name='sigCrossover', arguments = list(columns=c("nFast", "nSlow"),relationship="gt"
             ),
           label='long'
)

add.signal(strategy=strat,name='sigCrossover',arguments = list(columns=c("nFast", "nSlow"),relationship="lte"
             ),
           label='short'
)

# Add the rules
# a) Entry rules - enter on moving average cross, osMaxPos is the order function
add.rule(strategy=strat, name='ruleSignal',
         arguments=list(sigcol='long', sigval=TRUE, orderside='long', ordertype='market', 
                        orderqty=+100, osFUN='osATRsize', replace=FALSE
         ),
         type='enter', label='EnterLONG'
)

add.rule(strategy=strat, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE, orderside='short', ordertype='market', 
                        orderqty=-100, osFUN='osATRsize', replace=FALSE
         ),
         type='enter', label='EnterSHORT'
)

# b) Exit rules - Close on cross the other way
add.rule(strategy = strat, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE, orderside=NULL, ordertype='market',
                        orderqty="all", replace=TRUE, orderset = "ocolong"
         ),
         type='exit', label='ExitLONG'
)

add.rule(strategy = strat, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE, orderside=NULL , ordertype='market',
                        orderqty="all", replace=TRUE, orderset = "ocoshort"
         ),
         type='exit', label='ExitSHORT'
)

# c) Stoploss rules using ordersets and ATR based threshold, not enabled by default
add.rule(strategy=strat, name='ruleSignal',
         arguments=list(sigcol='long', sigval=TRUE, orderside=NULL, ordertype='stoplimit', 
                        prefer='High', orderqty="all", replace=FALSE, orderset ="ocolong",
                        tmult=TRUE, threshold=quote("atr.atrStopThresh")
         ),
         type='chain', parent = "EnterLONG", label='StopLONG',enabled = FALSE
)

add.rule(strategy=strat, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE, orderside=NULL, ordertype='stoplimit', 
                        prefer='Low', orderqty="all", replace=FALSE, orderset ="ocoshort",
                        tmult=TRUE, threshold=quote("atr.atrStopThresh")
         ),
         type='chain', parent = "EnterSHORT", label='StopSHORT',enabled = FALSE
)

# Add distributions and constraints
add.distribution(strategy = strat,
                 paramset.label = "DMA_OPT",
                 component.type = "indicator",
                 component.label = "nFast",
                 variable = list( n = MAfast ),
                 label = "ma_fast"
)

add.distribution(strategy = strat,
                 paramset.label = "DMA_OPT",
                 component.type = "indicator",
                 component.label = "nSlow",
                 variable = list( n = MAslow ),
                 label = "ma_slow"
)

add.distribution(strategy = strat,
                 paramset.label = "DMA_OPT",
                 component.type = "indicator",
                 component.label = "atrStopThresh",
                 variable = list( atr_mult=atrMult ),
                 label = "atr"
)

add.distribution.constraint(strategy = strat,
                            paramset.label = "DMA_OPT",
                            distribution.label.1 = "ma_fast",
                            distribution.label.2 = "ma_slow",
                            operator = "<",
                            label = "fastLTslow")

# Enable Rules
enable.rule(strat,type = "chain",label = "StopSHORT")
enable.rule(strat,type = "chain",label = "StopLONG")

# Register the cores for parralel procssing
registerDoMC(cores=detectCores())

# Now apply the parameter sets for optimization
out <- apply.paramset(strat, paramset.label = "DMA_OPT",
                      portfolio=portfolio.st, account = account.st, nsamples=0, verbose = TRUE, audit = globalenv())
stats <- out$tradeStats

# A loop to investigate the parameters via a 3D graph
for (a in atrMult){
  dfName <- paste(a,"stats", sep = "")
  statSubsetDf <- subset(stats, atr == a)
  assign(dfName, statSubsetDf)
  tradeGraphs(stats = statSubsetDf, 
              free.params=c("ma_fast","ma_slow"),
              statistics = c("Ann.Sharpe","Profit.To.Max.Draw"), 
              title = a)
}

# Or use a heatmap to look at one parameter at a time
for (a in atrMult){
  dfName <- paste(a,"stats", sep = "")
  statSubsetDf <- subset(stats, atr == a)
  assign(dfName, statSubsetDf)
  z <- tapply(X=statSubsetDf$Net.Trading.PL, 
              INDEX = list(statSubsetDf$ma_fast,statSubsetDf$ma_slow), 
              FUN = median)
  x <- as.numeric(rownames(z))
  y <- as.numeric(colnames(z))
  filled.contour(x=x,y=y,z=z,color=heat.colors,xlab="ma_fast",ylab="ma_slow")
  title(a)
}

Sys.setenv(TZ=ttz)                                             # Return to original time zone
