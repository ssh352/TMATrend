# Template for Strategy Development - SP500 testing

# Library and time zone setup
library(quantstrat)       # Required package for strategy back testing
library(doMC)             # For parrallel optimization
#library(rgl)             # Library to load 3D trade graphs
#library(reshape2)         # Library to load 3D trade graphs
ttz<-Sys.getenv('TZ')     # Time zone to UTC, saving original time zone
Sys.setenv(TZ='UTC')

# Quantstrat general variables
strat        <- "DMA"           # Give the stratgey a name variable
portfolio.st <- "portf"         # Portfolio name
account.st   <- "accnt"         # Account name
initEq       <- 100000          # this parameter is required to get pct equity rebalancing to work
csvDir       <- "/home/rjk/Financial/commodities_data" # Directory containing csv files
xtsDates     <- "2006/"        # Variable for the point in time you want your prices series to line up from

# Strategy specific variables
MAfast  <- seq(20, 100, by = 10)        #fast moving average period
MAslow  <- seq(40, 100, by = 10)        #slow moving average period
atrMult <- seq(2, 10, by = 1)           #atr multiple to use
riskpct <- 0.02

# Strategy Functions

# Custom indicator to generate the threshold multiplier to set an ATR based stop.
atrStopThresh <- function(HLC, n=14, atr_mult=2){
  ATR <- ATR(HLC = HLC, n)
  pctATR <- round((atr_mult*ATR$atr)/Cl(HLC),digits=5)
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

# Transaction Fee Function - Returns a numeric Fee which is a percetage multiple of the tranaction total value 
pctFee <- function(TxnQty,TxnPrice,Symbol){
  feeMult <- 0.005
  fee <- round(-1*(feeMult*abs(TxnPrice)*abs(TxnQty)),0)
  return(fee)
}

#Symbol Setup
# set the instument as a future and get the data from the csv file
# Setup the Environment
currency('USD')                                                                    # set USD as a base currency
symbol <- "CC"   # Universe selection

future(symbol, currency = "USD", multiplier = 1)

getSymbols(Symbols = symbol, verbose = TRUE, warnings = TRUE, 
           src = 'csv', dir= csvDir, extension='csv', header = TRUE, 
           stingsAsFactors = FALSE)

# Subset
# CC <- CC['2006-01-03/2010-12-31']

for (sym in symbol){
  no_dup <- to.daily(get(sym), indexAt='days',drop.time = TRUE) # this is required to remove duplicate data
  assign(sym, no_dup[xtsDates])                                 # Here the data is subsetted to allign it so that rebalancing works
}

# if run previously, run this code from here down
rm.strat(portfolio.st, silent = FALSE)
rm.strat(account.st, silent = FALSE)
delete.paramset(strategy = strat,"DMA_OPT")

# initialize the portfolio, account and orders. Starting equity and assuming data post 1995.
initPortf(portfolio.st, symbols = symbol)
initAcct(account.st, portfolios = portfolio.st, initEq = initEq)
initOrders(portfolio = portfolio.st)

# define the strategy with a position limit to prevent multiple trades in a direction

addPosLimit(portfolio = portfolio.st, symbol, timestamp="2000-01-01", maxpos=100, 
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
                                                                     n = 20, atr_mult=atrMult), label = "atrStopThresh"
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
                        orderqty="all", replace=TRUE, orderset = "ocolong",TxnFees = 'pctFee'
         ),
         type='exit', label='ExitLONG'
)

add.rule(strategy = strat, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE, orderside=NULL , ordertype='market',
                        orderqty="all", replace=TRUE, orderset = "ocoshort",TxnFees = 'pctFee'
         ),
         type='exit', label='ExitSHORT'
)

# c) Stoploss rules using ordersets and ATR based threshold, not enabled by default
add.rule(strategy=strat, name='ruleSignal',
         arguments=list(sigcol='long', sigval=TRUE, orderside=NULL, ordertype='stoplimit', 
                        prefer='High', orderqty="all", replace=FALSE, orderset ="ocolong",
                        tmult=TRUE, threshold=quote("atr.atrStopThresh"),TxnFees = 'pctFee'
         ),
         type='chain', parent = "EnterLONG", label='StopLONG',enabled = FALSE
)

add.rule(strategy=strat, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE, orderside=NULL, ordertype='stoplimit', 
                        prefer='Low', orderqty="all", replace=FALSE, orderset ="ocoshort",
                        tmult=TRUE, threshold=quote("atr.atrStopThresh"),TxnFees = 'pctFee'
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
                      portfolio=portfolio.st, account = account.st, nsamples=0, verbose = TRUE)

# Walk Forward Analysis
results <- walk.forward(strategy.st=strat, paramset.label='DMA_OPT', portfolio.st=portfolio.st,
  account.st=account.st, period='years', k.training=5, k.testing=2, nsamples=0,
  obj.args = list(x = quote(tradeStats.list$Profit.To.Max.Draw)),audit.prefix='wfa',
  anchored=FALSE, verbose=FALSE
)

stats <- out$tradeStats

out <- write.csv(stats,             # write to file
                 file = paste(getwd(),"/DMAopt",as.character(Sys.Date()),".csv", sep=""),
                 quote = FALSE, row.names = TRUE)

# If you've done this on a  previous date
date <- "2017-04-01"
stats <- read.csv(paste(getwd(),"/DMAopt",date,".csv", sep=""))
stats <- stats[,-1]

portfolio_avg <- aggregate(stats[,c(1,2,3,6:33)],list(stats$Portfolio), mean)

# A loop to investigate the parameters via a 3D graph
for (sym in symbol){
  dfName <- paste(sym,"stats", sep = "")
  statSubsetDf <- subset(stats, Symbol == sym)
  assign(dfName, statSubsetDf)
  tradeGraphs(stats = statSubsetDf, 
              free.params=c("ma_fast","ma_slow"),
              statistics = c("Ann.Sharpe","Profit.To.Max.Draw","Min.Equity"), 
              title = sym)
}

# Or use a heatmap to look at one parameter at a time


for (Atr in atrMult){
  dfName <- paste(Atr,"stats", sep = "")
  statSubsetDf <- subset(portfolio_avg, atr == Atr)
  assign(dfName, statSubsetDf)
  z <- tapply(X=statSubsetDf$Net.Trading.PL, 
              INDEX = list(statSubsetDf$ma_fast,statSubsetDf$ma_slow), 
              FUN = median)
  x <- as.numeric(rownames(z))
  y <- as.numeric(colnames(z))
  filled.contour(x=x,y=y,z=z,color=heat.colors,xlab="ma_fast",ylab="ma_slow")
  title(Atr)
}

portfolio_avg <- aggregate(stats[,c(1,2,3,6:33)],list(stats$Portfolio), mean)

Sys.setenv(TZ=ttz)                                             # Return to original time zone
