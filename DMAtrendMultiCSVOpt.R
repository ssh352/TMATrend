# Template for Strategy Development - SP500 testing

# Library and time zone setup
library(quantstrat)       # Required package for strategy back testing
library(doMC)       # For parrallel optimization
#library(rgl)              # Library to load 3D trade graphs
library(reshape2)         # Library to load 3D trade graphs
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
MAfast  <- seq(20, 200, by = 20)        #fast moving average period
MAslow  <- seq(20, 400, by = 20)        #slow moving average period
atrMult <- seq(1, 5, by = 1)            #atr multiple to use

# Strategy Functions
# A function to set the risk for rebalancing based on the number of symbols. Overly long use length instead
setRisk <- function(symlist){
  n = 0
  for (sym in symlist){
    n = n+1
  }
  risk <- round(1/n, digits = 2)
  return(risk)
}

# Custom indicator to generate the threshold multiplier to set an ATR based stop.
atrStopThresh <- function(HLC, n=14, atr_mult=2){
  ATR <- ATR(HLC = HLC, n)
  pctATR <- round((atr_mult*ATR$atr)/Cl(HLC),digits=5)
  pctATR
}

#Symbol Setup
# set the instument as a future and get the data from the csv file
# Setup the Environment
currency('USD')                                                        # set USD as a base currency
symbol <- c("LSU","RR","CO","NG","OJ","LB","HG","LC","CT","CC","KC")   # Universe selection
risk <- setRisk(symbol)                                                # set the risk for rebalancing using the function

for (sym in symbol){
  future(sym, currency = "USD", multiplier = 1)
}

getSymbols(Symbols = symbol, verbose = TRUE, warnings = TRUE, 
           src = 'csv', dir= csvDir, extension='csv', header = TRUE, 
           stingsAsFactors = FALSE)

for (sym in symbol){
  no_dup <- to.daily(get(sym), indexAt='days',drop.time = TRUE) # this is required to remove duplicate data
  assign(sym, no_dup[xtsDates])                                 # Here the data is subsetted to allign it so that rebalancing works
}

# if run previously, run this code from here down
rm.strat(portfolio.st, silent = FALSE)
rm.strat(account.st, silent = FALSE)
delete.paramset(strategy = strat,"DMA_OPT")

# initialize the portfolio, account and orders. Starting equity and assuming data post 1995.
initPortf(portfolio.st, symbols = symbol, initDate = "1995-01-01")
initAcct(account.st, portfolios = portfolio.st, initEq = initEq, initDate = "1995-01-01")
initOrders(portfolio = portfolio.st, initDate = "1995-01-01")

# define the strategy with a position limit to prevent multiple trades in a direction
for (sym in symbol){
  addPosLimit(portfolio = portfolio.st, sym, timestamp="2000-01-01", maxpos=100, 
              longlevels = 1, minpos=-100, shortlevels = 1)
}

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
                        orderqty=+100, osFUN='osMaxPos', replace=FALSE
         ),
         type='enter', label='EnterLONG'
)

add.rule(strategy=strat, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE, orderside='short', ordertype='market', 
                        orderqty=-100, osFUN='osMaxPos', replace=FALSE
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
                      portfolio=portfolio.st, account = account.st, nsamples=0, verbose = TRUE)
stats <- out$tradeStats

wd <- getwd()
csv_file <- paste(wd,"DMAopt2",".csv", sep="")
out <- write.csv(stats,             # write to file
                 file = csv_file,
                 quote = FALSE, row.names = TRUE)

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
for (sym in symbol){
  dfName <- paste(sym,"stats", sep = "")
  statSubsetDf <- subset(stats, Symbol == sym)
  assign(dfName, statSubsetDf)
  z <- tapply(X=statSubsetDf$Net.Trading.PL, 
              INDEX = list(statSubsetDf$ma_fast,statSubsetDf$ma_slow), 
              FUN = median)
  x <- as.numeric(rownames(z))
  y <- as.numeric(colnames(z))
  filled.contour(x=x,y=y,z=z,color=heat.colors,xlab="ma_fast",ylab="ma_slow")
  title(sym)
}

Sys.setenv(TZ=ttz)                                             # Return to original time zone
