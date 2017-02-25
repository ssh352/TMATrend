# This strategy uses a simple moving average crossover (MAfast and MASlow), to either a) go long if the fast
# moving avergae is above the slow moving average or b) short if the fast moving avergae is below the slow 
# moving average. It is essentially a modified version of the luxor demo, with the ordersets removed. This
# prevents the one closes other behaviour and allows us to be always in the market. Has a rebalancing rule 
# that enables us to compare being 100% invested in this strategy to buy and hold. No leverage. Here the
# simple percentage multiple stop loss is replace with an ATR based stop loss, which is implemented
# via a custom indicatoe. Working.

# Library and time zone setup
library(quantstrat)       # Required package for strategy back testing
library(doParallel)       # For parrallel optimization
library(rgl)              # Library to load 3D trade graphs
library(reshape2)         # Library to load 3D trade graphs
ttz<-Sys.getenv('TZ')     # Time zone to UTC, saving original time zone
Sys.setenv(TZ='UTC')

# Quantstrat general variables
strat        <- "DMA1EQ"       # Give the stratgey a name variable
portfolio.st <- "DMA1EQ"       # Portfolio name
account.st   <- "DMA1EQ"       # Account name
initEq       <- 10000          # this parameter is required to get pct equity rebalancing to work

# Strategy specific variables
MAfast  <- 10        #fast moving average period
MAslow  <- 100        #slow moving average period
atrMult <- seq(1, 5, by = 1)            #atr multiplier

# Strategy Functions
# Custom indicator to generate the threshold multiplier to set an ATR based stop, if a simple % multiplier
# is desired just set it to return a fixed numeric. e.g. return(0.04)
atrStopThresh <- function(HLC, n=14, atr_mult=2){
  ATR <- ATR(HLC = HLC, n)
  pctATR <- (atr_mult*ATR$atr)/Cl(HLC)
  return(pctATR)
}

# Symbols etc
currency('USD')             # set USD as a base currency
symbol <- "GSPC"            # Universe selection At this stage is only one symbol

# set the instument as a future and get the data from yahoo
stock(symbol, currency = "USD", multiplier = 1)
getSymbols("^GSPC", from = '1995-01-01')

# if run previously, run this code from here down
delete.paramset(portfolio.st,"DMA_OPT")
rm.strat(portfolio.st)

# initialize the portfolio, account and orders. Starting equity and assuming data post 1995.
initPortf(portfolio.st, symbols = symbol, initDate = "1995-01-01")
initAcct(account.st, portfolios = portfolio.st, initEq = initEq, initDate = "1995-01-01")
initOrders(portfolio = portfolio.st, initDate = "1995-01-01")

# define the strategy with a position limit to prevent multiple trades in a direction
strategy(strat, store = TRUE)
addPosLimit(strat, symbol, timestamp="1995-01-01", maxpos=100, 
            longlevels = 1, minpos=-100, shortlevels = 1)

# Add the indicators - One bband for the breakout another for the stop

add.indicator(strategy = strat,name = "SMA",arguments=list(x=quote(Cl(mktdata)[,1]),
                                                           n = MAfast), label = "nFast"
)

add.indicator(strategy = strat,name = "SMA",arguments=list(x=quote(Cl(mktdata)[,1]),
                                                           n = MAslow), label = "nSlow"
)

add.indicator(strategy = strat,name = "atrStopThresh",arguments=list(HLC=quote(OHLC(mktdata)),
                                                          atr_mult = atrMult), 
              label = "atr_mult_stop"
)

# Add the signals -  Go long on a cross of the close greater than the breakout band and close on a cross 
# less than the close band. Signals reversed for a short.

add.signal(strategy=strat,name='sigCrossover', arguments = 
             list(columns=c("nFast", "nSlow"),relationship="gt"
             ),
           label='long'
)

add.signal(strategy=strat,name='sigCrossover',arguments = 
             list(columns=c("nFast", "nSlow"),relationship="lte"
             ),
           label='short'
)

# Add the rules
# a) Entry rules - enter on moving average cross, osMaxPos is the order function
add.rule(strategy=strat,
         name='ruleSignal',
         arguments=list(sigcol='long', sigval=TRUE, orderside='long', ordertype='market', 
                         orderqty=+100, osFUN='osMaxPos', replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)

add.rule(strategy=strat,
         name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE, orderside='short', ordertype='market', 
                        orderqty=-100, osFUN='osMaxPos', replace=FALSE
         ),
         type='enter',
         label='EnterSHORT'
)

# b) Exit rules - Close on cross the other way
add.rule(strategy = strat, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE, orderside=NULL, ordertype='market',
                        orderqty="all", replace=TRUE, orderset = "ocolong"
         ),
         type='exit',
         label='ExitLONG'
)

add.rule(strategy = strat, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE, orderside=NULL , ordertype='market',
                        orderqty="all", replace=TRUE, orderset = "ocoshort"
         ),
         type='exit',
         label='ExitSHORT'
)

# c) Stoploss rules
add.rule(strategy=strat,
         name='ruleSignal',
         arguments=list(sigcol='long', sigval=TRUE, orderside=NULL, ordertype='stoplimit', 
                        orderqty="all", replace=FALSE, orderset ="ocolong",
                        tmult=TRUE, threshold=quote("atr.atrStopThresh")
         ),
         type='chain', parent = "EnterLONG",
         label='StopLONG',enabled = TRUE
)

add.rule(strategy=strat,
         name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE, orderside=NULL, ordertype='stoplimit', 
                        orderqty="all", replace=FALSE, orderset ="ocoshort",
                        tmult=TRUE, threshold=quote("atr.atrStopThresh")
         ),
         type='chain', parent = "EnterSHORT",
         label='StopSHORT',enabled = TRUE
)

# Add distributions and constraints
add.distribution(portfolio.st,
                 paramset.label = "DMA_OPT",
                 component.type = "indicator",
                 component.label = "atr_mult_stop",
                 variable = list(atr_mult = atrMult),
                 label = "atr_mult_stop"
)

# Now apply the parameter sets for optimization
out <- apply.paramset(strat, paramset.label = "DMA_OPT",
                      portfolio=portfolio.st, account = account.st, 
                      nsamples=0, verbose = TRUE,audit = globalenv())
stats <- out$tradeStats

Sys.setenv(TZ=ttz)                                             # Return to original time zone
