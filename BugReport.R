# This R Script provides a minimal reproducible example of a bug in the apply.paramset function in which
# if the a stop-loss order threshold is assigned to an indicator column the optimization fails to complete.

# Library and time zone setup
library(quantstrat)       # Required package for strategy back testing
ttz<-Sys.getenv('TZ')     # Time zone to UTC, saving original time zone
Sys.setenv(TZ='UTC')

# Quantstrat general variables
strat        <- "DMA1EQ"       # Give the stratgey a name variable
portfolio.st <- "portf1"       # Portfolio name
account.st   <- "acct1"        # Account name
initEq       <- 10000          # this parameter is required to get pct equity rebalancing to work

# Strategy specific variables
MAfast <- seq(20, 100, by = 20)
MAslow <- seq(20, 200, by = 20)
atrMult <- 2

# Strategy Functions
# Custom indicator to generate the threshold multiplier to set an ATR based stop, if a simple % multiplier
# is desired just set it to return a fixed numeric. e.g. return(0.04)
atrStopThresh <- function(HLC, n=14, atr_mult=2){
  ATR <- ATR(HLC = HLC, n)
  pctATR <- (atr_mult*ATR$atr)/Cl(HLC)
  pctATR
}

# Symbols etc
currency('USD')             # set USD as a base currency
symbol <- "GSPC"            # Universe selection At this stage is only one symbol

# set the instument as a stock and get the data from yahoo
stock(symbol, currency = "USD", multiplier = 1)
getSymbols("^GSPC", from = '1995-01-01')

# if run previously, run this code
delete.paramset(strategy = strat,"DMA_OPT")
rm.strat(portfolio.st)

# initialize the portfolio, account and orders. Starting equity and assuming data post 1995.
initPortf(portfolio.st, symbols = symbol, initDate = "1995-01-01")
initAcct(account.st, portfolios = portfolio.st, initEq = initEq, initDate = "1995-01-01")
initOrders(portfolio = portfolio.st, initDate = "1995-01-01")

# define the strategy with a position limit to prevent multiple trades in a direction
strategy(strat, store = TRUE)
addPosLimit(portfolio.st, symbol, timestamp="1995-01-01", maxpos=100, 
            longlevels = 1, minpos=-100, shortlevels = 1)

# Add the indicators - One bband for the breakout another for the stop

add.indicator(strategy = strat,name = "SMA",arguments=list(x=quote(Cl(mktdata)[,1]),
                                                           n = MAfast), label = "nFast"
)

add.indicator(strategy = strat,name = "SMA",arguments=list(x=quote(Cl(mktdata)[,1]),
                                                           n = MAslow), label = "nSlow"
)

add.indicator(strategy = strat,name = "atrStopThresh",arguments=list(HLC=quote(mktdata),
                                                                     n = 100, atr_mult=atrMult), label = "atrStopThresh"
)

# Add the signals -  long only

add.signal(strategy=strat,name='sigCrossover', arguments = 
             list(columns=c("nFast", "nSlow"),relationship="gt"
             ),
           label='long'
)

# Add the rules
# a) Entry rule - enter on fast moving average cross above slow 
add.rule(strategy=strat,
         name='ruleSignal',
         arguments=list(sigcol='long', sigval=TRUE, orderside='long', ordertype='market', 
                        orderqty=+100, osFUN='osMaxPos', replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)

# b) Exit rule - Close on cross the other way
add.rule(strategy = strat, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE, orderside=NULL, ordertype='market',
                        orderqty="all", replace=TRUE, orderset = "ocolong"
         ),
         type='exit',
         label='ExitLONG'
)

# c) Stoploss rules
add.rule(strategy=strat,
         name='ruleSignal',
         arguments=list(sigcol='long', sigval=TRUE, orderside=NULL, ordertype='stoplimit', 
                        prefer='High', orderqty="all", replace=FALSE, orderset ="ocolong",
                        tmult=TRUE, threshold=quote("atr.atrStopThresh")
         ),
         type='chain', parent = "EnterLONG",
         label='StopLONG',enabled = FALSE
)

enable.rule(strat,type = "chain",label = "StopLONG")
out <- applyStrategy(strategy=strat , portfolios=portfolio.st) 
updatePortf(Portfolio = portfolio.st)                                      # Update the portfolio
updateAcct(name = account.st)
updateEndEq(account.st)

# Add distributions and constraints
add.distribution(strat,
                 paramset.label = "DMA_OPT",
                 component.type = "indicator",
                 component.label = "nFast",
                 variable = list( n = MAfast ),
                 label = "ma_fast"
)

add.distribution(strat,
                 paramset.label = "DMA_OPT",
                 component.type = "indicator",
                 component.label = "nSlow",
                 variable = list( n = MAslow ),
                 label = "ma_slow"
)

add.distribution.constraint(strat,
                            paramset.label = "DMA_OPT",
                            distribution.label.1 = "ma_fast",
                            distribution.label.2 = "ma_slow",
                            operator = "<",
                            label = "fastLTslow")

out <- apply.paramset(strat, paramset.label = "DMA_OPT",
                      portfolio=portfolio.st, account = account.st, nsamples=0, verbose = TRUE)
