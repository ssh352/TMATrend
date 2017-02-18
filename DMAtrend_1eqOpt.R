# This strategy uses a simple moving average crossover (MAfast and MASlow), to either a) go long if the fast
# moving avergae is above the slow moving average or b) short if the fast moving avergae is below the slow 
# moving average. It is essentially a modified version of the luxor demo, with the ordersets removed. This
# prevents the one closes other behaviour and allows us to be always in the market. Has a rebalancing rule 
# that enables us to compare being 100% invested in this strategy to buy and hold. No leverage.

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
MAfast = seq(5, 200, by = 5)        #fast moving average period
MAslow = seq(10, 400, by = 10)      #slow moving average period

# Strategy Functions

# Symbols etc
currency('USD')             # set USD as a base currency
symbol <- "GSPC"            # Universe selection At this stage is only one symbol

# set the instument as a future and get the data from yahoo
stock(symbol, currency = "USD", multiplier = 1)
getSymbols("^GSPC", from = '1995-01-01')

# if run previously, run this code from here down
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

# Add the signals -  Go long on a cross of the close greater than the breakout band and close on a cross 
# less than the close band. Signals reversed for a short.

add.signal(strategy=strat,name='sigCrossover', arguments = 
             list(columns=c("nFast", "nSlow"),relationship="gt"
             ),
           label='long'
)

add.signal(strategy=strat,name='sigCrossover',arguments = 
             list(columns=c("nFast", "nSlow"),relationship="lt"
             ),
           label='short'
)

# Add the rules - what trades to make on the signals giving using osMaxPos to limit positions.


add.rule(strategy = strat, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='short',
                        ordertype='market',
                        orderqty="all",
                        replace=TRUE
         ),
         type='exit',
         label='Exit2LONG'
)

add.rule(strategy = strat, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty="all",
                        replace=TRUE
         ),
         type='exit',
         label='Exit2SHORT'
)

add.rule(strategy=strat,
         name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='long' ,
                        ordertype='stoplimit', prefer='High',
                        orderqty=+100, osFUN='osMaxPos',
                        replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)
add.rule(strategy=strat,
         name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='short',
                        ordertype='stoplimit', prefer='Low',
                        orderqty=-100, osFUN='osMaxPos', 
                        replace=FALSE
         ),
         type='enter',
         label='EnterSHORT'
)


# Percentage Equity rebalancing rule
add.rule(strat, 'rulePctEquity',
         arguments=list(rebalance_on='months',
                        trade.percent=1,
                        refprice=quote(last(getPrice(mktdata)[paste('::',curIndex,sep='')])[,1]),
                        digits=0
         ),
         type='rebalance',
         label='rebalance')

# Add distributions and constraints
add.distribution(portfolio.st,
                 paramset.label = "DMA_OPT",
                 component.type = "indicator",
                 component.label = "nFast",
                 variable = list( n = MAfast ),
                 label = "ma_fast"
)

add.distribution(portfolio.st,
                 paramset.label = "DMA_OPT",
                 component.type = "indicator",
                 component.label = "nSlow",
                 variable = list( n = MAslow ),
                 label = "ma_slow"
)

add.distribution.constraint(portfolio.st,
                            paramset.label = "DMA_OPT",
                            distribution.label.1 = "ma_fast",
                            distribution.label.2 = "ma_slow",
                            operator = "<",
                            label = "fastLTslow")

# Register the cores for parralel procssing
registerDoParallel(cores=detectCores())

# Now apply the parameter sets for optimization
out <- apply.paramset(strat, paramset.label = "DMA_OPT",
                      portfolio=portfolio.st, account = account.st, nsamples=0, verbose = TRUE)
stats <- out$tradeStats

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
  z <- tapply(X=statSubsetDf$Ann.Sharpe, 
              INDEX = list(statSubsetDf$ma_fast,statSubsetDf$ma_slow), 
              FUN = median)
  x <- as.numeric(rownames(z))
  y <- as.numeric(colnames(z))
  filled.contour(x=x,y=y,z=z,color=heat.colors,xlab="ma_fast",ylab="ma_slow")
  title(sym)
}

Sys.setenv(TZ=ttz)                                             # Return to original time zone
