# This strategy uses a simple moving average crossover (MAfast and MASlow), to either a) go long if the fast
# moving avergae is above the slow moving average or b) short if the fast moving avergae is below the slow 
# moving average. It is essentially a modified version of the luxor demo, with the ordersets removed. This
# prevents the one closes other behaviour and allows us to be always in the market. Has a rebalancing rule 
# that enables us to compare being 100% invested in this strategy to buy and hold. No leverage.

# Library and time zone setup
library(quantstrat)       # Required package for strategy back testing
ttz<-Sys.getenv('TZ')     # Time zone to UTC, saving original time zone
Sys.setenv(TZ='UTC')

# Quantstrat general variables
strat        <- "DMA1EQ"       # Give the stratgey a name variable
portfolio.st <- "DMA1EQ"       # Portfolio name
account.st   <- "DMA1EQ"       # Account name
initEq       <- 10000          # this parameter is required to get pct equity rebalancing to work

# Strategy specific variables
MAfast = 70
MAslow = 200

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

# Add the indicators
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

# Percentage Equity rebalancing rule
add.rule(strat, 'rulePctEquity',
         arguments=list(rebalance_on='months',
                        trade.percent=1,
                        refprice=quote(last(getPrice(mktdata)[paste('::',curIndex,sep='')])[,1]),
                        digits=0
         ),
         type='rebalance',
         label='rebalance')


out <- applyStrategy.rebalancing(strategy=strat , portfolios=portfolio.st) # Attempt the strategy
updatePortf(Portfolio = portfolio.st)                                      # Update the portfolio
updateAcct(name = account.st)
updateEndEq(account.st)

#chart the position
chart.Posn(Portfolio = portfolio.st, Symbol = symbol, 
           TA=list("add_SMA(n=10)","add_SMA(n=100)"), 
           Dates = "1996-01::2016-12")          # Chart the position
stats <- tradeStats(portfolio.st)
OB <- get.orderbook(portfolio.st)
orders <- OB$DMA1EQ$GSPC

#plot the returns vs buy and hold
eq1 <- getAccount(account.st)$summary$End.Eq
rt1 <- Return.calculate(eq1,"log")
rt2 <- periodReturn(GSPC, period = "daily")
returns <- cbind(rt1,rt2)
colnames(returns) <- c("DMA","SP500")
chart.CumReturns(returns,colorset=c(2,4),legend.loc="topleft",
                 main="Simple Dual Moving Average to Benchmark Comparison",ylab="cum return",xlab="",
                 minor.ticks=FALSE)
Sys.setenv(TZ=ttz)                                             # Return to original time zone
