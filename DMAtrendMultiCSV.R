# Template for Strategy Development - SP500 testing

# Library and time zone setup
library(quantstrat)       # Required package for strategy back testing
ttz<-Sys.getenv('TZ')     # Time zone to UTC, saving original time zone
Sys.setenv(TZ='UTC')

# Quantstrat general variables
strat        <- "DMA1EQ"       # Give the stratgey a name variable
portfolio.st <- "DMA1EQ"       # Portfolio name
account.st   <- "DMA1EQ"       # Account name
initEq       <- 1000000        # this parameter is required to get pct equity rebalancing to work
csvDir       <- "C:/Users/RJK/Documents/SpiderOak Hive/Financial/commodities_data" # Directory containing csv files
xtsDates     <- "2006/"      # Variable for the point in time you want your prices series to line up from

# Strategy specific variables
MAfast = 70
MAslow = 200

# Strategy Functions
# A function to set the risk for rebalancing based on the number of symbols
setRisk <- function(symlist){
  n = 0
  for (sym in symlist){
    n = n+1
  }
  risk <- round(1/n, digits = 2)
  return(risk)
}

#Symbol Setup
# set the instument as a future and get the data from the csv file
# Setup the Environment
currency('USD')                                         # set USD as a base currency
symbol <- c("LSU","RR","CO","NG","OJ","LB","HG","LC")   # Universe selection
risk <- setRisk(symbol)                                 # set the risk for rebalancing using the function

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
rm.strat(portfolio.st)

# initialize the portfolio, account and orders. Starting equity and assuming data post 1995.
initPortf(portfolio.st, symbols = symbol, initDate = "1995-01-01")
initAcct(account.st, portfolios = portfolio.st, initEq = initEq, initDate = "1995-01-01")
initOrders(portfolio = portfolio.st, initDate = "1995-01-01")

# define the strategy with a position limit to prevent multiple trades in a direction
strategy(strat, store = TRUE)
for (sym in symbol){
  addPosLimit(strat, sym, timestamp="2000-01-01", maxpos=100, 
              longlevels = 1, minpos=-100, shortlevels = 1)
}

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
                        trade.percent=risk,
                        refprice=quote(last(getPrice(mktdata)[paste('::',curIndex,sep='')])[,1]),
                        digits=0
         ),
         type='rebalance',
         label='rebalance')

# Apply the strategy assigning the output to a variable out
out <- applyStrategy.rebalancing(strategy=strat , portfolios=portfolio.st)
updatePortf(Portfolio = portfolio.st)                                      # Update the portfolio, acct and endeq
updateAcct(name = account.st)
updateEndEq(account.st)

# Plot the charts fo each symbol
for (sym in symbol){
  chart.Posn(Portfolio = portfolio.st, Symbol = sym, 
             TA=list("add_SMA(n=50)","add_SMA(n=100)"),
             Dates = "2006-01::2017-01") # Chart the position 
}
stats <- tradeStats(portfolio.st)

#plot the returns vs buy and hold
eq1 <- getAccount(account.st)$summary$End.Eq
rt1 <- Return.calculate(eq1,"log")
rt2 <- periodReturn(GSPC, period = "daily")
returns <- cbind(rt1,rt2)
colnames(returns) <- c("BB","SP500")
chart.CumReturns(returns,colorset=c(2,4),legend.loc="topleft",
                 main="BBand to Benchmark Comparison",ylab="cum return",xlab="",
                 minor.ticks=FALSE)
Sys.setenv(TZ=ttz)                                             # Return to original time zone
