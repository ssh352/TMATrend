# New Script to use the source and save strategy function

# Load Quantstrat and required packages
require(quantstrat)

# Set the system time to UTC
Sys.setenv(TZ="UTC")

# Get DMA include ile with parameters and Functions
source(paste(getwd(),"/DMAInclude.R",sep=""))

# Strategy specific variables
MAfast  <- 40   # fast moving average period
MAslow  <- 80   # slow moving average period
atrMult <- 10   # atr multiple for stop loss and order sizing
riskpct <- 0.02 # percentage of equity to risk 
risk    <- 1

# Paramset Variables
MAfastPset  <- seq(20, 100, by = 10)        # fast moving average period for paramset
MAslowPset  <- seq(40, 100, by = 10)        # slow moving average period for paramset
atrMultPset <- seq(2, 10, by = 1)           # atr multiple to use for paramset

# Get all symbols, uncomment what you need
# source(paste(getwd(),"/GetCommData.R",sep=""))
 source(paste(getwd(),"/GetCurrData.R",sep=""))

# Change symbol variable here if needed
# symbol <- ""

# if run previously, run this code from here down
rm.strat(portfolio.st, silent = FALSE)
rm.strat(account.st, silent = FALSE)

# initialize the portfolio, account and orders. Starting equity and assuming data post 1995.
initPortf(portfolio.st, symbols = symbol)
initAcct(account.st, portfolios = portfolio.st, initEq = initEq)
initOrders(portfolio = portfolio.st)

# define the strategy with a position limit to prevent multiple trades in a direction
for (sym in symbol){
  addPosLimit(portfolio = portfolio.st, sym, timestamp="2000-01-01", maxpos=100, 
              longlevels = 1, minpos=-100, shortlevels = 1)
}

load.strategy("DMA")

enable.rule(strat,type = "chain",label = "StopLONG")
enable.rule(strat,type = "chain",label = "StopSHORT")

# Apply the strategy assigning the output to a variable out
out <- applyStrategy(strategy=strat , portfolios=portfolio.st)

updatePortf(Portfolio = portfolio.st)                                      # Update the portfolio, acct and endeq
updateAcct(name = account.st)
updateEndEq(account.st)

# Plot the charts fo each symbol
for (sym in symbol){
  chart.Posn(Portfolio = portfolio.st, Symbol = sym, 
             TA=list("add_SMA(n=20)","add_SMA(n=40)"),
             Dates = "2006-01::2017-04") # Chart the position 
}
stats <- tradeStats(portfolio.st)

#plot the returns vs buy and hold
eq1 <- getAccount(account.st)$summary$End.Eq
rt1 <- Return.calculate(eq1,"log")
getSymbols("^GSPC", from = '1995-01-01')
rt2 <- periodReturn(GSPC, period = "daily")
returns <- cbind(rt1,rt2)
colnames(returns) <- c("DMA","SP500")
chart.CumReturns(returns,colorset=c(2,4),legend.loc="topleft",
                 main="Moving Average Crossover to Benchmark Comparison",ylab="cum return",xlab="",
                 minor.ticks=FALSE)
Sys.setenv(TZ=ttz)                                             # Return to original time zone
