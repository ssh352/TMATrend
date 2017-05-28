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
risk <- 1

# Paramset Variables
MAfastPset  <- seq(20, 100, by = 10)        # fast moving average period for paramset
MAslowPset  <- seq(40, 100, by = 10)        # slow moving average period for paramset
atrMultPset <- seq(2, 10, by = 1)           # atr multiple to use for paramset

# Get all commodity symbols
source(paste(getwd(),"/GetCommData.R",sep=""))

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