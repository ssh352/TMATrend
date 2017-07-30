# New Script to use the source and save strategy function. Will optimize the dual moving average
# strategy with the getsymbols function from the desired source file and the parameter sets
# specified and save the results to file on the fileserver for later analysis.

# Load Quantstrat and required packages
require(quantstrat)
require(doMC)

# Set the system time to UTC
Sys.setenv(TZ="UTC")

# Get DMA include ile with parameters and Functions
source(paste(getwd(),"/TMAInclude.R",sep=""))

# Strategy specific variables
riskpct <- 0.02 # percentage of equity to risk 
risk    <- 1

# Paramset Variables
MAfastPset  <- seq(50, 400, by = 50)        # fast moving average period for paramset
MAslowPset  <- seq(50, 400, by = 50)        # slow moving average period for paramset
MAmedPset  <- seq(50, 400, by = 50)        # slow moving average period for paramset
atrMultPset <- seq(2, 10, by = 1)           # atr multiple to use for paramset

# Get all symbols, uncomment what you need
# source(paste(getwd(),"/GetCommData.R",sep=""))
# source(paste(getwd(),"/GetCurrData.R",sep=""))
 source(paste(getwd(),"/GetEqData.R",sep=""))

# Change symbol variable here if needed
# symbol <- ""

# If run previously, run this code from here down
rm.strat(portfolio.st, silent = FALSE)
rm.strat(account.st, silent = FALSE)
delete.paramset(strat,"TMA_OPT")

# Initialize the portfolio, account and orders. Starting equity and assuming data post 1995.
initPortf(portfolio.st, symbols = symbol)
initAcct(account.st, portfolios = portfolio.st, initEq = initEq)
initOrders(portfolio = portfolio.st)

# Define the strategy with a position limit to prevent multiple trades in a direction
for (sym in symbol){
  addPosLimit(portfolio = portfolio.st, sym, timestamp="2000-01-01", maxpos=100, 
              longlevels = 1, minpos=-100, shortlevels = 1)
}

load.strategy("TMA")

enable.rule(strat,type = "chain",label = "StopLONG")
enable.rule(strat,type = "chain",label = "StopSHORT")

# Register the cores for parralel procssing
registerDoMC(cores=detectCores())

# Now apply the parameter sets for optimization
out <- apply.paramset(strat, paramset.label = "TMA_OPT",
                      portfolio=portfolio.st, account = account.st, nsamples=0, verbose = TRUE)

stats <- out$tradeStats

# Write to csv file for later analysis
out <- write.csv(stats,             
                 file = paste("/media/sjaoraid/strat/DMA/opt/DMACurrOpt",as.character(Sys.Date()),".csv", sep=""),
                 quote = FALSE, row.names = TRUE)