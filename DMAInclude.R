# This is the include file for Dual Moving Avergae Crossover Function

Sys.setenv(TZ='UTC')

# Quantstrat general variables
strat        <- "DMA"           # Give the stratgey a name variable
portfolio.st <- "portf"         # Portfolio name
account.st   <- "accnt"         # Account name
initEq       <- 100000          # this parameter is required to get pct equity rebalancing to work

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
  pctATR <- (atr_mult*ATR$atr)/Cl(HLC)
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
  feeMult <- 0.0005
  fee <- round(-1*(feeMult*abs(TxnPrice)*abs(TxnQty)),0)
  return(fee)
}