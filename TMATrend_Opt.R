# New Script to use the source and save strategy function. Will optimize the dual moving average
# strategy with the getsymbols function from the desired source file and the parameter sets
# specified and save the results to file on the fileserver for later analysis.

# Load Quantstrat and required packages
require(quantstrat)
require(doMC)
#require(doParallel) #for windoze

# Set the system time to UTC
Sys.setenv(TZ="UTC")

# Get DMA include ile with parameters and Functions
source(paste(getwd(),"/TMAInclude.R",sep=""))

# Strategy specific variables
riskpct <- 0.02 # percentage of equity to risk 
risk    <- 1

# Paramset Variables
MAfastPset  <- seq(20, 200, by = 20)        # fast moving average period for paramset
MAslowPset  <- seq(50, 400, by = 50)        # slow moving average period for paramset
MAmedPset  <- seq(50, 400, by = 50)        # slow moving average period for paramset
atrMultPset <- seq(2, 10, by = 2)           # atr multiple to use for paramset

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

# Define Strategy
strategy(strat, store = TRUE)

# Add the indicators - A fast moving average (MA), a slow MA, a medium MA and the custom indicator
add.indicator(strategy = strat,name = "SMA",arguments=list(x=quote(Cl(mktdata)[,1]),
                                                           n = MAfast), label = "nFast"
)

add.indicator(strategy = strat,name = "SMA",arguments=list(x=quote(Cl(mktdata)[,1]),
                                                           n = MAslow), label = "nSlow"
)

add.indicator(strategy = strat,name = "SMA",arguments=list(x=quote(Cl(mktdata)[,1]),
                                                           n = MAmed), label = "nMed"
)

add.indicator(strategy = strat,name = "atrStopThresh",arguments=list(HLC=quote(mktdata),
                                                                     n = 20, atr_mult=atrMult), label = "atrStopThresh"
)

# Add the signals - long on the first occurence of fastMA over medMA over slowMA and 
# short on the first occurence of fastMA below medMA below slowMA. Exit long on fastMA 
# below medMA or medMA below slowMA. Exit short on fastMA above MedMA or medMA above slowMA

add.signal(strategy=strat,name='sigFormula', arguments = 
             list(columns=c("SMA.nFast", "SMA.nSlow","SMA.nMed"),formula="(SMA.nFast > SMA.nMed) & (SMA.nMed > SMA.nSlow)",
                  cross = TRUE
             ),
           label='longEntry'
)

add.signal(strategy=strat,name='sigFormula',arguments = 
             list(columns=c("SMA.nFast", "SMA.nSlow","SMA.nMed"),formula="(SMA.nFast < SMA.nMed) & (SMA.nMed < SMA.nSlow)",
                  cross = TRUE
             ),
           label='shortEntry'
)

add.signal(strategy=strat,name='sigFormula', arguments = 
             list(columns=c("SMA.nFast", "SMA.nSlow","SMA.nMed"),formula="(SMA.nFast < SMA.nMed) | (SMA.nMed < SMA.nSlow)",
                  cross = TRUE
             ),
           label='longExit'
)

add.signal(strategy=strat,name='sigFormula',arguments = 
             list(columns=c("SMA.nFast", "SMA.nSlow","SMA.nMed"),formula="(SMA.nFast > SMA.nMed) | (SMA.nMed > SMA.nSlow)",
                  cross = TRUE
             ),
           label='shortExit'
)

# Add the rules
# a) Entry rules - enter on moving average cross, osATRsize is the order function
add.rule(strategy=strat,
         name='ruleSignal',
         arguments=list(sigcol='longEntry', sigval=TRUE, orderside='long', ordertype='market', 
                        orderqty=+100, osFUN='osATRsize', replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)

add.rule(strategy=strat,
         name='ruleSignal',
         arguments=list(sigcol='shortEntry', sigval=TRUE, orderside='short', ordertype='market', 
                        orderqty=-100, osFUN='osATRsize', replace=FALSE
         ),
         type='enter',
         label='EnterSHORT'
)

# b) Exit rules - Close on cross the other way
add.rule(strategy = strat, name='ruleSignal',
         arguments=list(sigcol='longExit' , sigval=TRUE, orderside=NULL, ordertype='market',
                        orderqty="all", replace=TRUE, orderset = "ocolong",TxnFees = 'pctFee'
         ),
         type='exit',
         label='ExitLONG'
)

add.rule(strategy = strat, name='ruleSignal',
         arguments=list(sigcol='shortExit', sigval=TRUE, orderside=NULL , ordertype='market',
                        orderqty="all", replace=TRUE, orderset = "ocoshort",TxnFees = 'pctFee'
         ),
         type='exit',
         label='ExitSHORT'
)

# c) Stoploss rules using ordersets and ATR based threshold, not enabled by default
add.rule(strategy=strat,
         name='ruleSignal',
         arguments=list(sigcol='longEntry', sigval=TRUE, orderside=NULL, ordertype='stoplimit', 
                        prefer='High', orderqty="all", replace=FALSE, orderset ="ocolong",
                        tmult=TRUE, threshold="atr.atrStopThresh",TxnFees = 'pctFee'
         ),
         type='chain', parent = "EnterLONG",
         label='StopLONG',enabled = FALSE
)

add.rule(strategy=strat,
         name='ruleSignal',
         arguments=list(sigcol='shortEntry', sigval=TRUE, orderside=NULL, ordertype='stoplimit', 
                        prefer='Low', orderqty="all", replace=FALSE, orderset ="ocoshort",
                        tmult=TRUE, threshold="atr.atrStopThresh",TxnFees = 'pctFee'
         ),
         type='chain', parent = "EnterSHORT",
         label='StopSHORT',enabled = FALSE
)

# Add distributions and constraints
add.distribution(strategy = strat,
                 paramset.label = "TMA_OPT",
                 component.type = "indicator",
                 component.label = "nFast",
                 variable = list( n = MAfastPset ),
                 label = "ma_fast"
)

add.distribution(strategy = strat,
                 paramset.label = "TMA_OPT",
                 component.type = "indicator",
                 component.label = "nSlow",
                 variable = list( n = MAslowPset ),
                 label = "ma_slow"
)

add.distribution(strategy = strat,
                 paramset.label = "TMA_OPT",
                 component.type = "indicator",
                 component.label = "nMed",
                 variable = list( n = MAmedPset ),
                 label = "ma_med"
)

add.distribution(strategy = strat,
                 paramset.label = "TMA_OPT",
                 component.type = "indicator",
                 component.label = "atrStopThresh",
                 variable = list( atr_mult=atrMultPset ),
                 label = "atr"
)

add.distribution.constraint(strategy = strat,
                            paramset.label = "TMA_OPT",
                            distribution.label.1 = "ma_fast",
                            distribution.label.2 = "ma_med",
                            operator = "<",
                            label = "fastLTmed")

add.distribution.constraint(strategy = strat,
                            paramset.label = "TMA_OPT",
                            distribution.label.1 = "ma_med",
                            distribution.label.2 = "ma_slow",
                            operator = "<",
                            label = "medLTslow")

save.strategy(strat)

enable.rule(strat,type = "chain",label = "StopLONG")
enable.rule(strat,type = "chain",label = "StopSHORT")

# Register the cores for parralel procssing
registerDoMC(cores=detectCores())
#registerDoParallel(cores = detectCores()) #windoze

# Now apply the parameter sets for optimization, measure execution time
start.time <- Sys.time()
out <- apply.paramset(strat, paramset.label = "TMA_OPT",
                      portfolio=portfolio.st, account = account.st, nsamples=0, verbose = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

stats <- out$tradeStats

# Write to csv file for later analysis
out <- write.csv(stats,             
                 file = paste("/media/sjaoraid/strat/DMA/opt/DMACurrOpt",as.character(Sys.Date()),".csv", sep=""),
                 quote = FALSE, row.names = TRUE)
