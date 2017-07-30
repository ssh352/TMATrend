# 

library(quantstrat)       # Required package for strategy back testing
library(doMC)

source(paste(getwd(),"/TMAInclude.R",sep=""))

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

# Percentage Equity rebalancing rule
add.rule(strat, 'rulePctEquity',
         arguments=list(rebalance_on='months',
                        trade.percent=risk,
                        refprice=quote(last(getPrice(mktdata)[paste('::',curIndex,sep='')])[,1]),
                        digits=0
         ),
         type='rebalance',
         label='rebalance')

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
