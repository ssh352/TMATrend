# This script will attempt to calculate the walk forward efficiency of a strategy tested with
# the walk forward function in quantstrat. The easiest way is to ask for 2 walkforward 
# files and then extract from the training file the best parameter combo and net trading PL
# Then the more difficult part is getting extracting all the testing period data from
# the portfolio object.

startYear      <- "2006"
endYear        <- "2016"
trainingLength <- 5
testingLength  <- 1
anchored       <- FALSE
prefix         <- "wfa."
dataDir        <- ""

# Generate year numerics
numStartYear <- as.numeric(startYear)
numEndYear <- as.numeric(endYear)
firstTest <-numStartYear+trainingLength
numTests <- numEndYear-firstTest
testYears <- seq(firstTest,numEndYear,by=testingLength)

# Get Testing Returns for each Period
# Get Testing Period File
.audit <- NULL
load(list.files(pattern=glob2rx(paste(prefix,"results.RData", sep = ""))))

# Get Portfolio Summary
summary1 <- getPortfolio(Portfolio = "portfolio.portf", envir = .audit)$summary

# Get Returns for each testing year
for(year in testYears){
yearsumm <- summary1[as.character(year)]
name <- paste("PL",year,sep = "")
assign(name,sum(yearsumm[,"Net.Trading.PL"]))
}

if (anchored=TRUE){
  startTestDates <- numStartYear
} else{
  startTestDates <- seq(numStartYear,numStartYear+numTests,by=testingLength)
}

.audit <- NULL
chosen.one <- .audit$param.combo.nr[1L]
chosen.portfolio.st = ls(name=.audit, 
                         pattern=glob2rx
                         (paste('portfolio', '*', chosen.one, sep='.')))
trainingport <- getPortfolio(chosen.portfolio.st,env = .audit)
trainingSumm <- trainingport$summary["2011/2015"]
PLTrainAnn <- (sum(trainingSumm[,"Net.Trading.PL"]))/5
WFE2011 <- PL2011/PLTrainAnn
WFE2012 <- PL2012/PLTrainAnn
WFE2013 <- PL2013/PLTrainAnn
WFE2014 <- PL2014/PLTrainAnn
WFE2015 <- PL2015/PLTrainAnn
WFE2016 <- PL2016/PLTrainAnn
WFETotal <- mean(c(WFE2011,WFE2012,WFE2013,WFE2014,WFE2015,WFE2016))
