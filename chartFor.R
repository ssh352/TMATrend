#' Chart to analyse walk.forward() objective function custom

  .audit <- NULL  # keep codetools happy
  
  # extract all portfolio names from the audit environment,
  # except result portfolio (which doesn't end with a digit)
  portfolios.st = ls(name=.audit, pattern='portfolio.*.[0-9]+')
  n <- length(portfolios.st)
  
  # calculate Net.Trading.PL for each portfolio, one xts col per portfolio
  PL.xts <- xts()
  for(portfolio.st in portfolios.st)
  {
    p <- getPortfolio(portfolio.st, envir=.audit)
    
    from <- index(p$summary[2])
    
    #R <- cumsum(p$summary['2004-01-01/','Net.Trading.PL'])
    R <- cumsum(p$summary[paste(from, '/', sep=''),'Net.Trading.PL'])
    names(R) <- portfolio.st
    
    PL.xts <- cbind(PL.xts, R)
  }
  
  # now for the result portfolio (which doesn't end with a digit)
  portfolio.st <- ls(name=.audit, pattern='portfolio.*[^.0-9]$')
  p <- getPortfolio(portfolio.st, envir=.audit)
  from <- index(p$summary[2])
  R <- cumsum(p$summary[paste(from, '/', sep=''),'Net.Trading.PL'])
  names(R) <- portfolio.st
  
  #PL.xts <- na.locf(PL.xts)
  PL.xts <- na.locf(cbind(PL.xts, R))
  
  # add drawdown columns for all portfolio columns
  CumMax <- cummax(PL.xts)
  Drawdowns.xts <- -(CumMax - PL.xts)
  data.to.plot <- as.xts(cbind(PL.xts, Drawdowns.xts))
  
  p <- plot(PL.xts, col="grey", main="Walk Forward Analysis")
  p <- lines(PL.xts[,n+1],col="blue",on=1)
  # set on=NA so it is drawn on a new panel
  p <- lines(Drawdowns.xts, col="grey", on=NA, main="Drawdowns")
  p <- lines(Drawdowns.xts [,n+1],col="blue",on=2)
  print(p)
