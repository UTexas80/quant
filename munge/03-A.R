# quantstrat: how to create multiple indicators, signal rules
# https://is.gd/4pQpKz

require(quantstrat)
suppressWarnings(rm("order_book.macd",pos=.strategy))
suppressWarnings(rm("account.macd","portfolio.macd",pos=.blotter))
suppressWarnings(rm("account.st","portfolio.st","stock.str","stratMACD","startDate","initEq",'start_t','end_t'))

stock.str='AAPL' # what are we trying it on


fastMA = 12
slowMA = 26
signalMA = 8
maType="EMA"

currency('USD')
stock(stock.str,currency='USD',multiplier=1)

startDate='2016-12-31'
initEq=1000000
portfolio.st='macd'
account.st='macd'

getSymbols(stock.str,from=startDate)


initPortf(portfolio.st,symbols=stock.str)
initAcct(account.st,portfolios=portfolio.st)
initOrders(portfolio=portfolio.st)

strat.st<-portfolio.st
# define the strategy
strategy(strat.st, store=TRUE)

#one indicator
add.indicator(strat.st, name = "MACD",
              arguments = list(x=quote(Cl(mktdata)),
                               nFast=fastMA,
                               nSlow=slowMA),
              label='_'
)

add.indicator(strat.st, name = "SMA",
              arguments = list(x=quote(Cl(mktdata)),
                               n=20),
              label='SMA020'
)


add.indicator(strat.st, name = "SMA",
              arguments = list(x=quote(Cl(mktdata)),
                               n = 50),
              label='SMA050'
)

add.indicator(strat.st, name = "SMA",
              arguments = list(x=quote(Cl(mktdata)),
                               n = 100),
              label='SMA100'
)


add.indicator(strat.st, name = "SMA",
              arguments = list(x=quote(Cl(mktdata)),
                               n = 200),
              label='SMA200'
)


# Create your own signal for entry:
goldenX_SMA_Buy <- function(data) {
  # first condition:
  sig <- data[, "SMA.SMA020"] > data[, "SMA.SMA050"] & data[, "SMA.SMA050"] > data[, "SMA.SMA100"] & data[, "SMA.SMA100"] > data[, "SMA.SMA200"]
  colnames(sig) <- "upSig"
  sig
}


# Create your own signal for entry:
macdSMAsig <- function(data) {
  # first condition:
  sig <- data[, "SMA.SMA020"] > data[, "SMA.SMA050"] & data[, "macd._"] > 0
  colnames(sig) <- "upSig"
  sig
}

 # Activate (uncomment) only ONE of the following signals.  Both do the same thing:

#OPTION 1 for entry signal based on combining signals:
add.signal(strat.st,name="goldenX_SMA_Buy",
           arguments = list(data = quote(mktdata)),
           label="enterSig"
)


# #OPTION 1b for entry signal based on combining signals:
# add.signal(strat.st,name="macdSMAsig",
#            arguments = list(data = quote(mktdata)),
#            label="enterSig"
# )

#OPTION 2 for entry signal based on combining signals:
# add.signal(strat.st, name = "sigFormula",
#            arguments = list(data = quote(mktdata),
#                             formula = "SMA.SMA50 > SMA.SMA10 & macd._ > 0"),
#            label = "upSig.enterSig"
#            )



add.signal(strat.st,name="sigThreshold",
           arguments = list(column="signal._",
                            relationship="lt",
                            threshold=0,
                            cross=TRUE),
           label="signal.lt.zero"
)

goldenX_SMA_Sell <- function(data) {
  # first condition:
  sig <- data[, "SMA.SMA020"] < data[, "SMA.SMA050"] | data[, "SMA.SMA050"] < data[, "SMA.SMA100"] | data[, "SMA.SMA100"] < data[, "SMA.SMA200"]
  colnames(sig) <- "upSig"
  sig
}
####
# add rules

# entry
add.rule(strat.st,name='ruleSignal',
         # be careful to get the label of the signal column correct:
         arguments = list(sigcol="upSig.enterSig",
                          sigval=TRUE,
                          orderqty=100,
                          ordertype='market',
                          orderside='long',
                          threshold=NULL),
         type='enter',
         label='enter',
         storefun=FALSE
)

# exit
add.rule(strat.st,name='ruleSignal',
         arguments = list(sigcol="signal.lt.zero",
                          sigval=TRUE,
                          orderqty='all',
                          ordertype='market',
                          orderside='long',
                          threshold=NULL,
                          orderset='exit2'),
         type='exit',
         label='exit'
)

#end rules
####


out<-applyStrategy(strat.st , portfolios=portfolio.st,verbose=TRUE)

updatePortf(Portfolio=portfolio.st,Dates=paste('::',as.Date(Sys.time()),sep=''))

chart.Posn(Portfolio=portfolio.st,Symbol=stock.str)

tx <- getTxns(portfolio.st, stock.str)
sum(tx$Net.Txn.Realized.PL)
