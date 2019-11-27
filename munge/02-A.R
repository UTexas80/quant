# Set initial values
initDate <- "2006-07-31"
endDate <- "2011-10-31"
initEq <- 1e+05

# Pull Yahoo Finance data
symbols <- c("SPY")
getSymbols(symbols, from = initDate, to = endDate, index.class = c("POSIXt",
  "POSIXct"))

# Set up instruments with FinancialInstruments package
currency("USD")
for (symbol in symbols) {
  stock(symbol, currency = "USD", multiplier = 1)
}

# Delete portfolio, account, and order book if they already
# exist
suppressWarnings(rm("account.RSI4", "portfolio.RSI4", pos = .blotter))
suppressWarnings(rm("order_book.RSI4", pos = .strategy))

# Initialize portfolio and account
initPortf("RSI4", symbols = symbols, initDate = initDate)
initAcct("RSI4", portfolios = "RSI4", initDate = initDate, initEq = initEq)
initOrders(portfolio = "RSI4", initDate = initDate)

# Initialize a strategy object
stratRSI4 <- strategy("RSI4")

#### INDICATORS#### Add the 200-day SMA indicator

stratRSI4 <- add.indicator(strategy = stratRSI4, name = "SMA",
  arguments = list(x = quote(Cl(mktdata)), n = 200), label = "SMA200")

# Add the RSI4 indicator
stratRSI4 <- add.indicator(strategy = stratRSI4, name = "RSI",
  arguments = list(price = quote(getPrice(mktdata)), n = 4),
  label = "RSI")

#### SIGNALS####

# There are two signals: The first is when close price is
# above the 200 day moving average and RSI4 is below 25

stratRSI4 <- add.signal(stratRSI4, name = "sigFormula", arguments = list(columns = c("Close",
  "SMA200", "RSI"), formula = "(Close > SMA200) & (RSI < 25)",
  label = "trigger", cross = TRUE), label = "Cl.gt.SMA")

# The second is when the RSI4 closes above 55
stratRSI4 <- add.signal(stratRSI4, name = "sigThreshold", arguments = list(threshold = 55,
  column = "RSI", relationship = "lt", cross = TRUE), label = "Cl.lt.RSI")

#### RULES####

# There are two rules: The first is to buy when the price is
# above the SMA and RSI4 below 25 (the first signal)

stratRSI4 <- add.rule(stratRSI4, name = "ruleSignal", arguments = list(sigcol = "Cl.gt.SMA",
  sigval = TRUE, orderqty = 1000, ordertype = "market", orderside = "long",
  pricemethod = "market", TxnFees = -5, osFUN = osMaxPos),
  type = "enter", path.dep = TRUE)

# The second is to sell when the RSI climbs above 55
stratRSI4 <- add.rule(stratRSI4, name = "ruleSignal", arguments = list(sigcol = "Cl.lt.RSI",
  sigval = TRUE, orderqty = "all", ordertype = "market", orderside = "long",
  pricemethod = "market", TxnFees = -5), type = "exit", path.dep = TRUE)

# Set position limits so we don't add to the position every
# month Close >
SMA10

addPosLimit("RSI4", "SPY", timestamp = initDate, maxpos = 1000,
  minpos = 0)

# Process the indicators and generate trades
out <- try(applyStrategy(strategy = stratRSI4, portfolios = "RSI4"))
updatePortf("RSI4")

# Evaluate results
portRet <- PortfReturns("RSI4")
portRet$Total <- rowSums(portRet, na.rm = TRUE)
charts.PerformanceSummary(portRet$Total)
tradeStats("RSI4")[, c("Symbol", "Num.Trades", "Net.Trading.PL<http://net.trading.pl/>
",
  "maxDrawdown")]
