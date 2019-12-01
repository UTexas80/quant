################################################################################
# How Do I in R?                                https://tinyurl.com/y9j67lfk ###
################################################################################
## Step 00.00 Processing Start Time - start the timer                        ###
################################################################################
start.time = Sys.time()
################################################################################
## Step 00.01 get stock symbols                                              ###
## Check existence of directory and create if doesn't exist                  ### https://tinyurl.com/y3adrqwa
################################################################################
symbols      <- basic_symbols()
getSymbols(Symbols = symbols,
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
# ------------Replace missing values (NA)       https://tinyurl.com/y5etxh8x ###
SPL.AX <-
  SPL.AX %>%
  na.omit()
## -- use FinancialInstrument::stock() to define the meta-data for the symbols.-
stock(symbols,
      currency = "USD",
      multiplier = 1)
################################################################################
## Step 00.02: Portfolio, Account, Strategy Setup                            ###
################################################################################
portfolio.st <- "Port.Luxor"
account.st   <- "Acct.Luxor"
strategy.st  <- "Strat.Luxor"
## -- remove residuals from previous runs. ----------------------------------###
rm.strat(portfolio.st)
rm.strat(account.st)
## portfolio, account and orders initialization. ----------------------------###
initPortf(name = portfolio.st,                    # Portfolio Initialization ###
          symbols = symbols,
          initDate = init_date)
# ------------------------------------------------------------------------------
initAcct(name = account.st,                       # Account Initialization   ###
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)
# ------------------------------------------------------------------------------
initOrders(portfolio = portfolio.st,              # Order Initialization     ###
           symbols = symbols,
           initDate = init_date)
# ------------------------------------------------------------------------------
strategy(strategy.st, store = TRUE)               # Strategy Initialization  ###
################################################################################
## Step 00.03a: Add Indicators to the Golden Cross Strategy                  ###
################################################################################
# Delete portfolio, account, and order book if they already exist
suppressWarnings(rm("account.GoldenX","portfolio.GoldenX",pos=.blotter))
suppressWarnings(rm("order_book.GoldenX",pos=.strategy))
# ------------------------------------------------------------------------------
## portfolio, account and orders initialization. ----------------------------###
initPortf(name = "GoldenX",                    # Portfolio Initialization    ###
          symbols = symbols,
          initDate = init_date)
# ------------------------------------------------------------------------------
initAcct(name = "GoldenX",                     # Account Initialization      ###
         portfolios="GoldenX",
         initDate=init_date,
         initEq=init_equity)
# ------------------------------------------------------------------------------
initOrders(portfolio="GoldenX",                # Order Initialization        ###
           symbols = symbols,
           initDate=init_date)
# ------------------------------------------------------------------------------
stratGoldenX <- strategy("GoldenX")            # Strategy Initialization     ###
################################################################################
## Step 00.03: Add Indicators to the Strategy                                ###
################################################################################
add.indicator(strategy = strategy.st,             # 10 day SMA               ###
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = 10),
              label = "nFast")

add.indicator(strategy = strategy.st,             # 30 day SMA               ###
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = 30),
              label = "nSlow")                    # label = mktdata column name#

####INDICATORS####---------------------------------------https://is.gd/SBHCcH---

# Add the 20-day SMA indicator
stratGoldenX <- add.indicator(strategy=stratGoldenX, name="EMA", arguments =
list(x=quote(mktdata[,4]), n=20), label="020")

# Add the 50-day SMA indicator
stratGoldenX <- add.indicator(strategy=stratGoldenX, name="EMA", arguments =
list(x=quote(mktdata[,4]), n=50), label="050")

# Add the 100-day SMA indicator
stratGoldenX <- add.indicator(strategy=stratGoldenX, name="EMA", arguments =
list(x=quote(mktdata[,4]), n=100), label="100")

# Add the 200-day SMA indicator
stratGoldenX <- add.indicator(strategy=stratGoldenX, name="EMA", arguments =
list(x=quote(mktdata[,4]), n=200), label="200")

# Add the GoldenX indicator
# stratGoldenX <- add.indicator(strategy=stratGoldenX, name="RSI", arguments =
# list(price = quote(getPrice(mktdata)), n=4), label="RSI")
################################################################################
## Step 00.04: Pass Signals to the Strategy                                  ###
################################################################################
add.signal(strategy = strategy.st,                # Long Signal              ###
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long")
# ------------------------------------------------------------------------------
add.signal(strategy = strategy.st,                # Short Signal             ###
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")
####SIGNALS####------------------------------------------https://is.gd/SBHCcH---

# The first is when a Golden Cross occurs, i.e.,
# EMA020 > EMA050 & EMA050 > EMA100 & EMA100 > EMA200
stratGoldenX <- add.signal(stratGoldenX,
                name="sigFormula",
                arguments = list
                  (columns=c("EMA.020","EMA.050","EMA.100", "EMA.200"),
                  formula = "(EMA.020 > EMA.050 & EMA.050 > EMA.100 & EMA.100 > EMA.200)",
                  label="trigger",
                  cross=TRUE),
                label="goldenX_EMA_open")

# The second is when a Golden Cross criteria is no longer met
stratGoldenX <- add.signal(stratGoldenX,
                name="sigFormula",
                arguments = list
                (columns=c("EMA.020","EMA.050","EMA.100", "EMA.200"),
                formula = "(EMA.020 <= EMA.050 | EMA.050 <= EMA.100 | EMA.100 <= EMA.200)",
                label="trigger",
                cross=TRUE),
                label="goldenX_EMA_close")

# stratRSI4 <- add.signal(stratGoldenX,
#                 name="sigThreshold",
#                 arguments=list(
#                   threshold=55,
#                   column="RSI",
#                   relationship="lt",
#                   cross=TRUE),
#                 label="Cl.lt.RSI")
################################################################################
## Step 00.05: Add Rules to the Strategy                                     ###
## Whenever our long variable (sigcol) is TRUE (sigval) we want to place a   ###
## stoplimit order (ordertype). Our preference is at the High (prefer) plus  ###
## threshold. We want to buy 100 shares (orderqty). A new variable EnterLONG ###
## will be added to mktdata. When we enter (type) a position EnterLONG will  ###
## be ## TRUE, otherwise FALSE. This order will not replace any other        ###
## open orders                                                               ###
################################################################################
## rules set up to enter positions based on our signals.                     ###
add.rule(strategy = strategy.st,                 # Open Long Position        ###
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderqty = 100,
                          ordertype = "stoplimit",
                          orderside = "long",
                          threshold = 0.0005,
                          prefer = "High",
                          TxnFees = -10,
                          replace = FALSE),
         type = "enter",
         label = "EnterLONG")
# ------------------------------------------------------------------------------
add.rule(strategy.st,                           # Open Short Position        ###
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderqty = -100,
                          ordertype = "stoplimit",
                          threshold = -0.005,
                          orderside = "short",
                          replace = FALSE,
                          TxnFees = -10,
                          prefer = "Low"),
         type = "enter",
         label = "EnterSHORT")
####RULES####--------------------------------------------https://is.gd/SBHCcH---
# The first is to buy when the Golden Crossing criteria is met
# (the first signal)
stratGoldenX <- add.rule(stratGoldenX,           # Open Long Position        ###
                name="ruleSignal",
                arguments=list(sigcol="goldenX_EMA_open",
                               sigval=TRUE,
                               orderqty=1000,
                               ordertype="market",
                               orderside="long",
                               pricemethod="market",
                               TxnFees=-5,
                               osFUN=osMaxPos),
                               type="enter",
                               path.dep=TRUE)
# ------------------------------------------------------------------------------
## rules set up to exit positions based on our signals.                      ###
# ------------------------------------------------------------------------------
add.rule(strategy.st,                            # Close Long Position       ###
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderside = "long",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = -10,
                          replace = TRUE),
         type = "exit",
         label = "Exit2SHORT")
# ------------------------------------------------------------------------------
add.rule(strategy.st,                            # Close Short Position      ###
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderside = "short",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = -10,
                          replace = TRUE),
         type = "exit",
         label = "Exit2LONG")
####RULES####--------------------------------------------https://is.gd/SBHCcH---
# The second is to sell when the RSI climbs above 55
stratGoldenX <- add.rule(stratGoldenX,
                name="ruleSignal",
                arguments=list(sigcol="goldenX_EMA_close",
                               sigval=TRUE,
                               orderqty="all",
                               ordertype="market",
                               orderside="long",
                               pricemethod="market",
                               TxnFees=-5),
                               type="exit",
                               path.dep=TRUE)


################################################################################
## Step 00.06: Set Position Limits                                           ###
################################################################################
addPosLimit("GoldenX", "SPL.AX", timestamp=initDate, maxpos=1000, minpos=0)
################################################################################
## Step 00.07: Apply Strategy                                                ###
################################################################################
cwd          <- getwd()
setwd("./reports/")
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  load(results_file)
} else {
  results  <- applyStrategy(strategy.st, portfolios = portfolio.st)
  updatePortf(portfolio.st)
  updateAcct(account.st)
  updateEndEq(account.st)
  if(checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)) {
    save(list = "results", file = results_file)
    save.strategy(strategy.st)
  }
}

goldenX_file <- paste("results", "GoldenX", "RData", sep = ".")
if( file.exists(goldenX_file) ) {
  load(goldenX_file)
} else {
  results  <- applyStrategy(stratGoldenX, portfolios = "GoldenX")
  updatePortf("GoldenX")
  updateAcct("GoldenX")
  updateEndEq("GoldenX")
  if(checkBlotterUpdate("GoldenX", "GoldenX", verbose = TRUE)) {
    save(list = "results", file = goldenX_file)
    save.strategy(stratGoldenX)
  }
}

setwd(cwd)
################################################################################
## Step 00.99: VERSION HISTORY                                               ###
################################################################################
a00.version = "1.0.0"
a00.ModDate = as.Date("2019-01-01")
################################################################################
# 2019.01.01 - v.1.0.0
#  1st release
################################################################################
