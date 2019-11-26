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
## Step 00.03: Ass Indicators to the Strategy                                ###
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
              label = "nSlow")
################################################################################
## Step 00.04: Pass Signals to the Strategy                                   ###
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
################################################################################
## Step 00.05: Ass Rules to the Strategy                                     ###
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
################################################################################
## Step 00.06: Apply Strategy                                                ###
################################################################################
cwd          <- getwd()
setwd("./_data/")
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
