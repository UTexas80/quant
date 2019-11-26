# Add any project specific configuration here.
add.config(
  apply.override = FALSE,
  currentYr = as.numeric(format(Sys.Date(), format="%y")),
  currentYr4 = as.numeric(format(Sys.Date(), format="%Y")),
  lastYr = as.numeric(format(Sys.Date(), format="%y")) - 1,
  LastYr4 = as.numeric(format(Sys.Date(), format="%Y"))-1,
  currentAY = as.numeric(paste(as.numeric(format(Sys.Date(), format="%y")) - 1, as.numeric(format(Sys.Date(), format="%y")), sep = "")),
  header = "Quantitative Trading" # header in reports,
)
################################################################################
## Quantstrat setup                                                          ###
################################################################################
options(getSymbols.warning4.0 = FALSE)                 # Suppresses warnings ###
rm(list = ls(.blotter), envir = .blotter)              # Do some house cleaning#
Sys.setenv(TZ = "UTC")                                 # Set the timezone    ###
currency("USD")                                        # Set the currency    ###
################################################################################
## Date Parameters                                                           ###
################################################################################
init_date <- "20027-01-01"
start_date <- "2003-01-01"
end_date <- "2019-11-25"

initDate = "2002-01-01"
from = "2003-01-01"
to = "2019-11-25"
################################################################################
## Equity Values                                                             ###
################################################################################
init_equity <- 1e4                        # $10,000                            #
adjustment <- TRUE                        # Adjust for Dividends, Stock Splits #
################################################################################
# Add project specific configuration that can be overridden from load.project()
add.config(
  apply.override = TRUE
)
