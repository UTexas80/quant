helper.function <- function()
{
  return(1)
}
################################################################################
## Stock Symbols                                                             ###
## https://tinyurl.com/yxcttpsa                                              ###
################################################################################
basic_symbols <- function() {
    symbols <- c(
        "IWM", # iShares Russell 2000 Index ETF
        "QQQ", # PowerShares QQQ TRust, Series 1 ETF
        "SPY" # SPDR S&P 500 ETF Trust
    )
}
enhanced_symbols <- function() {
    symbols <- c(
        basic_symbols(), 
        "TLT", # iShares Barclays 20+ Yr Treas. Bond ETF
        "XLB", # Materials Select Sector SPDR ETF
        "XLE", # Energy Select Sector SPDR ETF
        "XLF", # Financial Select Sector SPDR ETF
        "XLI", # Industrials Select Sector SPDR ETF
        "XLK", # Technology  Select Sector SPDR ETF
        "XLP", # Consumer Staples  Select Sector SPDR ETF
        "XLU", # Utilities  Select Sector SPDR ETF
        "XLV", # Health Care  Select Sector SPDR ETF
        "XLY" # Consumer Discretionary  Select Sector SPDR ETF
    )
}
global_symbols <- function() {
    symbols <- c(
        enhanced_symbols(), 
        "EFA", # iShares EAFE
        "EPP", # iShares Pacific Ex Japan
        "EWA", # iShares Australia
        "EWC", # iShares Canada
        "EWG", # iShares Germany
        "EWH", # iShares Hong Kong
        "EWJ", # iShares Japan
        "EWS", # iShares Singapore
        "EWT", # iShares Taiwan
        "EWU", # iShares UK
        "EWY", # iShares South Korea
        "EWZ", # iShares Brazil
        "EZU", # iShares MSCI EMU ETF
        "IGE", # iShares North American Natural Resources
        "IYR", # iShares U.S. Real Estate
        "IYZ", # iShares U.S. Telecom
        "LQD", # iShares Investment Grade Corporate Bonds
        "SHY" # iShares 42372 year TBonds
    )
}

################################################################################
## Tricks to manage the available memory in an R session                     ###
## https://tinyurl.com/rd7376n                                               ###
################################################################################
# improved list of objects -----------------------------------------------------
.ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
                           format(utils::object.size(x), units = "auto") })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Length_Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}

# shorthand
lsos <- function(..., n=100) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

