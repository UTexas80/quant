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
basic_symbols <- function() {
    symbols <- c(
        "SPL.AX" # Starpharma
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

# How to add multiple conditions to quantstrat?
# https://is.gd/tXXx8V
#'sigCOMP
#'@description signal comparison operators incl and, or, xor for quantstrat signals.
#'@param label name of the output signal
#'@param data the market data
#'@param columns the signal columns to intersect, if a second level comparison is used, the comparison result must reside in the first column only (compare one 2nd level with a True/False Column) or in both, marked by Keyword '2nd'
#'@param relationship operators gte, gt, lte, lt, eq, and, or, xor  TODO:NOT
#'@param secondComparison vector of columns to intersect, if yes, then also set the relationship comparison
#'@param relationshipSecondComparison operators gte, gt, lte, lt, eq
#'@param offset1 optional
#'@param offset2 optional
#'@return a new signal column that intersects the provided columns
#'@export


sigCOMP <- function (label, data = mktdata, columns, relationship = c("gte", "gt", "lte", "lt", "eq", "and", "or", "xor"),  relationshipSecondComparison = c("gte", "gt", "lte", "lt", "eq"), secondComparison, res_not, offset1 = 0, offset2 = 0) 
{
  ret_sig = NULL
  compcols <- NULL

  if(!missing(columns)){
    if (relationship == "op") {
      if (columns[1] %in% c("Close", "Cl", "close")) 
        stop("Close not supported with relationship=='op'")
      switch(columns[1], Low = , low = , bid = {
        relationship = "lt"
      }, Hi = , High = , high = , ask = {
        relationship = "gt"
      })
    } #whatever that is

    colNums <- NULL  
    for(sec in 1:length(columns)){
      if (columns[sec]=='2nd'){
        colNums <- c(colNums,0)
      }
      else{
        colNums <- c(colNums, match.names(columns[sec], colnames(data)))
      }
    }

    opr <- switch(relationship[1], 
                  gt = , `>` = ">",  
                  gte = , gteq = , ge = , `>=` = ">=",
                  lt = , `<` = "<", 
                  lte = , lteq = , le = , `<=` = "<=",
                  eq = , `==` = , `=` = "==",
                  and = "&",
                  or = "|",
                  xor = "xor"
                  # todo: NOT
    )

  } #perform preparation actions if 1|2 columns exist or else stop 
  else {

      stop("only works if two comparison columns are provided. for true/false evaluations you can add e.g. 2nd 2nd or <Signal>, 2nd ")  


  }

  if (!missing(secondComparison))
    {
      ret_sig2nd <- NULL
      opr2nd <- c(1:length(secondComparison))

        if (length(secondComparison) != length(relationshipSecondComparison)){
          stop("make sure to have a comparison operator for each second level comparison you would like to perform")
        } 
        else {

          for (j in 1:length(relationshipSecondComparison)) {
              # run through pairs of columns and relationship checks and return these in a dataframe ret_sig2nd
              # the return column of the appropriate pair will have the name col1 op col2 e.g. close gt nFast

              colNums2nd <- c(0,0)
              comp2ndPartners <- unlist(secondComparison[j])
              relationship2 <- unlist(relationshipSecondComparison)[j]
              colNums2nd[1] <- match.names(comp2ndPartners[1], colnames(data))
              colNums2nd[2] <- match.names(comp2ndPartners[2], colnames(data))
                opr2nd[j] <- switch(relationship2, 
                                  gt = , `>` = ">",  
                                  gte = , gteq = , ge = , `>=` = ">=",
                                  lt = , `<` = "<", 
                                  lte = , lteq = , le = , `<=` = "<=",
                                  eq = , `==` = , `=` = "==",
                                  and = "&",
                                  or = "|",
                                  xor = "xor"
                                  # todo: NOT
              )
               ret_append <- do.call(opr2nd[j], list(data[, colNums2nd[1]] + offset1, 
                                           data[, colNums2nd[2]] + offset2))  

               colnames(ret_append) <- paste0(comp2ndPartners[1]," ",relationship2[j]," ",comp2ndPartners[2])
               ret_sig2nd <- cbind(ret_sig2nd,ret_append)
               rm(ret_append)
            }

          compcols <- ret_sig2nd  
        } # end of 2nd Comp = 2nd Relationship validity block

      if(ncol(compcols)==1){ # check the case if only one second level comparison exists
        transfer2ndToFirst <- compcols  #assumption is, the second level comparison took place with the first column of the first level
        # if one second level comparison is provided, execute transfer object with second column of first level
        compcols <- transfer2ndToFirst[, 1] #offset already included in second level comparison
        compcols <- cbind(compcols, data[, colNums[2]] + offset2)

      } # provide the transfer object to be used in the first level comparison if only one second level comparison exists
    }
    else { # check the case if no second level comparison exists

      # if no second level comparison is provided, only execute first level
      compcols <- data[, colNums[1]] + offset1
      compcols <- cbind(compcols, data[, colNums[2]] + offset2)
    } # if no second level exists, execute comparison for first level only

    # for all cases, perform the first level comparison with the columns stored in compcols - offset has to be applied before storing to compcols 
    ret_sig <- do.call(opr, list(compcols[, 1] , 
                                 compcols[, 2] ))  

  colnames(ret_sig) <- label
  return(ret_sig)
}

# ### TESTS
# # To compare just two (first level) colums
# rm(testOnlyFirst)
# testOnlyFirst<- sigCOMP(
#   columns=c("nSlow","nFast"),
#   relationship=c("gt"),
#   label='GT'
# )
# 
# 
# #To compare a signal or another T/F value with a second level comparison
# rm(testOneSecond)
# testOneSecond<- sigCOMP(
#   columns=c("2nd","exitLong"),
#   relationship=c("and"),
#   secondComparison =list(c("Close", "nFast")),
#   relationshipSecondComparison = list(c("gt")),
#   label='andGT'
# )
# 
# 
# rm(test2Second)
# test2Second<- sigCOMP(
#   columns=c("2nd", "2nd"),
#   relationship=c("or"),
#   secondComparison =list(c("Close", "nFast"), c("Close", "nSlow")),
#   relationshipSecondComparison = list(c("gt"), c("gt")),
#   label='orGT'
# )
# 
# rm(test2SecondOr)
# test2SecondOr<- sigCOMP(
#   columns=c("2nd", "2nd"),
#   relationship=c("or"),
#   secondComparison =list(c("Close", "nFast"), c("Close", "nSlow")),
#   relationshipSecondComparison = list(c("gt"), c("gt")),
#   label='orGT'
# )
# 
# rm(test2SecondXor)
# test2SecondXor<- sigCOMP(
#   columns=c("2nd", "2nd"),
#   relationship=c("xor"),
#   secondComparison =list(c("Close", "nFast"), c("Close", "nSlow")),
#   relationshipSecondComparison = list(c("gt"), c("gt")),
#   label='orGT'
# )


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

