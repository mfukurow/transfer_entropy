
##-------------------------------------------------------------------------
##
##  multi_histcount.R
##    counts multi-dimensional histogram
##
##  Author        : Matasaburo Fukutomi
##  First publish : 01.11.2021
##  Last update   : 01.19.2021
##
##  Usage
##    counts <- multi_histcount(df, list_breaks, comb_list_out)
##
##  Variables
##    inputs
##      df = a data frame containing variables
##      list_breaks = bins for each variable in a list format
##
##    output
##      counts = a vector of histogram count
##               (if you set comb_list_out is TRUE, this includes the data
##                frames in a list format)
##
##-------------------------------------------------------------------------


# Function ----------------------------------------------------------------

multi_histcount <- function(df, list_breaks) {
  
  # function symbolizing data separated by bins
  hist_symbol <- function(x, breaks) {
    n_breaks <- length(breaks)
    symbols <- formatC(1:n_breaks, width = ceiling(log10(n_breaks)), flag = "0")
    rlt <- rep(symbols[1], length(x))
    for (ii in 1:n_breaks) {
      rlt[x > breaks[ii]] <- symbols[ii]
    }
    return(rlt)
  }
  
  # 1. extract variable names calculated for the joint entropy
  Vnames <- names(list_breaks)
  n_Vnames <- length(Vnames)
  
  # 2. symbolize all combination
  for (ii in 1:n_Vnames) {
    if (ii == 1) {
      symbols <- hist_symbol(df[[Vnames[ii]]], list_breaks[[Vnames[ii]]])
    } else {
      symbols <- paste0(symbols, hist_symbol(df[[Vnames[ii]]], list_breaks[[Vnames[ii]]]))
    }
  }
  
  # 3. count
  rlt <- as.vector(as.numeric(table(symbols)))
  return(rlt)
  
}

