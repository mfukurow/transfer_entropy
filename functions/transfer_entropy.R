
##-------------------------------------------------------------------------
##
##  transfer_entropy.R
##    calculates transfer entropy
##
##  Author        : Matasaburo Fukutomi
##  First publish : 01.12.2021
##  Last update   : 01.13.2021
##
##  Usage
##    TE <- transfer_entropy(df, tau, list_breaks)
##
##  Variables
##    inputs
##      df = a data frame containing variables
##           includes only two columns; 
##           the first is "effect" and the second is "cause"
##      tau = number of previous time points for the calculation
##      list_breaks = bins for each variable in a list format
##    output
##      TE = transfer entropy
##
##-------------------------------------------------------------------------


# Function ---------------------------------------------------------------

transfer_entropy <- function(df, tau, list_breaks) {
  
  # 1. make a time-shifted data frame
  tsdf <- time_shift_df(df, tau)
  list_breaks <- rep(list_breaks, each = tau + 1)
  Vnames <- names(tsdf)
  names(list_breaks) <- Vnames
  
  # 2. calculate transfer entropy
  TE <- joint_entropy_H(tsdf[, 1:(tau + 1)], list_breaks[Vnames[1:(tau + 1)]]) -
    joint_entropy_H(tsdf[, 2:(tau + 1)], list_breaks[Vnames[2:(tau + 1)]]) -
    joint_entropy_H(tsdf[, c(1:(tau + 1), (tau + 3):(2 * (tau + 1)))], 
                    list_breaks[Vnames[c(1:(tau + 1), (tau + 3):(2 * (tau + 1)))]]) +
    joint_entropy_H(tsdf[, c(2:(tau + 1), (tau + 3):(2 * (tau + 1)))],
                    list_breaks[Vnames[c(2:(tau + 1), (tau + 3):(2 * (tau + 1)))]])
  
  return(TE)
  
}
