
##-------------------------------------------------------------------------
##
##  transfer_entropy.R
##    calculates transfer entropy
##
##  Author        : Matasaburo Fukutomi
##  First publish : 01.12.2021
##  Last update   : 01.12.2021
##
##  Usage
##    TE <- transfer_entropy(df, tau, breaks_list)
##
##  Variables
##    inputs
##      df = a data frame containing variables
##      tau = number of previous time points for the calculation
##      list_breaks = bins for each variable in a list format
##    output
##      TE = transfer entropy
##
##-------------------------------------------------------------------------


# Functions ---------------------------------------------------------------

time_shift_df <- function(df, tau) {
  
  # gather df structure information
  n_col <- ncol(df)
  n_row <- nrow(df)
  Vnames <- names(df)
  
  # determine tsdf structure
  n_col_tsdf <- n_col * (tau + 1)
  n_row_tsdf <- n_row - tau
  
  # loop for making time-shifted data
  tsdf <- matrix(numeric(n_col_tsdf * n_row_tsdf), ncol = n_col_tsdf)
  for (ii in 1:n_col) {
    for (jj in 0:tau) {
      tsdf[, (ii - 1) * (tau + 1) + jj + 1] <- df[(jj + 1):(n_row - tau + jj), ii]
    }
  }
  
  # output
  tsdf <- data.frame(tsdf)
  names(tsdf) <- paste0(rep(Vnames, each = tau + 1), 0:tau)
  return(tsdf)
  
}
