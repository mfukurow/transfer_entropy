
##-------------------------------------------------------------------------
##
##  multi_histcount.R
##    counts multi-dimensional histogram
##
##  Author        : Matasaburo Fukutomi
##  First publish : 01.11.2021
##  Last update   : 01.13.2021
##
##  Usage
##    counts <- multi_histcount(df, list_breaks, comb_list_out)
##
##  Variables
##    inputs
##      df = a data frame containing variables
##      list_breaks = bins for each variable in a list format
##      comb_list_out = if it is TRUE, output will include data frames
##                      including comb_low_mat & comb_high_mat
##                      (default is FALSE)
##    output
##      counts = a vector of histogram count
##               (if you set comb_list_out is TRUE, this includes the data
##                frames in a list format)
##
##-------------------------------------------------------------------------

# Library -----------------------------------------------------------------

library(dplyr)


# Function ----------------------------------------------------------------

multi_histcount <- function(df, list_breaks, comb_list_out = FALSE) {
  
  # function to make all combination
  comb_list <- function(obj_list) {
    len_list <- length(obj_list)
    len_list_ii <- sapply(obj_list, length)
    for (ii in 1:len_list) {
      if (ii == 1) {
        rlt_mat <- rep(obj_list[[ii]], each = max(cumprod(len_list_ii[2:len_list])))
      } else if (ii > 1 & ii < len_list) {
        rlt_mat <- cbind(
          rlt_mat,
          rep(obj_list[[ii]], 
              each = max(cumprod(len_list_ii[(ii + 1):len_list])),
              times = max(cumprod(len_list_ii[1:(ii - 1)])))
        )
      } else {
        rlt_mat <- cbind(
          rlt_mat,
          rep(obj_list[[ii]], times = max(cumprod(len_list_ii[1:(len_list - 1)])))
        )
      }
    }
    rlt_mat <- data.frame(rlt_mat)
    colnames(rlt_mat) <- names(obj_list)
    return(rlt_mat)
  }
  
  # function to make a formula of filter_ function for a given combination
  mhc_formula <- function(Vnames, Vlow, Vhigh) {
    n_V <- length(Vnames)
    for (ii in 1:n_V) {
      if (ii == 1) {
        output_formula <- paste0(Vnames[1], " >= ", Vlow[1], " & ", 
                                 Vnames[1], " < ", Vhigh[1])
      } else {
        output_formula <- paste0(output_formula, " & ",
                                 paste0(Vnames[ii], " >= ", Vlow[ii], " & ",
                                        Vnames[ii], " < ", Vhigh[ii]))
      }
    }
    return(output_formula)
  }
  
  # 1. extract variable names calculated for the joint entropy
  Vnames <- names(list_breaks)
  
  # 2. extract bin edges (low and high) from list_breaks for all 
  #    combination of variables
  comb_low_mat <- comb_list(lapply(list_breaks, function(x) x[-length(x)]))
  comb_high_mat <- comb_list(lapply(list_breaks, function(x) x[-1]))
  
  # 3. count multi-dimensional histogram
  n_comb <- nrow(comb_low_mat)
  counts <- numeric(n_comb)
  for (ii in 1:n_comb) {
    D_ii <- filter_(df, mhc_formula(Vnames, comb_low_mat[ii, ], comb_high_mat[ii, ]))
    counts[ii] <- nrow(D_ii)
  }
  
  # 4. output
  if (comb_list_out == TRUE) {
    list_counts <- list()
    list_counts$counts <- counts
    list_counts$comb_low_mat <- comb_low_mat
    list_counts$comb_high_mat <- comb_high_mat
    return(list_counts)
  } else {
    return(counts)  
  }
  
}
