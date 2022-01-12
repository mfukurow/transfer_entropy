
##-------------------------------------------------------------------------
##
##  joint_entropy_H.R
##    calculates shannon's joint entropy of multiple variables
##
##  Author        : Matasaburo Fukutomi
##  First publish : 01.11.2021
##  Last update   : 01.11.2021
##
##  Usage
##    H <- joint_entropy_H(df, list_breaks)
##
##  Variables
##    inputs
##      df = a data frame containing variables
##      list_breaks = bins for each variable in a list format
##    output
##      H = shannon's joint entropy
##
##-------------------------------------------------------------------------


# Library -----------------------------------------------------------------

library(dplyr)

# Functions ---------------------------------------------------------------

joint_entropy_H <- function(df, list_breaks) {
  # 1. extract variable names calculated for the joint entropy
  Vnames <- names(list_breaks)
  
  # 2. extract bin edges (low and high) from list_breaks for all 
  #    combination of variables
  comb_low_mat <- comb_list(lapply(list_breaks, function(x) x[-length(x)]))
  comb_high_mat <- comb_list(lapply(list_breaks, function(x) x[-1]))
  
  # 3. count multi-dimensional histogram
  counts <- multi_histcount(df, Vnames, comb_low_mat, comb_high_mat)
  prob <- counts / sum(counts)
  
  # 4. calculate joint entropy
  H <- entropy_H(prob)
  
  return(H)
}

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

multi_histcount <- function(df, Vnames, comb_low_mat, comb_high_mat) {
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
  n_comb <- nrow(comb_low_mat)
  counts <- numeric(n_comb)
  for (ii in 1:n_comb) {
    D_ii <- filter_(df, mhc_formula(Vnames, comb_low_mat[ii, ], comb_high_mat[ii, ]))
    counts[ii] <- nrow(D_ii)
  }
  return(counts)
}

entropy_H <- function(X) {
  X <- X[X != 0]
  H <- sum(-X*log2(X))
  return(H)
}
