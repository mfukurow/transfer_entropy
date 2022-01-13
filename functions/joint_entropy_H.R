
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


# Functions ---------------------------------------------------------------

joint_entropy_H <- function(df, list_breaks) {
  
  # 1. count multi-dimensional histogram
  counts <- multi_histcount(df, list_breaks)
  prob <- counts / sum(counts)
  
  # 2. calculate joint entropy
  H <- entropy_H(prob)
  
  return(H)
}
