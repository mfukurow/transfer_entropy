
##-------------------------------------------------------------------------
##
##  entropy_H.R
##    calculates shannon's entropy
##
##  Author        : Matasaburo Fukutomi
##  First publish : 01.11.2021
##  Last update   : 01.13.2021
##
##  Usage
##    H <- entropy_H(X)
##
##  Variables
##    inputs
##      X = probability distribution
##    output
##      H = shannon's entropy
##
##-------------------------------------------------------------------------

entropy_H <- function(X) {
  X <- X[X != 0]
  H <- sum(-X*log2(X))
  return(H)
}
