#' @title Net Present Value Calculator
#'
#' @description Calculates the net present value of the cashflows with a given interest rate
#'
#' @usage NPV(c(0, 1, 2, 3, 0), .1)
#'
#'
#' @param cashflows total incoming cash values in vector form for all compounding/payment periods (first element in vector is in present time)
#' @param interest_rate set rate of return for the compounding periods
#'
#' @return the net present value of the cashflows (the overall value if it was a "fair trade" for the given interest rate)
#'
#' @import tidyverse
#'
#' @export


NPV <- function(cashflows, interest_rate){
  len = as.numeric(length(cashflows))
  time = 1:len
  dat <- data.frame(time, cashflows)
  dat <- dat %>%
    dplyr::mutate(cashflows, pv = cashflows/(1+interest_rate)^(time-1))
  pv = sum(dat$pv)
  return(pv)
}

#' @title IRR Calculator
#'
#' @description Calculates the interest rate for the given cashflows to be a "fair trade"
#'
#' @usage NPV(c(0, 1, 2, 3, 0), .1)
#'
#'
#' @param cashflows total incoming cash values in vector form for all compounding/payment periods (first element in vector is in present time)
#' @param interest_rate set rate of return for the compounding periods
#'
#' @return interest_rate for the compounding periods in order to be a "fair trade" (rounded)
#'
#' @import tidyverse
#'
#' @export


IRR <- function(cashflows){
  len = as.numeric(length(cashflows))
  time = 1:len

  cost = cashflows[1]
  cashflows[1] = 0
  dat <- data.frame(time, cashflows)
  interest_rate = 0
  pv = 0
  while(pv>=0){
    pv = sum(dat$cashflows/(1+interest_rate)^(time - 1)) + cost
    interest_rate <- interest_rate + .00002
  }

  return(round(interest_rate, 4))
}

