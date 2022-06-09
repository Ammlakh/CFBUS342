#' @title Perpetuity Present Value Calculator
#'
#' @description Calculates the Present Value of a given perpetuity with an interest and growth rate
#'
#' @usage PERPETIUTY_PV(1, .1, .08)
#'
#'
#' @param payment Numerical value of the set payment forever
#' @param discount_rate rate of return for the compounding period
#' @param growth_rate inflation rate (reducing the value of the perpetuity)
#'
#' @return the present Value of the whole perpetuity
#'
#' @import tidyverse
#'
#' @export


PERPETIUTY_PV <- function(payment, discount_rate, growth_rate){
  if(discount_rate == growth_rate){
    return("ERROR: discount_rate cannot equal growth_rate")
  }
  pv = payment/(discount_rate - growth_rate)

  return(pv)

}

#' @title Perpetuity Interest Rate Calculator
#'
#' @description Calculates the interest rate of a given perpetuity with the present value and growth rate
#'
#' @usage PERPETIUTY_R(1, 50, .08)
#'
#'
#' @param payment Numerical value of the set payment forever
#' @param PV the present value of the perpetuity
#' @param growth_rate inflation rate (reducing the value of the perpetuity)
#'
#' @return the discount rate of the whole perpetuity
#'
#' @import tidyverse
#'
#' @export


PERPETIUTY_R <- function(payment, PV, growth_rate){

  interest_rate = payment/PV + growth_rate

  return(interest_rate)

}
