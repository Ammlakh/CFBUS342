#' @title Big Ugly for Present Value Calculator
#'
#' @description Calculates the present value using a formula that Professor Gorman calls the Big Ugly.
#'
#' @usage BIG_UGLY_PV(1000, 30, .06, .03)
#' @usage BIG_UGLY_PV(1000, 30, .06, .03, annuity = T)
#'
#' @param payment st payment given at each compounding period
#' @param compounding_period number of compounding periods in a year
#' @param interest_rate rate or return for the compounding period
#' @param growth_rate inflation rate (reducing the value of the perpetuity)
#' @param annuity this indicates if the first payment is due immediately
#'
#' @return the present value
#'
#' @import tidyverse
#'
#' @export


BIG_UGLY_PV <- function(payment, compounding_period, interest_rate, growth_rate, annuity=F){

  pv = (payment/(interest_rate-growth_rate))*(1-((1+growth_rate)/(1+interest_rate))^compounding_period)

  if(annuity==T){
    pv_annuity = pv*(1+interest_rate)
    return(pv_annuity)
  }

  return(pv)

}


#' @title Big Ugly for Present Value Calculator
#'
#' @description Calculates the set payment needed using a formula that Professor Gorman calls the Big Ugly.
#'
#' @usage BIG_UGLY_PAYMENT(19246.3, 30, .06, .03)
#' @usage BIG_UGLY_PAYMENT(20401.08, 30, .06, .03, annuity = T)
#'
#'
#' @param payment st payment given at each compounding period
#' @param compounding_period number of compounding periods in a year
#' @param interest_rate rate of return for the compounding period
#' @param growth_rate inflation rate (reducing the value of the perpetuity)
#' @param annuity this indicates if the first payment is due immediately
#'
#' @return the set payment
#'
#' @import tidyverse
#'
#' @export


BIG_UGLY_PAYMENT <- function(PV, compound_period, interest_rate, growth_rate, annuity=F){
  payment = (PV*(interest_rate-growth_rate))/(1-((1+growth_rate)/(1+interest_rate))^compound_period)

  if(annuity==T){
    payment_annuity = payment/(1+interest_rate)
    return(payment_annuity)
  }

  return(payment)

}
