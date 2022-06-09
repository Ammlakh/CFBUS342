#' @title Present Value Calculator
#'
#' @description Calculates the present Value of a given future value with an interest rate
#'
#' @usage PV(10, 5, .1)
#'
#'
#' @param fv Numerical value of a single good in a future time (typically in $)
#' @param periods amount of compounding intervals between the present to the future (typically in years)
#' @param interest_rate rate or return for the compounding period (typically in $)
#'
#' @return the present Value of the single good
#'
#' @import tidyverse
#'
#' @export

PV <- function(fv, periods, interest_rate){

  pv=fv/((1+interest_rate)^periods)

  return(pv)

}

#' @title Future Value Calculator
#'
#' @description Calculates the Future Value of a given present value with an interest rate
#'
#' @usage FV(6.209213, 5, .1)
#'
#'
#' @param fv Numerical value of a single good in the present (typically in $)
#' @param periods amount of compounding intervals between the present to the future (typically in years)
#' @param interest_rate rate of return for the compounding period
#'
#' @return the future Value of the single good
#'
#' @import tidyverse
#'
#' @export



FV <- function(pv, periods, interest_rate){

  fv=pv*((1+interest_rate)^periods)

  return(fv)

}
