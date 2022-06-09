#' @title APR to EAR Calculator
#'
#' @description Calculates the effective return rate from the annual nominal rate of interest and number of compounding periods
#'
#' @usage APR_TO_EAR(.1, 12)
#'
#'
#' @param APR the annual nominal rate of interest
#' @param compounding_period number of compounding periods in a year
#'
#' @return the effective return rate
#'
#' @import tidyverse
#'
#' @export


APR_TO_EAR <- function(APR, compounding_period){

  #APR is the annual nominal rate of interest
  EAR = (1 + APR/compounding_period)^compounding_period - 1

  return(EAR)

}

#' @title EAR to APR Calculator
#'
#' @description Calculates the annual nominal rate of interest from the effective return rate and number of compounding periods
#'
#' @usage APR_TO_EAR(.1, 12)
#'
#'
#' @param EAR the effective return rate and number of compounding periods
#' @param compounding_period number of compounding periods in a year
#'
#' @return the annual nominal rate of interest
#'
#' @import tidyverse
#'
#' @export


EAR_to_APR <- function(EAR, compounding_period){

  #APR is the annual nominal rate of interest
  APR = compounding_period*(((1 + EAR)^(1/compounding_period)) - 1)

  return(APR)

}
