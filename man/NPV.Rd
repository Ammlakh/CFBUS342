% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NPV_IRR.R
\name{NPV}
\alias{NPV}
\title{Net Present Value Calculator}
\usage{
NPV(c(0, 1, 2, 3, 0), .1)
}
\arguments{
\item{cashflows}{total incoming cash values in vector form for all compounding/payment periods (first element in vector is in present time)}

\item{interest_rate}{set rate of return for the compounding periods}
}
\value{
the net present value of the cashflows (the overall value if it was a "fair trade" for the given interest rate)
}
\description{
Calculates the net present value of the cashflows with a given interest rate
}
