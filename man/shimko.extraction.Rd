\name{shimko.extraction}
\alias{shimko.extraction}
\title{Shimko Extraction}
\description{
  \code{shimko.extraction} extracts the implied volatility 
        quadratic function parameters based on Shimko's.
}
\usage{
shimko.extraction(market.calls, call.strikes, r, y, te, s0, lower, upper)
}
\arguments{
  \item{market.calls}{market calls (most expensive to cheapest)}
  \item{call.strikes}{strikes for the calls (smallest to largest)}
  \item{r}{risk free rate}
  \item{y}{dividend yield}
  \item{te}{time to expiration}
  \item{s0}{current asset value}
  \item{lower}{lower bound for the search of implied volatility}
  \item{upper}{upper bound for the search of implied volatility}
}
\details{Add more detail}
\value{
  \item{implied.curve.obj }{variable that holds a0, a1, and a2 which are the constant terms of the quadratic polynomial}
  \item{shimko.density }{density evaluated at the strikes}
  \item{implied.volatilities }{implied volatilities at each \code{call.strike}}
}
\references{
  D. Shimko (1993)
  Bounds of probability.
  \emph{Risk}, 6, 33-47

  E. Jondeau and S. Poon and M. Rockinger (2007):
  \emph{Financial Modeling Under Non-Gaussian Distributions} 
}
\author{Kam Hamidieh}
\note{Put additional notes here...}
\section{Warning }{Shimko's paper has an error (typo?)...the density derived lacks a negative sign!}
\seealso{See additional details here}
\examples{

###
###  Test the function shimko.extraction.  If BSM holds then a1 = a2 = 0. 
###

r       =  0.05
y       =  0.02
te      =  60/365
s0      =  1000
k       =  seq(from = 800, to = 1200, by = 5)
sigma   =  0.25

bsm.calls = bsm.option.price(r = r, te = te, s0 = s0, k = k, 
                             sigma = sigma, y = y)$call
shimko.extraction(market.calls = bsm.calls, call.strikes = k, r = r, y = y, te = te, 
                  s0 = s0, lower = -10, upper = 10)

###
### Note a0 is about equal to sigma, and a1 and a2 are close to zero.
###

}
\keyword{ Put a keyword here... }
\keyword{ Put a keyword here... }