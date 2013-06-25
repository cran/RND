\name{fit.implied.volatility.curve}
\alias{fit.implied.volatility.curve}
\title{Fit Implied Volatility Curve}
\description{
  \code{fit.implied.volatility.curve} estimates the coefficients of the quadratic equation for the implied volatilities.
}
\usage{
fit.implied.volatility.curve(x, k)
}
\arguments{
  \item{x}{a set of implied volatilities}
  \item{k}{range of strikes}
}
\details{Put details here...}
\value{
  \item{a0}{constant term in the quadratic ploynomial}
  \item{a1}{coefficient term of k in the quadratic ploynomial}
  \item{a2}{coefficient term of k squared in the quadratic ploynomial}
  \item{summary.obj }{statistical summary of the fit}
}
\references{  

  D. Shimko (1993)
  Bounds of probability.
  \emph{Risk}, 6, 33-47

  J. Hull (2011)
  \emph{Options, Futures, and Other Derivatives}
}
\author{Kam Hamidieh}
\note{Put addition notes here...}
\seealso{Put other things to see here...}
\examples{

###
### Suppose, we see the following implied volatilities and strikes
###

implied.sigma = c(0.11, 0.08, 0.065, 0.06, 0.05)  
strikes       = c(340, 360, 380, 400, 410)
tmp  = fit.implied.volatility.curve(x = implied.sigma, k = strikes)
tmp

strike.range = 340:410
plot(implied.sigma ~ strikes)
lines(strike.range, tmp$a0 + tmp$a1 * strike.range + tmp$a2 * strike.range^2)

}
\keyword{ Put a keyword here... }
\keyword{ Put a keyword here... }
