\name{ew.objective}
\alias{ew.objective}
\title{Edgeworth Objective}
\description{
  \code{ew.objective} is the objective function to be minimized in \code{ew.extraction}.
}
\usage{
ew.objective(theta, r, y, te, s0, market.calls, call.strikes, lambda = 1)
}
\arguments{
  \item{theta}{initial values for the optimization}
  \item{r}{risk free rate}
  \item{y}{dividend yield}
  \item{te}{time to expiration}
  \item{s0}{current asset value}
  \item{market.calls}{market calls (most expensive to cheapest)}
  \item{call.strikes}{strikes for the calls (smallest to largest)}
  \item{lambda}{Penalty parameter to enforce the martingale condition}
}
\details{Put additional details here...}
\value{
  \item{obj }{Value of the objective function}
}
\references{
  E. Jondeau and S. Poon and M. Rockinger (2007):
  \emph{Financial Modeling Under Non-Gaussian Distributions} 

  R. Jarrow and A. Rudd (1982)
  Approximate valuation for arbitrary stochastic processes.
  \emph{Journal of Finanical Economics}, 10, 347-369


  C.J. Corrado and T. Su (1996)
  S&P 500 index option tests of Jarrow and Rudd's approximate option valuation formula.
  \emph{Journal of Futures Markets}, 6, 611-629}
\author{Kam Hamidieh}
\note{Put notes here}
\seealso{Put other things to see here...}
\examples{

r       = 0.05
y       = 0.03
s0      = 1000
sigma   = 0.25
te      = 100/365
k       = seq(from=800, to = 1200, by = 50)
v       = sqrt(exp(sigma^2 * te) - 1)
ln.skew = 3 * v + v^3
ln.kurt = 16 * v^2 + 15 * v^4 + 6 * v^6 + v^8

###
### The objective function should be close to zero!
###

market.calls.bsm = bsm.option.price(r = r, te = te, s0 = s0, k=k, 
                   sigma=sigma, y=y)$call
ew.objective(theta = c(sigma, ln.skew, ln.kurt), r = r, y = y, te = te, s0=s0, 
             market.calls = market.calls.bsm, call.strikes = k, lambda = 1)


}
\keyword{ Put a keyword here... }
\keyword{ Put a keyword here... }
