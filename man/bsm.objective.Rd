\name{bsm.objective}
\alias{bsm.objective}
\title{BSM Objective}
\description{
  \code{bsm.objective} is the objective function to be minimized in \code{bsm.extraction}.
}
\usage{
bsm.objective(s0, r, te, y, market.calls, call.strikes, market.puts, 
              put.strikes, lambda = 1, theta)
}
\arguments{
  \item{s0}{current asset value}
  \item{r}{risk free rate}
  \item{te}{time to expiration}
  \item{y}{dividend yield}
  \item{market.calls}{market calls (most expensive to cheapest)}
  \item{call.strikes}{strikes for the calls (smallest to largest)}
  \item{market.puts}{market calls (cheapest to most expensive)}
  \item{put.strikes}{strikes for the puts (smallest to largest)}
  \item{lambda}{Penalty parameter to enforce the martingale condition}
  \item{theta}{initial values for the optimization}
}
\details{
Put details here
}
\value{
  \item{obj }{Value of the objective function}
}
\references{
  E. Jondeau and S. Poon and M. Rockinger (2007):
  \emph{Financial Modeling Under Non-Gaussian Distributions} 
}
\author{Kam Hamidieh}
\note{Put further notes here...}
\seealso{Put other things to see here...}
\examples{
r     = 0.05
te    = 60/365
s0    = 1000
sigma = 0.25
y     = 0.01

call.strikes   = seq(from = 500, to = 1500, by = 25)
market.calls   = bsm.option.price(r =r, te = te, s0 = s0, 
                 k = call.strikes, sigma = sigma, y = y)$call

put.strikes    = seq(from = 510, to = 1500, by = 25)
market.puts    = bsm.option.price(r =r, te = te, s0 = s0, 
                 k = put.strikes, sigma = sigma, y = y)$put

###
### perfect initial values under BSM framework
###

mu.0     = log(s0) + ( r - y - 0.5 * sigma^2) * te
zeta.0   = sigma * sqrt(te)
mu.0
zeta.0

###
### The objective function should be *very* small
###

bsm.obj.val = bsm.objective(theta=c(mu.0, zeta.0), r = r, y=y, te = te, s0 = s0, 
              market.calls = market.calls, call.strikes = call.strikes, 
              market.puts  = market.puts,  put.strikes = put.strikes, lambda = 1)
bsm.obj.val
}
\keyword{ Put a keyword here... }
\keyword{ Put a keyword here... }
