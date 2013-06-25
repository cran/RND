\name{bsm.extraction}
\alias{bsm.extraction}
\title{BSM Extraction}
\description{
  \code{bsm.extraction} extracts the parameters of the lognormal density as implied by the BSM model.
}
\usage{
bsm.extraction(initial.values = c(NA, NA), r, y, te, s0, market.calls, 
  call.strikes, market.puts, put.strikes, lambda = 1, hessian.flag = F)
}

\arguments{
  \item{initial.values}{initial values for the optimization}
  \item{r}{risk free rate}
  \item{y}{dividend yield}
  \item{te}{time to expiration}
  \item{s0}{current asset value}
  \item{market.calls}{market calls (most expensive to cheapest)}
  \item{call.strikes}{strikes for the calls (smallest to largest)}
  \item{market.puts}{market calls (cheapest to most expensive)}
  \item{put.strikes}{strikes for the puts (smallest to largest)}
  \item{lambda}{Penalty parameter to enforce the martingale condition}
  \item{hessian.flag}{if F, no hessian is produced}
}
\details{
Put description here...
}
\value{
  Let ST with the lognormal random variable of the risk neutral density.

  \item{mu}{mean of log(ST)}
  \item{zeta }{sd of log(ST)}
  \item{converge.result }{Did the result converge?}
  \item{hessian}{ Hessian matrix}
}
\references{
  E. Jondeau and S. Poon and M. Rockinger (2007):
  \emph{Financial Modeling Under Non-Gaussian Distributions} 
}
\author{Kam Hamidieh}
\note{Put further notes here....}
\seealso{Put other things to see here...}
\examples{
##
## Create some BSM Based options
##

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
###  Get extract the parameter of the density
###

bsm.extraction(r = r, y = y, te = te, s0 = s0, market.calls = market.calls, 
               call.strikes = call.strikes,  market.puts = market.puts, 
               put.strikes = put.strikes, lambda = 1, hessian.flag = FALSE)

###
### The extracted parameters should be close to these actual values:
###
actual.mu     = log(s0) + ( r - y - 0.5 * sigma^2) * te
actual.zeta   = sigma * sqrt(te)
actual.mu 
actual.zeta
}
\keyword{ Put a keyword here... }
\keyword{ Put a keyword here... }
