\name{ew.extraction}
\alias{ew.extraction}
\title{Edgeworth Extraction}
\description{
  \code{ew.extraction} extracts the parameters density approximated by the Edgeworth expansion method.
}
\usage{
ew.extraction(initial.values = c(NA, NA, NA), r, y, te, s0, 
              market.calls, call.strikes, lambda = 1, hessian.flag = F)
}
\arguments{
   \item{initial.values}{initial values for the optimization}
  \item{r}{risk free rate}
  \item{y}{dividend yield}
  \item{te}{time to expiration}
  \item{s0}{current asset value}
  \item{market.calls}{market calls (most expensive to cheapest)}
  \item{call.strikes}{strikes for the calls (smallest to largest)}
  \item{lambda}{Penalty parameter to enforce the martingale condition}
  \item{hessian.flag}{if F, no hessian is produced}
}
\details{Put additional details if needed.}
\value{
  \item{sigma }{volatility of the underlying lognormal}
  \item{skew }{normalized skewness}
  \item{kurt }{normalized kurtosis}
  \item{converge.result }{Did the result converge?}
  \item{hessian}{ Hessian matrix}
}
\references{
  E. Jondeau and S. Poon and M. Rockinger (2007):
  \emph{Financial Modeling Under Non-Gaussian Distributions} 

  R. Jarrow and A. Rudd (1982)
  Approximate valuation for arbitrary stochastic processes.
  \emph{Journal of Finanical Economics}, 10, 347-369


  C.J. Corrado and T. Su (1996)
  S&P 500 index option tests of Jarrow and Rudd's approximate option valuation formula.
  \emph{Journal of Futures Markets}, 6, 611-629
}
\author{Kam Hamidieh}
\note{Put further notes here...}
\section{Warning }{Any?} 
\seealso{Put other things to see here...}
\examples{

###
###  ln.skew & ln.kurt are the normalized skewness and kurtosis of a true lognormal.
###

r       = 0.05
y       = 0.03
s0      = 1000
sigma   = 0.25
te      = 100/365
strikes = seq(from=600, to = 1400, by = 1)
v       = sqrt(exp(sigma^2 * te) - 1)
ln.skew = 3 * v + v^3
ln.kurt = 16 * v^2 + 15 * v^4 + 6 * v^6 + v^8

###
### Now "perturb" the lognormal
###

new.skew = ln.skew * 1.50
new.kurt = ln.kurt * 1.50

###
### new.skew & new.kurt should not be extracted.
###

market.calls      =  ew.option.price(r = r, te = te, s0 = s0, k=strikes, sigma=sigma, 
                     y=y, skew = new.skew, kurt = new.kurt)$call
ew.extracted.obj  =  ew.extraction(r = r, y = y, te = te, s0 = s0, 
                     market.calls = market.calls, call.strikes = strikes, 
                     lambda = 1, hessian.flag = FALSE)
ew.extracted.obj

}
\keyword{ Put a keyword here... }
\keyword{ Put a keyword here... }
