\name{mln.objective}
\alias{mln.objective}
\title{Mixture of Lognormal Objective}
\description{
   \code{mln.objective} is the objective function to be minimized in \code{mln.extraction}.
}
\usage{
mln.objective(theta, r, y, te, s0, market.calls, 
         call.strikes, market.puts, put.strikes, lambda = 1)
}
\arguments{
  \item{theta}{initial values for the optimization}
  \item{r}{risk free rate}
  \item{y}{dividend yield}
  \item{te}{time to expiration}
  \item{s0}{current asset value}
  \item{market.calls}{market calls (most expensive to cheapest)}
  \item{call.strikes}{strikes for the calls (smallest to largest)}
  \item{market.puts}{market calls (cheapest to most expensive)}
  \item{put.strikes}{strikes for the puts (smallest to largest)}
  \item{lambda}{Penalty parameter to enforce the martingale condition}
}
\details{
  mln is the density f(x) = alpha.1 * g(x) + (1 - alpha.1) * h(x), 
  where g and h are densities of two lognormals.
}
\value{
  \item{obj }{Value of the objective function}
}
\references{

  F. Gianluca and A. Roncoroni (2008)
  \emph{Implementing Models in Quantitative Finance: Methods and Cases}

  B. Bahra (1996):
  Probability distribution of future asset prices implied by option prices.
  \emph{Bank of England Quarterly Bulletin}, August 1996, 299-311

  P. Soderlind and L.E.O. Svensson (1997)
  New techniques to extract market expectations from financial instruments.
  \emph{Journal of Monetary Economics}, 40, 383-429

  E. Jondeau and S. Poon and M. Rockinger (2007):
  \emph{Financial Modeling Under Non-Gaussian Distributions}

}
\author{Kam Hamidieh}
\note{Add more notes here...}
\seealso{Put more here...}
\examples{

###
### The mln objective function should be close to zero...
###

r  = 0.05
te = 60/365
y  = 0.02
s0 = 982
meanlog.1 = 6.8
meanlog.2 = 6.95
sdlog.1   = 0.065
sdlog.2   = 0.055
alpha.1   = 0.4


call.strikes = seq(from = 800, to = 1200, by = 10)
market.calls = mln.option.price(r=r, y = y, te = te, k = call.strikes, 
               alpha.1 = alpha.1, meanlog.1 = meanlog.1, meanlog.2 = meanlog.2, 
               sdlog.1 = sdlog.1, sdlog.2 = sdlog.2)$call

put.strikes  = seq(from = 805, to = 1200, by = 10)
market.puts  = mln.option.price(r = r, y = y, te = te, k = put.strikes, 
               alpha.1 = alpha.1, meanlog.1 = meanlog.1, meanlog.2 = meanlog.2, 
               sdlog.1 = sdlog.1, sdlog.2 = sdlog.2)$put

mln.objective(theta=c(alpha.1,meanlog.1, meanlog.2 , sdlog.1, sdlog.2), 
               r = r, y = y, te = te, s0 = s0, 
               market.calls = market.calls, call.strikes = call.strikes, 
               market.puts = market.puts, put.strikes = put.strikes, lambda = 1)

}
\keyword{ Put a keyword here... }
\keyword{ Put a keyword here... }