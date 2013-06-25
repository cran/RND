\name{bsm.option.price}
\alias{bsm.option.price}
\title{BSM Option Pricing}
\description{
  \code{bsm.option.price} computes the BSM European option prices.
}
\usage{
bsm.option.price(s0, k, r, te, sigma, y)
}
\arguments{
  \item{s0}{current asset value}
  \item{k}{strike}
  \item{r}{risk free rate}
  \item{te}{time to expiration}
  \item{sigma}{volatility}
  \item{y}{dividend yield}
}
\details{
Put description here...
}
\value{
  \item{d1 }{value of (log(s0/k) + (r - y + (sigma^2)/2) * te)/(sigma * sqrt(te))}
  \item{d2 }{value of d1 - sigma * sqrt(te)}
  \item{call }{call price}
  \item{put }{put price}
}
\references{
  J. Hull (2011)
  \emph{Options, Futures, and Other Derivatives}
}
\author{Kam Hamidieh}
\note{Put further notes here....}
\seealso{Put other things to see here...}
\examples{

###
### call should be 4.76, put should be 0.81, from Hull 8th, page 315, 316
###

r     = 0.10
te    = 0.50
s0    = 42
k     = 40
sigma = 0.20
y     = 0

bsm.option = bsm.option.price(r =r, te = te, s0 = s0, k = k, sigma = sigma, y = y)
bsm.option

###
### Make sure put-call parity holds, Hull 8th, page 351
###

(bsm.option$call - bsm.option$put) - (s0 * exp(-y*te) - k * exp(-r*te))

}
\keyword{ Put a keyword here... }
\keyword{ Put a keyword here... }
