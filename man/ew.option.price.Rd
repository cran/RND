\name{ew.option.price}
\alias{ew.option.price}
\title{Edgeworth Option Price}
\description{
  \code{ew.option.price} computes the Edgeworth based option price.
}
\usage{
ew.option.price(r, te, s0, k, sigma, y, skew, kurt)
}
\arguments{
  \item{r}{risk free rate}
  \item{te}{time to expiration}
  \item{s0}{current asset value}
  \item{k}{strike}
  \item{sigma}{volatility}
  \item{y}{dividend rate}
  \item{skew}{normalized skewness}
  \item{kurt}{normalized kurtosis}
}
\details{Put additional details here if need...}
\value{
  \item{call }{Edgeworth based call}
  \item{put }{Edgeworth based put}
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
\note{Put notes here}
\seealso{Put other things to see here...}
\examples{


###
### Test the ew.option.price.  The prices must match EXACTLY the BSM prices
###

r       = 0.05
y       = 0.03
s0      = 1000
sigma   = 0.25
te      = 100/365
k       = seq(from=800, to = 1200, by = 50)
v       = sqrt(exp(sigma^2 * te) - 1)
ln.skew = 3 * v + v^3
ln.kurt = 16 * v^2 + 15 * v^4 + 6 * v^6 + v^8

ew.option.prices  =  ew.option.price(r = r, te = te, s0 = s0, k=k, sigma=sigma, 
                                     y=y, skew = ln.skew, kurt = ln.kurt)
bsm.option.prices =  bsm.option.price(r = r, te = te, s0 = s0, k=k, sigma=sigma, y=y)

ew.option.prices
bsm.option.prices

###
### Now ew prices should now be different as we increase skewness and kurtosis
###

new.skew = ln.skew * 1.10
new.kurt = ln.kurt * 1.10

new.ew.option.prices  =  ew.option.price(r = r, te = te, s0 = s0, k=k, sigma=sigma, 
                                         y=y, skew = new.skew, kurt = new.kurt)
new.ew.option.prices
bsm.option.prices


}
\keyword{ Put a keyword here... }
\keyword{ Put a keyword here... }
