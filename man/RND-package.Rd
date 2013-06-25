\name{RND-package}
\alias{RND-package}
\alias{RND}
\docType{package}
\title{
RND
}
\description{
This package provides functions to extracts the risk neutral density from options.
}
\details{
\tabular{ll}{
Package: \tab RND\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-06-25\cr
License: \tab GPL (>= 2)\cr
}
The function \code{MOE} will probably be the most useful.
}
\author{Kam Hamidieh}
\references{
  E. Jondeau and S. Poon and M. Rockinger (2007):
  \emph{Financial Modeling Under Non-Gaussian Distributions} 
}
\keyword{ package }
\examples{

###
### Recover the lognormal density based on BSM
###

r     = 0.05
te    = 60/365
s0    = 1000
k     = seq(from = 500, to = 1500, by = 1)
sigma = 0.25
y     = 0.01

bsm.calls = bsm.option.price(r =r, te = te, s0 = s0, k = k, sigma = sigma, y = y)$call
density.est = get.point.estimate(market.calls = bsm.calls, 
              call.strikes = k, r = r , te = te)

len = length(k)-1
### Note, estimates at two data points (smallest and largest strikes) are lost
plot(density.est ~ k[2:len], type = "l")  

}
