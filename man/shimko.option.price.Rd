\name{shimko.option.price}
\alias{shimko.option.price}
\title{Shimko Option Price}
\description{
  \code{shimko.option.price} prices a European option based on the extracted Shimko volatility function.
}
\usage{
shimko.option.price(r, te, s0, k, y, a0, a1, a2)
}
\arguments{
  \item{r}{risk free rate}
  \item{te}{time to expiration}
  \item{s0}{current asset value}
  \item{k}{strike}
  \item{y}{dividend yield}
  \item{a0}{constant term in the quadratic ploynomial}
  \item{a1}{coefficient term of k in the quadratic ploynomial}
  \item{a2}{coefficient term of k squared in the quadratic ploynomial}
}
\details{Put further notes here...}
\value{
  \item{call }{call price}
  \item{put }{put price}
}
\references{

  D. Shimko (1993)
  Bounds of probability.
  \emph{Risk}, 6, 33-47

  E. Jondeau and S. Poon and M. Rockinger (2007):
  \emph{Financial Modeling Under Non-Gaussian Distributions} 
}
\author{Kam Hamidieh}
\note{Put further notes here...}
\seealso{Put see also here...}
\examples{

r       =  0.05
y       =  0.02
te      =  60/365
s0      =  1000
k       =  950
sigma   =  0.25
a0      =  0.30
a1      =  -0.00387
a2      =  0.00000445

###
###  Note how Shimko price is the same when a0 = sigma but substantially 
###  more when a0, a1, a2
###  are changed so the implied volatilies are very high!
###

bsm.option.price(r = r, te = te, s0 = s0, k = k, sigma = sigma, y = y)$call
shimko.option.price(r = r, te = te, s0 = s0, k = k, y = y, 
                    a0 = sigma, a1 = 0, a2 = 0)$call
shimko.option.price(r = r, te = te, s0 = s0, k = k, y = y, 
                    a0 = a0, a1 = a1, a2 = a2)$call

}
\keyword{ Put a keyword here... }
\keyword{ Put a keyword here... }