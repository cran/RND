\name{gb.option.price}
\alias{gb.option.price}
\title{Generalized Beta Option Pricing}
\description{
  \code{gb.option.price} computes the price of European options.
}
\usage{
gb.option.price(r, te, s0, k, y, a, b, v, w)
}
\arguments{
  \item{r}{risk free interest rate}
  \item{te}{time to expiration}
  \item{s0}{current asset value}
  \item{k}{strike}
  \item{y}{dividend yield}
  \item{a}{power parameter > 0}
  \item{b}{scale paramter > 0}
  \item{v}{first beta paramter > 0}
  \item{w}{second beta parameter > 0}
}
\details{Put details here...}
\value{
  \item{prob.1 }{Probability of GB(a,b,v+1/a,w-1/a) rv will be above k, strike}
  \item{prob.2 }{Probability of GB(a,b,v,w) rv will be above k, strike}
  \item{call }{call price}
  \item{put }{put price}
}
\references{

  R.M. Bookstaber and J.B. McDonald (1987)
  A general distribution for describing security price returns.
  \emph{Journal of Business}, 60, 401-424 

  X. Liu and M.B. Shackleton and S.J. Taylor and X. Xu (2007)
  Closed-form transformations from risk-neutral to real-world distributions
  \emph{Journal of Business}, 60, 401-424 

  E. Jondeau and S. Poon and M. Rockinger (2007):
  \emph{Financial Modeling Under Non-Gaussian Distributions} }

\author{Kam Hamidieh}
\note{Put further notes here}
\seealso{Put see also here}
\examples{

###
### A basic GB option pricing....
###

r  = 0.03
te = 50/365
s0 = 1000.086
k  = seq(from = 800, to = 1200, by = 10)
y  = 0.01
a  = 10
b  = 1000
v  = 2.85
w  = 2.85

gb.option.price(r = r, te = te, s0 = s0, k = k, y = y, a = a, b = b, v = v, w = w)


}
\keyword{ Put a keyword here... }
\keyword{ Put a keyword here... }
