\name{dgb}
\alias{dgb}
\title{Density of Generalized Beta}
\description{
  \code{dgb} is the probability density function of generalized beta distribution.
}
\usage{
dgb(x, a, b, v, w)
}
\arguments{
  \item{x}{value at which the denisty is to be evaluated}
  \item{a}{power parameter > 0}
  \item{b}{scale paramter > 0}
  \item{v}{first beta paramter > 0}
  \item{w}{second beta parameter > 0}
}
\details{
  Let B be a beta random variable with parameters v and w.  Then Z = b *(B/(1-B))^(1/a) is a generalized
  beta with parameter with (a,b,v,w).
}
\value{
  \item{out }{density value at x}
}
\references{
  R.M. Bookstaber and J.B. McDonald (1987)
  A general distribution for describing security price returns.
  \emph{Journal of Business}, 60, 401-424 

  X. Liu and M.B. Shackleton and S.J. Taylor and X. Xu (2007)
  Closed-form transformations from risk-neutral to real-world distributions
  \emph{Journal of Business}, 60, 401-424 

  E. Jondeau and S. Poon and M. Rockinger (2007):
  \emph{Financial Modeling Under Non-Gaussian Distributions} 

}
\author{Kam Hamidieh}
\note{Put notes here...}
\seealso{Put see also here...}
\examples{
###
### Just simple plot of the density
###

x = seq(from = 500, to = 1500, length.out = 10000)
a = 10
b = 1000
v = 3
w = 3
dx = dgb(x = x, a = a, b = b, v = v, w = w)
plot(dx ~ x, type="l")

}
\keyword{ Put a keyword here... }
\keyword{ Put a keyword here... }
