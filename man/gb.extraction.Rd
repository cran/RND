\name{gb.extraction}
\alias{gb.extraction}
\title{Generalized Beta Extraction}
\description{
  \code{gb.extraction} extracts the generalized beta parameters from market options.
}
\usage{
gb.extraction(initial.values = c(NA, NA, NA, NA), r, te, y, s0, 
              market.calls, call.strikes, 
              market.puts, put.strikes, lambda = 1, hessian.flag = F)
}
\arguments{
   \item{initial.values}{initial values for the optimization}
  \item{r}{risk free rate}
  \item{te}{time to expiration}
  \item{y}{dividend yield}
  \item{s0}{current asset value}
  \item{market.calls}{market calls (most expensive to cheapest)}
  \item{call.strikes}{strikes for the calls (smallest to largest)}
  \item{market.puts}{market calls (cheapest to most expensive)}
  \item{put.strikes}{strikes for the puts (smallest to largest)}
  \item{lambda}{Penalty parameter to enforce the martingale condition}
  \item{hessian.flag}{if F, no hessian is produced}
}
\details{Put additional details if needed.}
\value{
  \item{a}{extracted power parameter}
  \item{b}{extracted scale paramter}
  \item{v}{extracted first beta paramter}
  \item{w}{extracted second beta parameter}
  \item{converge.result }{Did the result converge?}
  \item{hessian}{ Hessian matrix}
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
\note{This function extracts the parameters by assuming that the first 4 moments exist.}
\seealso{Put see also here...}
\examples{

###
### create some GB based calls and puts
###

r  = 0.03
te = 50/365
k  = seq(from = 800, to = 1200, by = 10)
a  = 10
b  = 1000
v  = 2.85
w  = 2.85
y  = 0.01
s0 = exp((y-r)*te) * b * beta(v + 1/a, w - 1/a)/beta(v,w) 
s0

call.strikes = seq(from = 800, to = 1200, by = 10)
market.calls = gb.option.price(r = r, te = te, y = y, s0 = s0, 
                               k = call.strikes, a = a, b = s0, v = v, w = w)$call

put.strikes = seq(from = 805, to = 1200, by = 10)
market.puts = gb.option.price(r = r, te = te, y = y, s0 = s0, 
                               k = put.strikes, a = a, b = s0, v = v, w = w)$put


###
### Extraction...should match the a,b,v,w above. You will also get warning messages.
###

gb.extraction(r=r, te=te, y = y, s0=s0, market.calls = market.calls, 
              call.strikes = call.strikes, market.puts = market.puts, 
              put.strikes = put.strikes, lambda = 1, hessian.flag = FALSE)


}
\keyword{ Put a keyword here... }
\keyword{ Put a keyword here... }
