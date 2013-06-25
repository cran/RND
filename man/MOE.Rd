\name{MOE}
\alias{MOE}
\title{Mother of All Extractions}
\description{
  \code{MOE} function extracts the risk neutral density based on all models and summarizes the results.  
}
\usage{
MOE(market.calls, call.strikes, market.puts, 
    put.strikes, s0, r, te, y, file.name )
}
\arguments{
  \item{market.calls}{market calls (most expensive to cheapest)}
  \item{call.strikes}{strikes for the calls (smallest to largest)}
  \item{market.puts}{market calls (cheapest to most expensive)}
  \item{put.strikes}{strikes for the puts (smallest to largest) - SAME AS call.strikes}
  \item{s0}{current asset value}
  \item{r}{risk free rate}
  \item{te}{time to expiration}
  \item{y}{dividend yield}
  \item{file.name}{your default file name}
}
\details{Put addition details here...}
\value{
  \item{bsm.mu}{mean of log(S(T)), when S(T) is lognormal}
  \item{bsm.sigma}{SD of log(S(T)), when S(T) is lognormal}
  \item{gb.a}{extracted power parameter, when S(T) is assumed to be a GB rv}
  \item{gb.b}{extracted scale paramter, when S(T) is assumed to be a GB rv}
  \item{gb.v}{extracted first beta paramter, when S(T) is assumed to be a GB rv}
  \item{gb.w}{extracted second beta parameter, when S(T) is assumed to be a GB rv}
  \item{mln.alpha.1}{extracted proportion of the first lognormal.  Second one is 1 - \code{alpha.1} in mixture of lognormals}
  \item{mln.meanlog.1}{extracted mean of the log of the first lognormal in mixture of lognormals}
  \item{mln.meanlog.2}{extracted mean of the log of the second lognormal in mixture of lognormals}
  \item{mln.sdlog.1}{extracted standard deviation of the log of the first lognormal in mixture of lognormals}
  \item{mln.sdlog.2}{extracted standard deviation of the log of the second lognormal in mixture of lognormals}
  \item{ew.sigma }{volatility when using the Edgeworth expansions}
  \item{ew.skew }{normalized skewness when using the Edgeworth expansions}
  \item{ew.kurt }{normalized kurtosis when using the Edgeworth expansions}
  \item{a0}{extracted constant term in the quadratic ploynomial of Shimko method}
  \item{a1}{extracted coefficient term of k in the quadratic ploynomial of Shimko method}
  \item{a2}{extracted coefficient term of k squared in the quadratic ploynomial of Shimko method}
}
\references{

  E. Jondeau and S. Poon and M. Rockinger (2007):
  \emph{Financial Modeling Under Non-Gaussian Distributions}

}
\author{Kam Hamidieh}
\note{You will get a pdf file with all the graphs and diagnostics and a file with predicted puts and calls.}
\section{Warning }{Make sure the call strikes and put strikes are the same.} 
\seealso{Put additional notes here....}
\examples{

###
### You should see that all methods beautifully extract the same density!
###

r     = 0.05
te    = 60/365
s0    = 1000
sigma = 0.25
y     = 0.02

call.strikes.bsm   = seq(from = 500, to = 1500, by = 5)
market.calls.bsm   = bsm.option.price(r =r, te = te, s0 = s0, 
                     k = call.strikes.bsm, sigma = sigma, y = y)$call

put.strikes.bsm    = seq(from = 500, to = 1500, by = 5)
market.puts.bsm    = bsm.option.price(r =r, te = te, s0 = s0, 
                     k = put.strikes.bsm, sigma = sigma, y = y)$put

###
### See where your results will be outputted to...
###

getwd()


###
###  Running this may take 1-2 minutes...
###
###  MOE(market.calls.bsm, call.strikes.bsm, market.puts.bsm, 
###  put.strikes.bsm, s0, r , te, y, "bsm2")
###


}
\keyword{ Put a keyword here... }
\keyword{ Put a keyword here... }
