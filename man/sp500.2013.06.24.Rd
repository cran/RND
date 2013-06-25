\name{sp500.2013.06.24}
\alias{sp500.2013.06.24}
\docType{data}
\title{SP 500 Index Option}
\description{
  S&P 500 Options expiring in August were obtained on June 24, 2013.
}
\usage{data(sp500.2013.06.24)}
\format{
  A data frame with 173 observations on the following 9 variables.
  \describe{
    \item{\code{bid}}{bid price for call options}
    \item{\code{ask}}{ask price for call options}
    \item{\code{vol}}{volume of call options traded}
    \item{\code{openint}}{total number of call option contracts}
    \item{\code{strike}}{strike value}
    \item{\code{bid.1}}{bid price for put options}
    \item{\code{ask.1}}{ask price for put options}
    \item{\code{vol.1}}{volume of put options traded}
    \item{\code{openint.1}}{total number of put option contracts}
  }
}
\details{There were 53 days to expiration.  S&P 500 closed at 1573 on 6/24/2013.}
\source{http://www.cboe.com/DelayedQuote/QuoteTableDownload.aspx}
\references{None.}
\examples{

data(sp500.2013.06.24)

###
### See a few lines of the data set
###

head(sp500.2013.06.24)
tail(sp500.2013.06.24)

}
\keyword{datasets}
