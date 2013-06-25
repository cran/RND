\name{sp500.2013.04.19}
\alias{sp500.2013.04.19}
\docType{data}
\title{SP 500 Index Option}
\description{
  S&P 500 Options expiring in June obtained at the end of business day April 19, 2013.
}
\usage{data(sp500.2013.04.19)}
\format{
  A data frame with 171 observations on the following 19 variables.
  \describe{
    \item{\code{bidsize}}{bid size for call options}
    \item{\code{bid}}{bid price for call options}
    \item{\code{ask}}{ask price for call options}
    \item{\code{asksize}}{ask size for call options}
    \item{\code{chg}}{change in the price}
    \item{\code{impvol}}{implied volatility from calls}
    \item{\code{vol}}{volume of call options traded}
    \item{\code{openint}}{total number of call option contracts}
    \item{\code{delta}}{delta of the call option}
    \item{\code{strike}}{strike value}
    \item{\code{bidsize.1}}{bid size for put options}
    \item{\code{bid.1}}{bid price for put options}
    \item{\code{ask.1}}{ask price for put options}
    \item{\code{asksize.1}}{ask size for put options}
    \item{\code{chg.1}}{change in the price}
    \item{\code{impvol.1}}{implied volatility from calls}
    \item{\code{vol.1}}{volume of put options traded}
    \item{\code{openint.1}}{total number of put option contracts}
    \item{\code{delta.1}}{delta of the put option}
  }
}
\details{There were 62 days to expiration.  S&P 500 closed at 1555 on 4/19/2013.  }
\source{The data were scraped from the Web.}
\references{None.}
\examples{

data(sp500.2013.04.19)

###
### Take a look at a few rows of the data...
###

head(sp500.2013.04.19)
tail(sp500.2013.04.19)


}
\keyword{datasets}
