\name{vix.2013.06.25}
\alias{vix.2013.06.25}
\docType{data}
\title{VIX Index Option}
\description{
    VIX options expiring in August were obtained on June 25, 2013.
}
\usage{data(vix.2013.06.25)}
\format{
  A data frame with 35 observations on the following 13 variables.
  \describe{
    \item{\code{last}}{last quote for call option}
    \item{\code{change}}{change in call option price}
    \item{\code{bid}}{bid price for call options}
    \item{\code{ask}}{ask price for call options}
    \item{\code{vol}}{volume of call options traded}
    \item{\code{openint}}{total number of call option contracts}
    \item{\code{strike}}{strike value}
    \item{\code{last.1}}{last quote for put option}
    \item{\code{change.1}}{change in call option price}
    \item{\code{bid.1}}{bid price for put options}
    \item{\code{ask.1}}{ask price for put options}
    \item{\code{vol.1}}{volume of put options traded}
    \item{\code{openint.1}}{total number of put option contracts}
  }
}
\details{There were 57 days to expiration.  VIX closed at 18.21 on 6/25/2013.}
\source{http://finance.yahoo.com/q?s=^VIX}
\references{None.}
\examples{

data(vix.2013.06.25)

###
### See a few lines of the data set
###

head(vix.2013.06.25)
tail(vix.2013.06.25)

}
\keyword{datasets}
