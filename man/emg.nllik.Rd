\name{emg.nllik}
\alias{emg.nllik}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Negative Log Likelihood for EMG }
\description{
  Negative log likelihood function for EMG
}
\usage{
  emg.nllik(x, mu, sigma, lambda)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector of observations}
  \item{mu}{mu of normal}
  \item{sigma}{sigma of normal}
  \item{lambda}{lambda of exponential}
}
\value{
  A single real value of the negative log likelihood that the given parameters explain the observations.
}
\author{ Shawn Garbett }

\seealso{ \code{\link[emg]{emg.mle}} }
\examples{
  y <- remg(200)
  emg.nllik(y, 0, 1, 1)
}
